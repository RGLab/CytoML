#' Convert a GatingSet to gatingML
#' 
#' this function retrieves the gates from GatingSet 
#' and writes a customed GatingML-2.0 file  
#' which can then be imported into cytobank  
#' operation:
#' 1. Read in gate geometry, compensation and transformation from gatingSet
#' 2. Rescale gate boundaries with flowJoTrans() so gates show up in flowJo
#' 3. Save gates and hierarchy structure to R environment
#' 4. Write environment out to gatingML using write.GatingML()
#' @importFrom  flowUtils write.gatingML
#' @importFrom XML saveXML xmlTreeParse xmlRoot
#' @export
#' @examples 
#' \dontrun{
#' library(CytoML)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' outFile <- tempfile(fileext = ".xml")
#' GatingSet2GatingML(gs, outFile)
#' }
GatingSet2GatingML <- function(gs, outFile, showHidden = FALSE, ...){
  flowEnv <- GatingSet2Environment(gs, showHidden = showHidden, ...) 
  tmp <- tempfile(fileext = ".xml")#ensure correct file extension for xmlTreeParse to work
  flowUtils::write.gatingML(flowEnv, tmp)
  tree <- xmlTreeParse(tmp, trim = FALSE)
  root <- xmlRoot(tree)
  # browser()
  
  root <- addCustomInfo(root, gs, flowEnv, showHidden = showHidden, ...)
  #add pop (GateSet/BooleanAndGate)
  root <- addGateSets(root, gs, flowEnv[["guid_mapping"]], showHidden = showHidden)  
  #add experiment info to custom node
  root <- addExperimentInfo(root)
  saveXML(root, file = outFile)
}


#' extract the trans, comps and gates into GML2 compatible format
#' and save to environment
GatingSet2Environment <- function(gs, cytobank.default.scale = TRUE, showHidden) {
  if(cytobank.default.scale)
    warning("With 'cytobank.default.scale' set to 'TRUE', data and gates will be re-transformed with cytobank's default scaling settings, which may affect how gates look like.")
  flowEnv <- new.env(parent = emptyenv())
  
  #parse comp and channel names
  comp <- gs@compensation
  if(is.null(comp)){
    #(assume it is identical across samples)
    comp.obj <- flowWorkspace:::.cpp_getCompensation(gs@pointer, sampleNames(gs)[1])  
    if(is.null(comp.obj)){
      #no compensation and channel names in transformation are not prefixed
      chnls <- colnames(getData(gs))
    }else{
      #parsed from flowJo and channel names are usually prefixed
      #thus get the raw channel names from here
      chnls <- comp.obj[["parameters"]]
      comp <- compensation(matrix(comp.obj$spillOver
                                  ,nrow=length(chnls)
                                  ,ncol=length(chnls)
                                  ,byrow=TRUE
                                  ,dimnames=list(chnls,chnls)
                                  )
                           )
      
    }
  }else{
    #compensation was added in R
    #channel names are not prefixed
    chnls <- as.vector(parameters(comp))
    
  }
  
  #add comp
  if(!is.null(comp)){
    #prefix the channels since cytobank expect that
    prefix_chnls <- paste0("Comp_", chnls)
    rownames(comp@spillover) <- prefix_chnls #change fluorochromes and leave detector(colnames) unchanged
    comp <- compensation(comp@spillover)
    
    compId <- identifier(comp)
    compId <- paste("Spill", compId, sep = "_")
    identifier(comp) <- compId
    
    flowEnv[[compId]] <- comp
  }else
    prefix_chnls <- chnls
  
  
  
  #add trans (assume it is identical across samples)
  trans <- getTransformations(gs[[1]], only.function = FALSE)
  
  if(length(trans) == 0)
    stop("no transformation is found in GatingSet!")
  trans.Gm2objs <- list()
  rescale.gate <- FALSE
  for(transName in names(trans)){
    trans.obj <- trans[[transName]]
    type <- trans.obj[["name"]]
    ind <- sapply(chnls, grepl, x = transName, USE.NAMES = FALSE)
    nMatched <- sum(ind)
    if(nMatched > 1)
      stop("More than one channels matched to transformation: ", transName)
    else if(nMatched == 1){
      trans.func <- trans.obj[["transform"]]
      chnl <- chnls[ind]
      prefix_chnl <- prefix_chnls[ind]
      param.obj <- compensatedParameter(prefix_chnl
                           , spillRefId = compId
                           , searchEnv = flowEnv
                           , transformationId = prefix_chnl)
      
      if(type == "asinhtGml2"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_Arcsinh_", prefix_chnl)
        flowEnv[[transID]] <- asinhtGml2(parameters = param.obj
                                     , M = env[["m"]]
                                     , T = env[["t"]]
                                     , A = env[["a"]]
                                     , transformationId = transID
                                    )
      }else if(type == "logicleGml2"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_logicleGml2_", prefix_chnl)
        flowEnv[[transID]] <- logicletGml2(parameters = param.obj
                                     , M = env[["M"]]
                                     , T = env[["T"]]
                                     , A = env[["A"]]
                                     , W = env[["W"]]
                                     , transformationId = transID
                                    )
      }else if(type == "logicle"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_logicle_", prefix_chnl)
        flowEnv[[transID]] <- logicletGml2(parameters = param.obj
                                        , M = env[["m"]]
                                        , T = env[["t"]]
                                        , A = env[["a"]]
                                        , W = env[["w"]]
                                        , transformationId = transID
                                        )
        rescale.gate <- TRUE  
      }else if(type %in% c("flowJo_biexp", "flowJo_fasinh")){
        transID <- paste0("Tr_Arcsinh_", prefix_chnl)
        #use asinhtGml2 with cytobank default setting
        flowEnv[[transID]] <- asinhtGml2(parameters = param.obj
                                         , M = 0.43429448190325176
                                         , T = 176.2801790465702
                                         , A = 0.0
                                         , transformationId = transID
                                        )
        rescale.gate <- TRUE  
      }else{
        # browser()
        stop("unsupported trans: ", type)
      }
        
      
    }
    
    if(cytobank.default.scale){
      rescale.gate <- TRUE 
      #overwrite the customed scale with default one 
      flowEnv[[transID]] <- asinhtGml2(parameters = param.obj
                                       , M = 0.43429448190325176
                                       , T = 176.2801790465702
                                       , A = 0.0
                                       , transformationId = transID)
    }
      
    #save another copy of trans.obj in the list
    trans.Gm2objs[[chnl]] <- flowEnv[[transID]]
  }
  
  #add gates and pops(as GateSets)
  nodePaths <- getNodes(gs, showHidden = showHidden)[-1]
  fcs_names <- pData(gs)[["name"]]
  rng <- range(getData(gs[[1]], use.exprs = FALSE))
  for(gate_id in seq_along(nodePaths)){
    nodePath <- nodePaths[gate_id]
    gates <- getGate(gs, nodePath)
    popName <- basename(nodePath)
    for(fcs_id in seq_along(fcs_names)){
      sn <- fcs_names[fcs_id]
      gate <- gates[[sn]]
# browser()      
      #cytobank does not support negated gate
      #we have to create inverse gate on our end
      if(flowWorkspace:::isNegated(gs[[sn]], nodePath)){
        gate <- inverse(gate, rng)
      }
      #transform to raw scale
      #and attach comp and trans reference to parameters
      gate <- processGate(gate, trans.Gm2objs, compId, flowEnv, rescale.gate, trans)
    
      parent <- getParent(gs, nodePath)
      if(parent == "root")
        parent_id <- 0
      else
        parent_id <- match(parent, nodePaths)
      
      guid <- paste("gate", gate_id, fcs_id, sep = "_")
      identifier(gate) <- guid
      #add gate
      flowEnv[[guid]] <- gate
      
    }
  }
  flowEnv
}

#' @importFrom base64enc base64encode base64decode
base64encode_cytobank <- function(x){
  x <- base64encode(charToRaw(x))
  x <- gsub("=", ".", x)
  x <- gsub("\\+", "_", x)
  x <- gsub("/", "-", x) 
  x
}
base64decode_cytobank <- function(x){
  x <- gsub("\\.", "=", x)
  x <- gsub("_", "\\+", x)
  x <- gsub("-", "/", x)
  base64decode(x)
}

setMethod("transform", signature = c("polygonGate"), function(`_data`, ...){
  .transform.polygonGate(`_data`, ...)
})
.transform.polygonGate <- function(gate, trans.fun, param){
  gate@boundaries[, param] <- trans.fun(gate@boundaries[, param])
  gate
}

setMethod("transform", signature = c("ellipsoidGate"), function(`_data`, ...){
  # .transform.ellipsoidGate(`_data`, ...)
  transform(as(`_data`, "polygonGate"), ...)
})
# somehow ellips shape is not well perseved after transforming the two antipods and mean
.transform.ellipsoidGate <- function(gate, trans.fun, param){
  #convert cov format to antipotal format since cov can not be transformed independently on each param
  #it is based on 5.3.1 of gatingML2 doc
  mu <- gate@mean
  CC <- gate@cov
  dims <- colnames(CC)
  x <- dims[1]
  y <- dims[2] 
  D <- gate@distance
  
#   term <- sqrt((CC[x, x] - CC[y, y]) ^ 2 + 4 * CC[x, y] ^ 2)
#   lambda <- ((CC[x, x] + CC[y, y]) + c(term, -term)) / 2
#   
#   if(CC[x,y] == 0){
#     X1 <- c(1, 0)
#     X2 <- c(0, 1)
#   }else{
#     X1 <- c(lambda[1] - CC[y, y], CC[x, y])
#     X2 <- c(lambda[2] - CC[y, y], CC[x, y])  
#   }
  #compute eigen value (for a, b) and eigen vector (for angle)
  res <- eigen(CC)
  lambda <- res[["values"]]
  X1 <- res[["vectors"]][,1]
  if(X1[1] == 0){
    theta <- pi/2
  }else{
    theta <- atan(X1[2]/X1[1])
  }
  
  a <- sqrt(lambda[1] * D ^ 2)
  b <- sqrt(lambda[2] * D ^ 2)
  
  #get coordinates of centred antipodal points
  antipod1 <- c(a * cos(theta), a * sin(theta))
  antipod2 <- c(b * sin(theta), - b * cos(theta))
  # browser()
  #shift to mu
  antipod1 <- antipod1 + mu
  antipod2 <- antipod2 + mu
  names(antipod1) <- dims
  names(antipod2) <- dims
  #transform the respective dim of antipods
  antipod1[param] <- trans.fun(antipod1[param])
  antipod2[param] <- trans.fun(antipod2[param])
  
  # transform to get new mu
  mu[param] <- trans.fun(mu[param])
  #shift to new center
  antipod1 <- antipod1 - mu
  antipod2 <- antipod2 - mu
  #compute the new a, b
  a <- sqrt(sum(antipod1 ^ 2))
  b <- sqrt(sum(antipod2 ^ 2))
  #convert it back to the inverse covaiance mat
  
  CC.inv <- CC
  CC.inv[x, x] <- cos(theta) ^ 2 / a ^ 2 + sin(theta) ^ 2 / b ^ 2
  CC.inv[y, y] <- sin(theta) ^ 2 / a ^ 2 + cos(theta) ^ 2 / b ^ 2
  CC.inv[x, y] <- CC.inv[y, x] <- sin(theta) * cos(theta) * (1/a^2 - 1/b^2)
  CC <- solve(CC.inv)
  
  
  gate1 <- gate
  gate1@cov <- CC
  gate1@mean <- mu
  # browser()
  gate1
  
}

setMethod("transform", signature = c("rectangleGate"), function(`_data`, ...){
  .transform.rectangleGate(`_data`, ...)
})
.transform.rectangleGate <- function(gate, trans.fun, param){
  
  min <- gate@min[[param]]
  if(!is.infinite(min))
    gate@min[[param]] <- trans.fun(min)
  
  max <- gate@max[[param]]
  if(!is.infinite(max))
    gate@max[[param]] <- trans.fun(max)
  
  gate
  
}
#' @importFrom flowCore eval
processGate <- function(gate, gml2.trans, compId, flowEnv, rescale.gate = FALSE, orig.trans){
  
  params <- as.vector(parameters(gate))
  chnls <- names(gml2.trans)
   
  for(i in seq_along(params)){
    param <- params[i]
    ind <- sapply(chnls, function(chnl)grepl(chnl, param), USE.NAMES = FALSE)
    nMatched <- sum(ind)
    if(nMatched == 0){
     
      chnl <- gate@parameters[[i]]@parameters
      gate@parameters[[i]] <- compensatedParameter(chnl
                                                   , spillRefId = "uncompensated"#compId
                                                   , searchEnv = flowEnv
                                                   , transformationId = chnl
                                                   )
    }else if(nMatched == 1){
      chnl <- chnls[ind]
      orig.trans.obj <- orig.trans[[which(ind)]]
      gml2.trans.obj <- gml2.trans[[chnl]]
      if(rescale.gate){
        inv.fun <- orig.trans.obj[["inverse"]] 
        trans.fun <- eval(gml2.trans.obj)
        #rescale
        gate <- transform(gate, inv.fun, param)
        gate <- transform(gate, trans.fun, param)  
      }
      
      
      #attach trans reference
      gate@parameters[[i]] <- gml2.trans.obj
    }else if(nMatched > 1)
      stop("multiple trans matched to :", param)
  }
  
  # round
  gate
  
}

#' @importFrom XML xmlTree
addGateSets <- function(root, gs, showHidden, ...)
{
  
  nodePaths <- getNodes(gs, showHidden = showHidden)[-1]
  # browser()
  newNodes <- lapply(seq_along(nodePaths), function(gate_id){
                      nodePath <- nodePaths[gate_id]
                      curNode <- nodePath
                      pop_name <- basename(nodePath)
                      gate_id_path <- gate_id
                      # browser()
                      repeat{
                        curNode <- getParent(gs, curNode)
                        if(curNode == "root")
                          break
                        else{
                          cur_parent_id <- match(curNode, nodePaths)
                          gate_id_path <- c(cur_parent_id, gate_id_path)
                        }
                          
                      }
                      GateSetNode(gate_id, pop_name, gate_id_path, nodePaths, ...)
                    })
  
  addChildren(root, kids = newNodes)
}

#' @importFrom jsonlite toJSON
#' @importFrom XML xmlNode
GateSetNode <- function(gate_id, pop_name, gate_id_path, nodePaths, guid_mapping){

  attrs = c("gating:id" = paste("GateSet", gate_id, sep = "_"))
  
  definition <- toJSON(list(gates = gate_id_path, negGates = vector()))
  
  #duplicate the refs if it is the root
  ref_gate_id_path <- gate_id_path
  if(length(ref_gate_id_path) == 1)
    ref_gate_id_path <- c(ref_gate_id_path, ref_gate_id_path)
  xmlNode("gating:BooleanGate", attrs = attrs
          , xmlNode("data-type:custom_info"
                    , xmlNode("cytobank"
                              , xmlNode("name", pop_name)
                              , xmlNode("gate_set_id", gate_id)
                              , xmlNode("definition", I(definition))#set AsIs to avoid xml escaping
                              )
                    )
          
         ,  xmlNode("gating:and"
                  #create two dummy reference
                  , .children = lapply(ref_gate_id_path, function(gate_id){
                    
                    guid <- guid_mapping[[gate_id]]
                    attrs = c("gating:ref" = guid)
                    xmlNode("gating:gateReference", attrs = attrs)  
                  })
                )
        )
}

#' add customInfo nodes to each gate node and add BooleanAndGates
#' @importFrom  XML xmlAttrs getNodeSet addChildren xmlAttrs<-
addCustomInfo <- function(root, gs, flowEnv, cytobank.default.scale = TRUE, showHidden){
  nodePaths <- getNodes(gs, showHidden = showHidden)[-1]
  pd <- pData(gs)
  fcs_names <- pd[["name"]]
  fcs_guids <- rownames(pd)
  translist <- getTransformations(gs[[1]], only.function = FALSE)
  transNames <- names(translist)
  rng <- range(getData(gs[[1]], use.exprs = FALSE))
  for(id in 1:length(root)){
    
    curNode <- root[[id]]
    guid <- as.vector(xmlAttrs(curNode, "gating:id"))
    if(!is.null(guid)&&grepl("gate_", guid)){
        #parse pop and fcs info from guid
        fields <- strsplit(guid, "_")[[1]]
        gate_id <- as.integer(fields[[2]])
        fcs_id <- as.integer(fields[[3]])
        
        nodePath <- nodePaths[gate_id]
        pop_name<- basename(nodePath)
        fcs_name <- fcs_names[fcs_id]
        fcs_guid <- fcs_guids[fcs_id]
        # browser()
        
        gate <- flowEnv[[guid]]
        gate_type <- class(gate)
        if(gate_type == "rectangleGate"){
          if(length(parameters(gate)) == 1)
            gate_type <- "RangeGate"
          else
            gate_type <- "RectangleGate"
        }else if(gate_type == "polygonGate")
          gate_type <- "PolygonGate"
        else if(gate_type == "ellipsoidGate")
          gate_type <- "EllipseGate"
        else
          stop("unsupported gate: ", gate_type)
        # browser()
        # message(guid)
        #parse scale info from gate parameter
        scale <- lapply(gate@parameters@.Data, function(param){
          # browser()
          if(class(param) == "compensatedParameter"){
            if(cytobank.default.scale){
             thisRng <- c(1, 262144.0) 
            }else{
                chnl <- as.vector(parameters(param))
                thisRng <- rng[, chnl]    
              }
            
            flag <- 1
            argument <- "1"
          }else if(is(param, "singleParameterTransform")){
            
            chnl <- as.vector(parameters(param@parameters))
            chnl <- sub("(^Comp_)(.*)", "\\2", chnl) #strip the prefix before matching
            ind <- grepl(chnl, names(rng))
            nMatched <- sum(ind)
            if(nMatched == 1){
              if(cytobank.default.scale){
                thisRng <- c(-200, 262144.0)
              }else
                thisRng <- rng[, ind]
            }else
              stop(chnl , " not found in range info")
            if(is(param, "asinhtGml2")){
              flag <- 4
              argument <- as.character(round(param@T/sinh(1)))
            }else if(is(param, "logicletGml2")){
             flag <- 4
             argument <- as.character(round(param@T/sinh(1)))
            }else
              stop("unsupported transform: ", class(param))
            if(!cytobank.default.scale){
              #inverse range into raw scale
              ind <- sapply(transNames, function(transName)grepl(chnl, transName), USE.NAMES = FALSE)
              nMatched <- sum(ind)
              if(nMatched == 1){
                trans.obj <- translist[[which(ind)]]
                trans.fun <- trans.obj[["inverse"]] 
                thisRng <- round(trans.fun(thisRng))#cytobank experiment scale expect 0 digits after decimal
              }else
                stop("can't find the transformation function in GatingSet to inverse the range for :", chnl)  
            }
            
          }else 
            stop("unsupported transform: ", class(param))
          
          thisRng <- round(thisRng, 2)
          list(flag = flag, argument = argument, min = thisRng[1], max = thisRng[2], bins = 256, size = 256)
        })
        if(length(scale) == 1){
          scale <- unlist(scale, recursive = FALSE)
        }else{
          names(scale) <- c("x", "y")
        }
        definition <- list(scale = scale)
        definition <- toJSON(definition, auto_unbox = TRUE)
        #insert custom info
        customNode <- customInfoNodeForGate(id, gate_id, pop_name, fcs_id, fcs_name, gate_type, definition)
        newNode <- addChildren(curNode, kids = list(customNode), at = 0)        
        #modify id
        guid.new <- paste("Gate", id, base64encode_cytobank(pop_name), sep = "_")
        xmlAttrs(newNode, suppressNamespaceWarning = TRUE, append = FALSE) <- c("gating:id" = guid.new)
        #update the tree
        root[[id]] <- newNode  
        
        #record the mapping between gate_id and guid.new for the refs of GateSets
        if(fcs_id == 1)
          flowEnv[["guid_mapping"]][[gate_id]] <- guid.new
    }
  }
  root
  
}

#' @importFrom  XML newXMLNode
customInfoNodeForGate <- function(id, gate_id, pop_name, fcs_id, fcs_name, type, definition)
{
    if(fcs_id == 1){
      fcs_id <- fcs_name <- ""
    }
      
 #avoid using newXMLNode since it is not segfault-free.
  xmlNode("data-type:custom_info"
      , xmlNode("cytobank"
          , xmlNode("name", pop_name)
          , xmlNode("id", id)
          , xmlNode("gate_id", gate_id)
          , xmlNode("type", type)
          , xmlNode("version", 1)
          , xmlNode("fcs_file_id", fcs_id)
          , xmlNode("fcs_file_filename", fcs_name)
          , xmlNode("definition", I(definition))
          )
      )
}

addExperimentInfo <- function(root, experiment_number = ""){
   
   customNode <- root[["custom_info"]]
   customNode <- addChildren(customNode, xmlNode("flowWorkspace-version", packageVersion("flowWorkspace")))
   
   newNode <- xmlNode("cytobank"
                      , xmlNode("experiment_number", experiment_number)
   )
   customNode <- addChildren(customNode, newNode, at = 0)

   root[["custom_info"]] <- customNode
   root            
}

inverse <- function(gate, boundary){
  UseMethod("inverse")
}

inverse.polygonGate <- function(gate, ...){
  gate@boundaries <- inverse.polygon(gate@boundaries, ...)
  gate
}
inverse.ellipsoidGate <- function(gate, ...){
  inverse(as(gate, "polygonGate"), ...)
}
inverse.rectangleGate <- function(gate, ...){
  param <- as.vector(parameters(gate))
  if(length(param) == 1){
    stop("to be implemented!")
  }else
    inverse(as(gate, "polygonGate"), ...)
}

inverse.polygon <- function(mat, boundary){
  
  
  dims <- colnames(mat)
  stopifnot(all(dims %in% colnames(boundary)))
  
  #enlarge the boundary to ensure the negated polygon includes all events outside of original polygon
  boundary["min", ] <- boundary["min", ] - 1e4
  boundary["max", ] <- boundary["max", ] + 1e4
  
  x <- dims[1]
  y <- dims[2]

  #find the top most point as starting point
  ind <- which.max(mat[, y])
  #reorder mat starting from top most
  nRow <- nrow(mat)
  orig.seq <- seq_len(nRow)
  seq1 <- ind:nRow
  seq2 <- setdiff(orig.seq, seq1)
  new.seq <- c(seq1, seq2)
  new.seq
  mat.new <- mat[new.seq, ]
  
  
  mat.inv <- mat.new[1, ]
  #extend to upper bound
  pt1 <- c(mat.new[1, x], boundary["max", y])
  mat.inv <- rbind(mat.inv, pt1, deparse.level = 0)
  #left top
  mat.inv <- rbind(mat.inv, c(boundary["min", x], boundary["max",y]))
  #left bottom
  mat.inv <- rbind(mat.inv, c(boundary["min", x], boundary["min",y]))
  #right bottom
  mat.inv <- rbind(mat.inv, c(boundary["max", x], boundary["min",y]))
  #right top
  mat.inv <- rbind(mat.inv, c(boundary["max", x], boundary["max",y]))
  #back to first extended point
  mat.inv <- rbind(mat.inv, pt1, deparse.level = 0)
  #connect to the remain mat.new in reverse order
  mat.inv <- rbind(mat.inv, mat.new[rev(seq_len(nRow)[-1]), ])
  
#   plot(mat.inv, type = "n")
#   polygon(mat.new, col = "red")
#   polygon(mat.inv, col = "blue")
  mat.inv
}
