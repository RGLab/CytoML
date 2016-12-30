#' Convert a GatingSet to a Cytobank-compatible gatingML
#'
#' this function retrieves the gates from GatingSet and writes a customed GatingML-2.0 file
#' that can be imported into cytobank.
#'
#' The process can be divided into four steps:
#' 1. Read in gate geometry, compensation and transformation from gatingSet
#' 2. Rescale gate boundaries with flowJoTrans() so gates can be displayed properly in Cytobank
#' 3. Save gates and hierarchy structure to R environment
#' 4. Write environment out to gatingML using write.GatingML()
#'
#' @importFrom  flowUtils write.gatingML
#' @importFrom XML saveXML xmlTreeParse xmlRoot
#' @importFrom utils localeToCharset packageVersion
#' @export
#' @return nothing
#' @param gs a GatingSet object
#' @param outFile a file name
#' @param showHidden whether to include the hidden population nodes in the output
#' @param cytobank.default.scale logical flag indicating whether to use the default Cytobank asinhtGml2 settings.
#'                              Currently it should be set to TRUE in order for gates to be displayed properly in Cytobank
#'                              because cytobank currently does not parse the global scale settings from GatingML.
#' @examples
#' library(flowWorkspace)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' Rm("CD8", gs)
#'
#' #output to cytobank
#' outFile <- tempfile(fileext = ".xml")
#' GatingSet2cytobank(gs, outFile) #type by default is 'cytobank'
#'
#'
GatingSet2cytobank <- function(gs, outFile, showHidden = FALSE, cytobank.default.scale = TRUE){

  #convert comp and trans as GML2 compatible format and save to env
  if(cytobank.default.scale)
    warning("With 'cytobank.default.scale' set to 'TRUE', data and gates will be re-transformed with cytobank's default scaling settings, which may affect how gates look like.")

  flowEnv <- new.env(parent = emptyenv())
  res <- export_comp_trans(gs, flowEnv, cytobank.default.scale = cytobank.default.scale, type = "cytobank")
  #convert gates to GML2
  export_gates_cytobank(gs, flowEnv, res[["trans.Gm2objs"]], res[["trans"]], res[["compId"]], showHidden = showHidden)

  tmp <- tempfile(fileext = ".xml")#ensure correct file extension for xmlTreeParse to work
  flowUtils::write.gatingML(flowEnv, tmp)
  tree <- xmlTreeParse(tmp, trim = FALSE)
  root <- xmlRoot(tree)
  # browser()

  root <- addCustomInfo(root, gs, flowEnv, showHidden = showHidden, cytobank.default.scale = cytobank.default.scale)
  #add pop (GateSet/BooleanAndGate)
  root <- addGateSets(root, gs, flowEnv[["guid_mapping"]], showHidden = showHidden)
  #add experiment info to custom node
  root <- addExperimentInfo(root)
  saveXML(root, file = outFile)
}

export_gates_cytobank <- function(gs, flowEnv, trans.Gm2objs, trans, compId, showHidden, rescale.gate = TRUE)
{
  #add gates and pops(as GateSets)
  nodePaths <- getNodes(gs, showHidden = showHidden)[-1]
  fcs_guids <- sampleNames(gs)
  rng <- range(getData(gs[[1]], use.exprs = FALSE))
  for(gate_id in seq_along(nodePaths)){
    nodePath <- nodePaths[gate_id]
    gates <- getGate(gs, nodePath)
    popName <- basename(nodePath)
    for(fcs_id in seq_along(fcs_guids)){
      fcs_guid <- fcs_guids[fcs_id]
      gate <- gates[[fcs_guid]]

      #cytobank does not support negated gate
      #we have to create inverse gate on our end
      if(flowWorkspace:::isNegated(gs[fcs_guid][[1]], nodePath)){
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
#' @inheritParams GatingSet2cytobank
#' @param root the root node of the XML
#' @param flowEnv the environment that stores the information parsed by 'read.GatingML'.
#' @importFrom  XML xmlAttrs getNodeSet addChildren xmlAttrs<-
#' @importFrom flowWorkspace pData
#' @return XML root node
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

