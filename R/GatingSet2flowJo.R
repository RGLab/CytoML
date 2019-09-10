#' Convert a GatingSet to flowJo workspace
#'
#'
#' @param gs a GatingSet object
#' @param outFile the workspace file path to write
#' @param ... other arguments
#'        showHidden whether to include the hidden population nodes in the output
#' @export
#' @importFrom flowWorkspace gs_clone gs_update_channels pData<- cs_unlock cs_lock gs_copy_tree_only cs_load_meta 
#' @return nothing
#' @examples
#' library(flowWorkspace)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' #output to flowJo
#' outFile <- tempfile(fileext = ".wsp")
#' gatingset_to_flowjo(gs, outFile)
#'
#' @rdname gatingset_to_flowjo
GatingSet2flowJo <- function(...){
  .Deprecated("gatingset_to_flowjo")
  gatingset_to_flowjo(...)
  }
#' @export
#' @rdname gatingset_to_flowjo
gatingset_to_flowjo <- function(gs, outFile, ...){
  encoding <- localeToCharset()[1]
  if(encoding == "ISO8859-1")
    encoding <- "ISO-8859-1"
  #have a dry run of saveXML served as a validity check on outFile to throw error at early stage instead of the end of long process
  suppressWarnings(saveXML(xmlNode("Workspace"), file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", encoding)))
  #validity check for slash
  # for(chnl in colnames(gs))
  # {
  #
  #   if(grepl("/", chnl))
  #     stop("'/' is found in channel '", chnl, "'! Please update GatingSet by running 'gs <- fix_channel_slash(gs)'")
  # }
  
  #NOTE we call a lot of flowWorkspace accessors, they need to be imported explicitly. Otherwise the user needs to load flowWorkspace explicitly before using CytoML.
  # see all the NOTES in R CMD check that have to do with "no visible global function / binding / variable". 
  chnls <- colnames(gs)
  slash_loc <- sapply(chnls, function(thisCol)as.integer(gregexpr("/", thisCol)[[1]]), simplify = FALSE)
  new_cnd <- fix_channel_slash(chnls, slash_loc)
  if(!all(new_cnd == chnls)){
    gs <- gs_copy_tree_only(gs) # ensure everything else is cloned except hdf5
    cs <- gs_pop_get_data(gs)
    cs_unlock(cs)#temporarily allow it to be writable
    gs <- gs_update_channels(gs, map = data.frame(old = chnls, new = new_cnd))
    cs_lock(cs)
  }

  ws <- workspaceNode(gs, outputdir = dirname(outFile))
  
  #restore meta from disk to prevent the change to be permanant
  cs_load_meta(gs_pop_get_data(gs))
  encoding <- localeToCharset()[1]
  if(encoding == "ISO8859-1")
  encoding <- "ISO-8859-1"
  ## Write out to an XML file (suppress the warning due to the usage of deprecated structure call in saveXML)
  suppressWarnings(saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", encoding)
                           )
                   )
}

# overwrite XML:::saveXML.XMLNode
#saveXML ignores encoding argument thus we are not sure what is used by the underlying textConnection Call
#which causes trouble to read it back later on when the encoding info is assumed to be UTF-8
# .saveXML <- function (doc, file = NULL, compression = 0, indent = TRUE, prefix = "<?xml version=\"1.0\"?>\n", 
#           doctype = NULL, encoding = "UTF-8", ...) 
# {
#   cn = file(file, "w"
#             # , encoding = encoding
#             )
#   sink(cn)
#   on.exit(sink())
#   on.exit(close(cn), add = TRUE)
#   
#   
#   if (!is.null(prefix)) 
#     cat(as.character(prefix))
#   if (!is.null(doctype)) 
#     cat(as(doctype, "character"), "\n")
#   print(doc)
#   file
# }
workspaceNode <- function(gs, ...){
  guids <- sampleNames(gs)
  sampleIds <- seq_along(guids)
  xmlNode("Workspace"
          , attrs = c(version = "20.0"
                                  , "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance"
                                  , "xmlns:gating" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating"
                                  , "xmlns:transforms" = "http://www.isac-net.org/std/Gating-ML/v2.0/transformations"
                                  , "xmlns:data-type" ="http://www.isac-net.org/std/Gating-ML/v2.0/datatypes"
                                  , "xsi:schemaLocation" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating http://www.isac-net.org/std/Gating-ML/v2.0/gating/Gating-ML.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/transformations http://www.isac-net.org/std/Gating-ML/v2.0/gating/Transformations.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/datatypes http://www.isac-net.org/std/Gating-ML/v2.0/gating/DataTypes.v2.0.xsd"
                                 )
          , groupNode(sampleIds)
          , SampleListNode(gs, sampleIds, ...)

        )

}


groupNode <- function(sampleIds){
  xmlNode("Groups"
          , xmlNode("GroupNode"
                    , attrs = c(name="All Samples")
                    , xmlNode("Group"
                              , attrs = c(name="All Samples")
                              , xmlNode("SampleRefs"
                                        , .children = lapply(sampleIds
                                                             , function(sampleId){
                                                                xmlNode("SampleRef", attrs = c(sampleID = sampleId))
                                                             })
                                        )
                              )
                    )
          )
}

SampleListNode <- function(gs, sampleIds, outputdir, ...){
  xmlNode("SampleList"
          , .children = lapply(sampleIds, function(sampleId){
                    guid <- sampleNames(gs)[sampleId]
                    gh <- gs[[guid]]
                    matInfo <- getSpilloverMat(gh)

                    #global variable that keep records of the referenced NOT node
                    #so that the same NOT node will not be generated repeatly if referred multiple times
                    #it is also used to store the derived parameters and gate info
                    #so that it doesn't need to be processed second time
                    env.nodes <- new.env(parent = emptyenv())
                    env.nodes[["DerivedParameters"]] <- new.env(parent = emptyenv())
                    dp <- DerivedParametersNode(gh, env.nodes, outputdir = outputdir, ...)
                    xmlNode("Sample"
                            , datasetNode(gh, sampleId)
                            , spilloverMatrixNode(matInfo)
                            , transformationNode(gh, matInfo)
                            , keywordNode(gh)
                            , sampleNode(gh, sampleId = sampleId
                                         , matInfo = matInfo
                                         , env.nodes = env.nodes, ...)
                            , dp
                    )
                  })
        )
}

#' @importFrom flowWorkspace extract_cluster_pop_name_from_node
DerivedParameterNode <- function(sn, parent, childnodes, vec, cluster_name, env.nodes, outputdir){
  pname <- paste(cluster_name, gsub("/", ".", parent), sep = ".")
  #create hash map for pop vs derived parameter info
  pops <- levels(vec)  
  for(pop_path in childnodes)
  {
    pop <- extract_cluster_pop_name_from_node(pop_path, cluster_name)
    env.nodes[["DerivedParameters"]][[pop_path]] <- list(name = pname
                                                         , value = match(pop, pops))
      
  }
    
  vec <- as.integer(vec)
  vec[is.na(vec)] <- 0 #set NA to zeros
  rg <- range(vec)
  vec <- data.frame(vec)
  colnames(vec) <- pname
  
  csvfile <- paste(sn, pname, "EPA.csv", sep = ".")
  csvpath <- file.path(outputdir, csvfile)
  write.csv(vec, csvpath, row.names = FALSE)
  message("DerivedParameter: ", normalizePath(csvpath))
  xmlNode("DerivedParameter"
          , attrs = c(name = pname
                    , type = "importCsv"
                    , importFile = csvfile
                    , range = as.character(rg[2] + 1)
                    , columnIndex="1"
                    )
          , xmlNode("Transform"
                    ,xmlNode("transforms:linear"
                             , attrs = c("transforms:minRange" = "0"
                                        , "transforms:maxRange" = as.character(format_float(rg[2] + 1))
                                        , gain="1")
                             , xmlNode("parameter"
                                       , namespace = "data-type"
                                       , "data-type:name" = pname)
                             )
                    )
          )
}

#' @importFrom flowWorkspace gh_pop_get_cluster_name gh_get_cluster_labels
DerivedParametersNode <- function(gh, ...){
  sn <- sampleNames(gh)
  dpnodes <- lapply(gs_get_pop_paths(gh, path = "auto"), function(parent){
                    childnodes <- gs_pop_get_children(gh, parent, path = "auto")
                    cluster_names <- compact(sapply(childnodes, function(nd){
                                            gh_pop_get_cluster_name(gh, nd)
                                          }))
                    lapply(unique(cluster_names), function(cl){
                      vec <- gh_get_cluster_labels(gh, parent, cluster_method_name = cl) 
                      
                      DerivedParameterNode(sn, parent
                                           , childnodes = names(which(cluster_names == cl))
                                           , vec = vec
                                           , cluster_name = cl
                                           , ...)
                    })
                      
                    
              })
  dpnodes <- compact(dpnodes)
  dpnodes <- unlist(dpnodes, recursive = FALSE)
  xmlNode("DerivedParameters"
          , .children = dpnodes
          )
}

datasetNode <- function(gh, sampleId){

  xmlNode("DataSet", attrs = c("uri" = pData(gh)[["name"]]
                               , sampleID = sampleId)
          )

}
getSpilloverMat <- function(gh){
    compobj <- gh_get_compensations(gh)
    if(!is.null(compobj)){
      comp <- gs_get_compensation_internal(gh@pointer,sampleNames(gh))
      cid <- comp$cid
      prefix <- comp$prefix
      suffix <- comp$suffix
    }else{
      prefix <- ""
      suffix <- ""
    }

  
  if(is(compobj, "compensation")){
    mat <- compobj@spillover
  }else if(is(compobj, "matrix")){
    mat <- compobj
  }else{
    mat <- NULL
  }

  list(mat = mat, prefix = prefix,  suffix = suffix, cid = 1)

}
spilloverMatrixNode <- function(matInfo){
  mat <- matInfo[["mat"]]
  prefix <- "Comp-" #hardcode the prefix for vX
  suffix <- ""
  cid <- matInfo[["cid"]]

  if(!is.null(mat)){
    rownames(mat) <- colnames(mat) #ensure rownames has channel info
    xmlNode("transforms:spilloverMatrix"
            , attrs = c(prefix = prefix
                        , name="Acquisition-defined"
                        , editable="0"
                        , color="#c0c0c0"
                        , version="FlowJo-10.1r5"
                        , status="FINALIZED"
                        , "transforms:id" = cid
                        , suffix = suffix )

            , .children = c(list(paramerterNode(colnames(mat)))
                             , spilloverNodes(mat)

                              )

          )
  }
}
spilloverNodes <- function(mat){
  lapply(rownames(mat), function(param){
    coefVec <- mat[param, ]
    xmlNode("transforms:spillover"
            , attrs = c("data-type:parameter"=param)
            , .children = lapply(names(coefVec), function(chnl){
                xmlNode("transforms:coefficient",
                        attrs = c("data-type:parameter" = chnl
                                  , "transforms:value" = as.character(format_float(coefVec[chnl]))
                                )
                        )
                })
            )
  })

}

#' @importFrom flowCore exprs
transformationNode <- function(gh, matInfo){

  trans.objs <- gh_get_transformations(gh, only.function = FALSE)
  if(length(trans.objs) == 0)
    stop("No transformation is found in GatingSet!")
  fr <- gh_pop_get_data(gh)

  chnls <- colnames(fr)
  # chnls <- names(trans.objs)
  xmlNode("Transformations"
          , .children = lapply(chnls, function(chnl){

                paramNode <- xmlNode("data-type:parameter",  attrs = c("data-type:name" = fixChnlName(chnl, matInfo)))
                trans.obj <- trans.objs[[chnl]]
                if(is.null(trans.obj)){
                  trans.type <- "flowJo_linear"
                }else
                {
                  trans.type <- trans.obj[["name"]]
                  func <- trans.obj[["transform"]]
                }

                if(trans.type == "flowJo_biexp"){
                  param <-  attr(func,"parameters")
                  transNode <- xmlNode("biex"
                                      , namespace = "transforms"
                                      , attrs = c("transforms:length" = param[["channelRange"]]
                                                  , "transforms:maxRange" = param[["maxValue"]]
                                                  , "transforms:neg" = param[["neg"]]
                                                  , "transforms:width" = format_float(param[["widthBasis"]])
                                                  , "transforms:pos" = format_float(param[["pos"]])
                                                  )
                                      )
                }else if(trans.type == "flowJo_caltbl"){
                  warning("Calibration table is stored in GatingSet!We are unable to restore the original biexp parameters,thus use the default settings (length = 4096, neg = 0, width = -10, pos = 4.5), which may or may not produce the same gating results.")

                  transNode <- xmlNode("biex"
                                      , namespace = "transforms"
                                      , attrs = c("transforms:length" = 4096
                                                  , "transforms:maxRange" = 262144
                                                  , "transforms:neg" = 0
                                                  , "transforms:width" = -10
                                                  , "transforms:pos" = 4.5
                                              )
                                      )
                }else if(trans.type == "flowJo_fasinh"){
                    param <- as.list(environment(func))

                    transNode <- xmlNode("fasinh"
                                         , namespace = "transforms"
                                         , attrs = c("transforms:length" = param[["length"]]
                                                     , "transforms:maxRange" = param[["t"]]
                                                     , "transforms:T" = param[["t"]]
                                                     , "transforms:A" = param[["a"]]
                                                     , "transforms:M" = param[["m"]]
                                         )
                    )

                }else if(trans.type == "flowJo_flog"){
                  param <- as.list(environment(func))
                  transNode <- xmlNode("log"
                                       , namespace = "transforms"
                                       , attrs = c("transforms:offset" = param[["offset"]]
                                                   , "transforms:decades" = param[["decade"]]
                                                   )
                  )

                }else if(trans.type == "flowJo_linear"){
                  if(grepl("time", chnl, ignore.case = TRUE)){
                    rg <- range(exprs(fr)[, chnl])
                    gain <- compute_timestep(keyword(fr), rg, timestep.source = "TIMESTEP")

                  }else
                  {
                    rg <- range(fr)[, chnl]
                    gain <- 1

                  }
                  minRange <- rg[1]
                  maxRange <- rg[2]
                  transNode <- xmlNode("linear"
                                       , namespace = "transforms"
                                       , attrs = c("transforms:minRange" = format_float(minRange)
                                                   , "transforms:maxRange" = format_float(maxRange)
                                                   , "gain" = gain
                                       )
                  )

                }else if(trans.type %in% c("logicle", "flowJo_logicle")){
                  param <- as.list(environment(func))
                  withBasis <- - 10 ^ (2 * as.vector(param[["w"]]))
                  transNode <- xmlNode("biex"
                                       , namespace = "transforms"
                                       , attrs = c("transforms:length" = 4096
                                                   , "transforms:maxRange" = param[["t"]]
                                                   , "transforms:neg" = param[["a"]]
                                                   , "transforms:width" = format_float(withBasis)
                                                   , "transforms:pos" = format_float(param[["m"]])
                                       )
                  )

                }
                else
                  stop("unsupported transformation: ", trans.type)

                addChildren(transNode, paramNode)
              })
          )
}

paramerterNode <- function(params){

  xmlNode("data-type:parameters"
          , .children = lapply(params, function(param){
                        xmlNode("data-type:parameter", attrs = c("data-type:name"=param))
                    })
          )

}
#' @importFrom flowWorkspace keyword
keywordNode <- function(gh){
  kw <- keyword(gh)
  kns <- names(kw)
  kns <- kns[!grepl("flowCore", kns)]
  #skip spillover matrix for now since it requires the special care (see flowCore:::collapseDesc)
  kns <- kns[!grepl("SPILL", kns, ignore.case = TRUE)]
  
  xmlNode("Keywords", .children = lapply(kns, function(kn){
                          kv <- kw[[kn]]
                          kv <- paste(kv, collapse = " ") #handle the exceptional kw value that is multi-element vector
                          kv <- enc2native(kv) #handle the special encoded character
                          xmlNode("Keyword", attrs = c(name = kn, value = kv))
                })
          )
}

# replace the original perfix with "Comp-" since vX only accepts this particular one
fixChnlName <- function(chnl, matInfo){
  mat <- matInfo[["mat"]]
  if(!is.null(mat)){ #only do this when spillover matrix is present
    #get raw chnl
    prefix <- matInfo[["prefix"]]
    suffix <- matInfo[["suffix"]]
    chnl <- sub(paste0("^", prefix), "", chnl) #strip prefix
    chnl <- sub(paste0(suffix, "$"), "", chnl) #strip suffix
    #only do it when chnl is used in compensation matrix
    if(chnl %in% colnames(mat))
      chnl <- paste0("Comp-", chnl) #prepend the new one
  }
  chnl

}

#' @importFrom flowWorkspace gh_pop_get_stats
sampleNode <- function(gh, sampleId, matInfo, showHidden = FALSE, env.nodes, ...){

  sn <- pData(gh)[["name"]]
  stat <- gh_pop_get_stats(gh, "root", xml = FALSE)[[2]]
  children <- gs_pop_get_children(gh, "root", path = "auto")
  if(!showHidden)
    children <- children[!sapply(children, function(child)gh_pop_is_hidden(gh, child))]
  param <- as.vector(parameters(gh_pop_get_gate(gh, children[1])))


  param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
  trans <- gh_get_transformations(gh, only.function = FALSE)
  
  env.nodes[["NotNode"]] <- character(0)
  xmlNode("SampleNode", attrs = c(name = sn
                                  , count = stat
                                  , sampleID = sampleId
                                  )
                      , graphNode(param)
                      , subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden, env.nodes = env.nodes, ...)
          )
}

graphNode <- function(param){
  x <- param[1]
  if(length(param)==1)
    y <- ""
  else
    y <- param[2]
  xmlNode("Graph"
          , attrs = c(smoothing="0", backColor="#ffffff", foreColor="#000000", type="Pseudocolor", fast="1")
          , xmlNode("Axis", attrs = c(dimension="x", name= x, label="", auto="auto"))
          , xmlNode("Axis", attrs = c(dimension="y", name= y, label="", auto="auto"))
          )
}

constructPopNode <- function(gh, pop, trans, matInfo, showHidden = FALSE, env.nodes, quad.gate = NULL){
  if(!gh_pop_is_hidden(gh, pop)||showHidden)
  {
    dpinfo <- env.nodes[["DerivedParameters"]][[pop]]
    
    if(is.null(quad.gate))
    {
      if(is.null(dpinfo))
        gate <- gh_pop_get_gate(gh, pop)
      else
      {
        #create range gate for the clusterGate
        coord <- dpinfo[["value"]]
        coord <- list(c(coord - 0.5, coord + 0.5))
        names(coord) <- dpinfo[["name"]]
        gate <- rectangleGate(coord)
      }
    }else
      gate <- quad.gate
    
    eventsInside <- !gh_pop_is_negated(gh, pop)
    children <- gs_pop_get_children(gh, pop, path = "auto")
    if(!showHidden)
      children <- children[!sapply(children, function(child)gh_pop_is_hidden(gh, child))]

    isBool <- is.null(dpinfo)&&is(gate, "booleanFilter")

    if(length(children) == 0){ #leaf node
      if(isBool){
        #use parent gate's dims for boolean node
        gate.dim <- gh_pop_get_gate(gh, gs_pop_get_parent(gh, pop, path = "auto"))
      }else
        gate.dim <- gate
      subNode <- NULL
    }else{
      #get dim from non-boolean children
      nonBool <- sapply(children, function(child){
        thisGate <- gh_pop_get_gate(gh, child)
        !is.null(env.nodes[["DerivedParameters"]][[child]])||!is(thisGate, "booleanFilter")
      })
      if(sum(nonBool) == 0)
        stop("Can't find any non-boolean children node under ", pop)

      children.dim <- children[nonBool]
      gate.dim <- gh_pop_get_gate(gh, children.dim[1]) #pick the first children node for dim
      subNode <- subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden, env.nodes = env.nodes)
    }

    if(!isBool){
      gate <- inverseTransGate(gate, trans)

    }

    param <- as.vector(parameters(gate.dim))
    param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
    count <- gh_pop_get_stats(gh, pop, xml = FALSE)[[2]]

    if(is.na(count))
      count <- -1
    if(isBool){
      booleanNode(gate, pop, count, env.nodes = env.nodes, param = param, subNode = subNode)

    }else{

      list(xmlNode("Population"
                   , attrs = c(name = basename(pop), count = count)
                   , graphNode(param)
                   , xmlNode("Gate"
                             , gateNode(gate, eventsInside, matInfo = matInfo)
                   )
                   , subNode
      )
      )
    }
  }
}
subPopulationNode <- function(gh, pops, trans, matInfo, showHidden = FALSE, env.nodes){
  #reconstruct quadgate when needed
  groups <- ggcyto:::merge.quad.gates(gh, pops)

  subPops <- list()
  i <- 1
  for(pops in groups)
  {
    if(is.list(pops)){#check if multi pops
      pops <- pops[["popIds"]]

      if(is.list(pops)){#check if multi pops have been merged to quad gate
        quad.gate <- pops[["quad.gate"]]
        quad.patterns <- pops[["quad.pattern"]]
        pops <- pops[["pop.name"]]
        for(j in seq_along(pops))
        {
          pop <- pops[j]
          attr(quad.gate, "quad.pattern") <- quad.patterns[j]
          subPops[[i]] <- constructPopNode(gh, pop, trans, matInfo, showHidden, env.nodes, quad.gate = quad.gate)
          i <- i+1
        }
      }else
        for(pop in pops)
        {
          subPops[[i]] <- constructPopNode(gh, pop, trans, matInfo, showHidden, env.nodes)
          i <- i+1
        }
    }else
    {
      subPops[[i]] <- constructPopNode(gh, pops, trans, matInfo, showHidden, env.nodes)
      i <- i+1
    }

  }

    xmlNode("Subpopulations", .children = unlist(subPops, recursive = FALSE, use.names = FALSE))
}

#' @importFrom flowWorkspace filterObject
booleanNode <- function(gate, pop, count, env.nodes, ...){

  parsed <- filter_to_list(gate)

  op <- parsed[["op"]][-1]
  op <- unique(op)
  nOp <- length(op)

  isNot <- parsed[["isNot"]]
  nNot <- sum(isNot)

  refs <- parsed[["refs"]]

  if(nOp >= 2)
    stop("And gate and Or gate can't not be used together!")


  if(nOp == 0){ #basic single NotNode
    if(nNot == 0)
      stop("isNot flag must be TRUE in 'Not' boolean gate!")

    nodeName <- "NotNode"

    if(pop %in% env.nodes[["NotNode"]]){
      res <- list(NULL)
    }else{
      res <- boolXmlNode(nodeName, pop, count, refs, ...)
      res <- list(res)
      env.nodes[["NotNode"]] <- c(env.nodes[["NotNode"]], pop)
    }

  }else{#combine gates

    if(nNot == 0){ #simple & or | gate

      if(op == "&"){
        nodeName <- "AndNode"
      }else if(op == "|"){
        nodeName <- "OrNode"
      }else
        stop("unsupported logical operation: ", op)

      res <- boolXmlNode(nodeName, pop, count, refs, ...)
      res <- list(res)
    }else{ # with Not gates included

      # try to separate NOT gates first

      #update the references
      new.refs <- mapply(isNot, refs, FUN = function(flag, ref){
                      suffix <- ifelse(flag, "-", "")
                      pop.old <- basename(ref)
                      pop.new <- paste0(pop.old, suffix)
                      file.path(dirname(ref), pop.new)
                      })

      #deal with NOT gates first
      not.nodes <- mapply(isNot, refs, new.refs, FUN = function(flag, ref, new.ref){
                            if(flag){
                              exprs <- as.symbol(paste0("!", ref))
                              new.gate <- eval(substitute(booleanFilter(v), list(v = exprs)))

                              booleanNode(new.gate, pop = basename(new.ref), count = -1, env.nodes = env.nodes, ...)[[1]]
                            }

                          })
      #take core of the bool gate based on newly generated NOT gates
      exprs <- as.symbol(paste0(new.refs, collapse = op))
      new.gate <- eval(substitute(booleanFilter(v), list(v = exprs)))
      new.node <- booleanNode(new.gate, pop = pop, count = count, env.nodes = env.nodes, ...)
      res <- c(not.nodes, new.node)

    }
  }
  res
}

boolXmlNode <- function(nodeName, pop, count, refs, param, subNode){
  xmlNode(nodeName
        , attrs = c(name = basename(pop), count = count)
        , graphNode(param)
        , xmlNode("Dependents", .children = lapply(refs
                                                   , function(ref){
                                                     xmlNode("Dependent", attrs = c(name = ref))
                                                   })
        )
        , subNode
  )

}
inverseTransGate <- function(gate, trans){

  params <- as.vector(parameters(gate))
  chnls <- names(trans)

  for(i in seq_along(params)){
    param <- params[i]
    ind <- chnls %in% param
    nMatched <- sum(ind)
    if(nMatched == 1){

      trans.obj <- trans[[which(ind)]]
      inv.fun <- trans.obj[["inverse"]]
      #rescale
      gate <- rescale_gate(gate, inv.fun, param)

    }else if(nMatched > 1)
      stop("multiple trans matched to :", param)
    }

    gate

}

gateAttr <- function(eventsInside){
  c(eventsInside = as.character(sum(eventsInside))
    , annoOffsetX="0"
    , annoOffsetY="0"
    , tint="#000000"
    , isTinted="0"
    , lineWeight="Normal"
    , userDefined="1"
    , percentX="0"
    , percentY="0"
  )
}
#customize precision for double to match up to pugixml behavior
format_float <- function(x){
  if(!is.null(x))
    x <- format(x, digits = 10)
  x
}
#modified based on xmlDimensionNode
xmlDimensionNode <- function(parameter, min = NULL, max = NULL)
{
#  min <- ggcyto:::.fixInf(min)
#  max <- ggcyto:::.fixInf(max)
	if(!is.null(min)&&is.infinite(min))
		min <- NULL
	if(!is.null(max)&&is.infinite(max))
		max <- NULL
	xmlNode("dimension"
          , namespace = "gating"
          , attrs = c("gating:min" = as.character(format_float(min)), "gating:max" = as.character(format_float(max)))
          , xmlNode("fcs-dimension"
                    , namespace = "data-type"
                    , attrs = c("data-type:name" = parameter)
                    )
        )

}

gateNode <- function(gate, ...)UseMethod("gateNode")

gateNode.default <- function(gate, ...)stop("unsupported gate type: ", class(gate))


gateNode.ellipsoidGate <- function(gate, ...){

  gate <- methods::as(gate, "polygonGate")
  gateNode(gate, ...)
}

gateNode.polygonGate <- function(gate, matInfo, ...){

  param <- parameters(gate)

  param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
  dims <- lapply(param, xmlDimensionNode)

  verts <- apply(gate@boundaries, 1, xmlVertexNode)
  xmlNode("PolygonGate"
          , namespace="gating"
          , attrs = gateAttr(...)
          , .children=c(dims, verts)
          )
}
# gateNode.ellipsoidGate <- function(gate){
  # xmlEllipsoidGateNode(gate)
# }
gateNode.rectangleGate <- function(gate, matInfo, ...){
  param <- parameters(gate)

  dims <- lapply(param, function(x){

            chnl <- fixChnlName(x, matInfo = matInfo)
            xmlDimensionNode(parameter = chnl, min = gate@min[[x]], max = gate@max[[x]])
              })
  xmlNode("RectangleGate"
          , namespace="gating"
          , attrs = gateAttr(...)
          , .children=dims)
}

#' @param quad a character of size 2, indicating the quadrant pattern .e.g. '+-')
#' @noRd
gateNode.quadGate <- function(gate, matInfo, ...){
  quad <- attr(gate, "quad.pattern")
  stopifnot(grepl("^[\\+-]{2}$", quad))
  quad <- strsplit(quad, split = "")[[1]]
  param <- parameters(gate)

  dims <- lapply(1:2, function(i){
      chnl <- fixChnlName(param[i], matInfo = matInfo)
      if(quad[i] == "+")
        xmlDimensionNode(parameter = chnl, min = gate@boundary[[param[i]]])
      else
        xmlDimensionNode(parameter = chnl, max = gate@boundary[[param[i]]])
                  })
  xmlNode("RectangleGate"
          , namespace="gating"
          , attrs = gateAttr(...)
          , .children=dims)
}
