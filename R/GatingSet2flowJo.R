#' Convert a GatingSet to flowJo workspace
#'
#'
#' @param gs a GatingSet object
#' @param outFile the workspace file path to write
#' @param ... other arguments
#'        showHidden whether to include the hidden population nodes in the output
#' @export
#' @importFrom flowWorkspace clone updateChannels
#' @return nothing
#' @examples
#' library(flowWorkspace)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' #output to flowJo
#' outFile <- tempfile(fileext = ".wsp")
#' GatingSet2flowJo(gs, outFile)
#'
#'
GatingSet2flowJo <- function(gs, outFile, ...){
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
  new_cnd <- flowWorkspace:::.fix_channel_slash(chnls, slash_loc)
  if(!all(new_cnd == chnls)){
    gs <- clone(gs, isNew = FALSE, isEmpty = FALSE) # ensure everything else is cloned except hdf5
    gs <- updateChannels(gs, map = data.frame(old = chnls, new = new_cnd))
  }

  pData(gs)[["name"]] <- as.character(pData(gs)[["name"]]) #coerce factor to character

  ws <- workspaceNode(gs)
  locale <- localeToCharset()[1]
  if(locale == "ISO8859-1")
    locale <- "ISO-8859-1"
  ## Write out to an XML file (suppress the warning due to the usage of deprecated structure call in saveXML)
  suppressWarnings(saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", locale)))
}

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

SampleListNode <- function(gs, sampleIds, ...){
  xmlNode("SampleList"
          , .children = lapply(sampleIds, function(sampleId){
                    guid <- sampleNames(gs)[sampleId]
                    gh <- gs[[guid]]
                    matInfo <- getSpilloverMat(gh)


                    xmlNode("Sample"
                            , datasetNode(gh, sampleId)
                            , spilloverMatrixNode(matInfo)
                            , transformationNode(gh, matInfo)
                            , keywordNode(gh)
                            , sampleNode(gh, sampleId, matInfo, ...)
                    )
                  })
        )
}

datasetNode <- function(gh, sampleId){

  xmlNode("DataSet", attrs = c("uri" = pData(gh)[["name"]]
                               , sampleID = sampleId)
          )

}
getSpilloverMat <- function(gh){
  compobj <- gh@compensation
  if(is.null(compobj)){
    compobj <- getCompensationMatrices(gh)
    if(!is.null(compobj)){
      mat <- compobj@spillover
      comp <- flowWorkspace:::.cpp_getCompensation(gh@pointer,sampleNames(gh))
      cid <- comp$cid
      prefix <- comp$prefix
      suffix <- comp$suffix
    }else{
      prefix <- ""
      suffix <- ""
      mat <- NULL
    }


  }else{
    sn <- sampleNames(gh)
    compobj <- compobj[[sn]]
    mat <- compobj@spillover
    cid <- "1"
    prefix <- ""
    suffix <- ""
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
                                  , "transforms:value" = as.character(coefVec[chnl])
                                )
                        )
                })
            )
  })

}

#' @importFrom flowCore exprs
transformationNode <- function(gh, matInfo){

  trans.objs <- getTransformations(gh, only.function = FALSE)
  if(length(trans.objs) == 0)
    stop("No transformation is found in GatingSet!")
  fr <- getData(gh)

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
                                                  , "transforms:width" = param[["widthBasis"]]
                                                  , "transforms:pos" = param[["pos"]]
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
                    gain <- flowWorkspace:::compute.timestep(keyword(fr), rg, timestep.source = "TIMESTEP")

                  }else
                  {
                    rg <- range(fr)[, chnl]
                    gain <- 1

                  }
                  minRange <- rg[1]
                  maxRange <- rg[2]
                  transNode <- xmlNode("linear"
                                       , namespace = "transforms"
                                       , attrs = c("transforms:minRange" = minRange
                                                   , "transforms:maxRange" = maxRange
                                                   , "gain" = gain
                                       )
                  )

                }else if(trans.type == "logicle"){
                  param <- as.list(environment(func))
                  withBasis <- - 10 ^ (2 * as.vector(param[["w"]]))
                  transNode <- xmlNode("biex"
                                       , namespace = "transforms"
                                       , attrs = c("transforms:length" = 4096
                                                   , "transforms:maxRange" = param[["t"]]
                                                   , "transforms:neg" = param[["a"]]
                                                   , "transforms:width" = withBasis
                                                   , "transforms:pos" = param[["m"]]
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
                          xmlNode("Keyword", attrs = c(name = kn, value = kw[[kn]]))
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

#' @importFrom flowWorkspace getTotal
sampleNode <- function(gh, sampleId, matInfo, showHidden = FALSE, ...){

  sn <- pData(gh)[["name"]]
  stat <- getTotal(gh, "root", xml = FALSE)
  children <- getChildren(gh, "root")
  if(!showHidden)
    children <- children[!sapply(children, function(child)flowWorkspace:::isHidden(gh, child))]
  param <- as.vector(parameters(getGate(gh, children[1])))


  param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
  trans <- getTransformations(gh, only.function = FALSE)
  #global variable that keep records of the referenced NOT node
  #so that the same NOT node will not be generated repeatly if referred multiple times
  env.nodes <- new.env(parent = emptyenv())
  env.nodes[["NotNode"]] <- character(0)
  xmlNode("SampleNode", attrs = c(name = sn
                                  , count = stat
                                  , sampleID = sampleId
                                  )
                      , graphNode(param[1], param[2])
                      , subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden, env.nodes = env.nodes, ...)
          )
}

graphNode <- function(x, y){
  xmlNode("Graph"
          , attrs = c(smoothing="0", backColor="#ffffff", foreColor="#000000", type="Pseudocolor", fast="1")
          , xmlNode("Axis", attrs = c(dimension="x", name= x, label="", auto="auto"))
          , xmlNode("Axis", attrs = c(dimension="y", name= y, label="", auto="auto"))
          )
}

constructPopNode <- function(gh, pop, trans, matInfo, showHidden = FALSE, env.nodes, quad.gate = NULL){
  if(!flowWorkspace:::isHidden(gh, pop)||showHidden){
    if(is.null(quad.gate))
      gate <- getGate(gh, pop)
    else
      gate <- quad.gate
    eventsInside <- !flowWorkspace:::isNegated(gh, pop)
    children <- getChildren(gh, pop)
    if(!showHidden)
      children <- children[!sapply(children, function(child)flowWorkspace:::isHidden(gh, child))]

    isBool <- is(gate, "booleanFilter")

    if(length(children) == 0){ #leaf node
      if(isBool){
        #use parent gate's dims for boolean node
        gate.dim <- getGate(gh, getParent(gh, pop))
      }else
        gate.dim <- gate
      subNode <- NULL
    }else{
      #get dim from non-boolean children
      nonBool <- sapply(children, function(child){
        thisGate <- getGate(gh, child)
        !is(thisGate, "booleanFilter")
      })
      if(sum(nonBool) == 0)
        stop("Can't find any non-boolean children node under ", pop)

      children.dim <- children[nonBool]
      gate.dim <- getGate(gh, children.dim[1]) #pick the first children node for dim
      subNode <- subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden, env.nodes = env.nodes)
    }

    if(!isBool){
      gate <- inverseTransGate(gate, trans)

    }

    param <- as.vector(parameters(gate.dim))
    param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
    count <- getTotal(gh, pop, xml = FALSE)

    if(is.na(count))
      count <- -1
    if(isBool){
      booleanNode(gate, pop, count, env.nodes = env.nodes, param = param, subNode = subNode)

    }else{

      list(xmlNode("Population"
                   , attrs = c(name = basename(pop), count = count)
                   , graphNode(param[1], param[2])
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

  parsed <- filterObject(gate)

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
        , graphNode(param[1], param[2])
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
      gate <- transform_gate(gate, inv.fun, param)

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

#modified based on flowUtils:::xmlDimensionNode
xmlDimensionNode <- function(parameter, min = NULL, max = NULL)
{
  min <- ggcyto:::.fixInf(min)
  max <- ggcyto:::.fixInf(max)
  xmlNode("dimension"
          , namespace = "gating"
          , attrs = c("gating:min" = min, "gating:max" = max)
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
  # flowUtils:::xmlEllipsoidGateNode(gate)
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
