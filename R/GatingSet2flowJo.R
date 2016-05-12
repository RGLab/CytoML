#' Convert a GatingSet to flowJo workspace
#'
#'
#' @param gs a GatingSet object
#' @param outFile the workspace file path to write
#' @param ... other arguments
#'        showHidden whether to include the hidden population nodes in the output
#' @export
#' @examples
#' \dontrun{
#' library(flowWorkspace)
#' library(CytoML)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' #output to flowJo
#' outFile <- "/loc/no-backup/mike/test/ws/temp.wsp"#tempfile(fileext = ".wsp")
#' GatingSet2flowJo(gs, outFile)
#'
#'
#' }
GatingSet2flowJo <- function(gs, outFile, ...){

  ws <- workspaceNode(gs)

  ## Write out to an XML file
  saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", localeToCharset()[1]))
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
                            , transformationNode(gh)
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
    }else
      mat <- NULL

  }else{
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

transformationNode <- function(gh){
  trans.objs <- getTransformations(gh, only.function = FALSE)
  chnls <- names(trans.objs)
  xmlNode("Transformations"
          , .children = lapply(chnls, function(chnl){
                paramNode <- xmlNode("data-type:parameter",  attrs = c("data-type:name" = chnl))
                trans.obj <- trans.objs[[chnl]]
                trans.type <- trans.obj[["name"]]
                func <- trans.obj[["transform"]]
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

                }else
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

fixChnlName <- function(chnl, matInfo){

  isRaw <- matInfo[["prefix"]] == ""

  if(isRaw){
    if(chnl%in%colnames(matInfo[["mat"]]))
      chnl <- paste0("Comp-", chnl) #hardcode prefix for vX
  }
  chnl

}

sampleNode <- function(gh, sampleId, matInfo, showHidden = FALSE, ...){

  sn <- pData(gh)[["name"]]
  stat <- getTotal(gh, "root", flowJo = TRUE)
  children <- getChildren(gh, "root")
  if(!showHidden)
    children <- children[!sapply(children, function(child)flowWorkspace:::isHidden(gh, child))]
  param <- as.vector(parameters(getGate(gh, children[1])))


  param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
  trans <- getTransformations(gh, only.function = FALSE)
  xmlNode("SampleNode", attrs = c(name = sn
                                  , count = ifelse(is.na(stat)||stat == -1, "", stat)
                                  , sampleID = sampleId
                                  )
                      , graphNode(param[1], param[2])
                      , subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden, ...)
          )
}

graphNode <- function(x, y){
  xmlNode("Graph"
          , attrs = c(smoothing="0", backColor="#ffffff", foreColor="#000000", type="Pseudocolor", fast="1")
          , xmlNode("Axis", attrs = c(dimension="x", name= x, label="", auto="auto"))
          , xmlNode("Axis", attrs = c(dimension="y", name= y, label="", auto="auto"))
          )
}

subPopulationNode <- function(gh, pops, trans, matInfo, showHidden = FALSE){
  subPops <-lapply(pops, function(pop){
                  if(!flowWorkspace:::isHidden(gh, pop)||showHidden){

                      gate <- getGate(gh, pop)
                      eventsInside <- !flowWorkspace:::isNegated(gh, pop)
                      children <- getChildren(gh, pop)
                      if(!showHidden)
                        children <- children[!sapply(children, function(child)flowWorkspace:::isHidden(gh, child))]
                      if(length(children) == 0){
                        gate.dim <- gate
                        subNode <- NULL
                      }else{
                        gate.dim <- getGate(gh, children[1])
                        subNode <- subPopulationNode(gh, children, trans, matInfo = matInfo, showHidden = showHidden)
                      }

                      gate <- inverseTransGate(gate, trans)

                      param <- as.vector(parameters(gate.dim))

                      param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
                      count <- getTotal(gh, pop, flowJo = TRUE)
                      if(is.na(count))
                        count <- -1
                      xmlNode("Population"
                              , attrs = c(name = basename(pop), count = count)
                              , graphNode(param[1], param[2])
                              , xmlNode("Gate"
                                        , gateNode(gate, eventsInside, matInfo = matInfo)
                                        )
                              , subNode
                      )
                  }
                })
    xmlNode("Subpopulations", .children = subPops)
}

inverseTransGate <- function(gate, trans){

  params <- as.vector(parameters(gate))
  chnls <- names(trans)

  for(i in seq_along(params)){
    param <- params[i]
    ind <- sapply(chnls, function(chnl)grepl(chnl, param, fixed = TRUE), USE.NAMES = FALSE)
    nMatched <- sum(ind)
    if(nMatched == 1){

      trans.obj <- trans[[which(ind)]]
      inv.fun <- trans.obj[["inverse"]]
      #rescale
      gate <- transform(gate, inv.fun, param)

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

  gate <- as(gate, "polygonGate")
  gateNode(gate, ...)
}

gateNode.polygonGate <- function(gate, matInfo, ...){

  param <- parameters(gate)

  param <- sapply(param, fixChnlName, matInfo = matInfo, USE.NAMES = FALSE)
  dims <- lapply(param, xmlDimensionNode)

  verts <- apply(gate@boundaries, 1, flowUtils:::xmlVertexNode)
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