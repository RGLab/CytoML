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
  xmlNode("Workspace", attrs = c("xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance"
                                  , "xmlns:gating" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating"
                                  , "xmlns:transforms" = "http://www.isac-net.org/std/Gating-ML/v2.0/transformations"
                                  , "xmlns:data-type" ="http://www.isac-net.org/std/Gating-ML/v2.0/datatypes"
                                  , "xsi:schemaLocation" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating http://www.isac-net.org/std/Gating-ML/v2.0/gating/Gating-ML.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/transformations http://www.isac-net.org/std/Gating-ML/v2.0/gating/Transformations.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/datatypes http://www.isac-net.org/std/Gating-ML/v2.0/gating/DataTypes.v2.0.xsd"
                                 )
                    , xmlNode("SampleList"
                              , .children = lapply(gs, function(gh){
                                                xmlNode("Sample"
                                                        , datasetNode(gh)
                                                        , spilloverMatrixNode(gh)
#                                                         , transformationNode(gh)
                                                        , keywordNode(gh)
                                                        , sampleNode(gh, ...)
                                                      )
                                              })
                            )
        )

}

datasetNode <- function(gh){

  xmlNode("DataSet", attrs = c("uri" = pData(gh)[["name"]]))

}
spilloverMatrixNode <- function(gh){
  mat <- getCompensationMatrices(gh)
  mat <- mat@spillover
  xmlNode("transforms:spilloverMatrix"
          , attrs = c(prefix="Comp-"
                      , name="Acquisition-defined"
                      , editable="0"
                      , color="#c0c0c0"
                      , version="FlowJo-10.1r5"
                      , status="FINALIZED"
                      , "transforms:id" = "" #"50174c36-d015-4f8a-aa3a-68ea4880341b"
                      , suffix="" )

          , .children = c(list(paramerterNode(colnames(mat)))
                           , spilloverNodes(mat)

                            )

        )

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
  kns <- kn[!grepl("flowCore", kns)]
  xmlNode("Keywords", .children = lapply(kns, function(kn){
                        xmlNode("Keyword", attrs = c(name = kn, value = as.character(kw[[kn]])))
                })
          )
}



sampleNode <- function(gh, showHidden = FALSE){
  sn <- pData(gh)[["name"]]
  nodes <- getNodes(gh, showHidden = showHidden)

  stat <- getTotal(gh, "root", flowJo = TRUE)
  children <- getChildren(gh, "root")
  param <- as.vector(parameters(getGate(gh, children[1])))
  trans <- getTransformations(gh, only.function = FALSE)
  xmlNode("SampleNode", attrs = c(name = sn, count = ifelse(is.na(stat)||stat == -1, "", stat))
                      , graphNode(param[1], param[2])
                      , subPopulationNode(gh, children, trans)
          )
}

graphNode <- function(x, y){
  xmlNode("Graph"
          , attrs = c(smoothing="0", backColor="#ffffff", foreColor="#000000", type="Pseudocolor", fast="1")
          , xmlNode("Axis", attrs = c(dimension="x", name= x, label="", auto="auto"))
          , xmlNode("Axis", attrs = c(dimension="y", name= y, label="", auto="auto"))
          )
}

subPopulationNode <- function(gh, pops, trans){
  subPops <-lapply(pops, function(pop){
                  gate <- getGate(gh, pop)
                  eventsInside <- !flowWorkspace:::isNegated(gh, pop)
                  children <- getChildren(gh, pop)
                  if(length(children) == 0){
                    gate.dim <- gate
                    subNode <- NULL
                  }else{
                    gate.dim <- getGate(gh, children[1])
                    subNode <- subPopulationNode(gh, children, trans)
                  }

                  gate <- inverseTransGate(gate, trans)

                  param <- as.vector(parameters(gate.dim))
                  xmlNode("Population"
                          , attrs = c(name = basename(pop), count = getTotal(gh, pop, flowJo = TRUE))
                          , graphNode(param[1], param[2])
                          , gateNode(gate, eventsInside)
                          , subNode
                  )
                })
    xmlNode("Subpopulations", .children = subPops)
}

inverseTransGate <- function(gate, trans){

  params <- as.vector(parameters(gate))
  trans.names <- names(trans)

  for(i in seq_along(params)){
    param <- params[i]
    ind <- grepl(param, trans.names)
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

gateNode <- function(gate, ...)UseMethod("gateNode")

gateNode.default <- function(gate, ...)stop("unsupported gate type: ", class(gate))

gateNode.polygonGate <- function(gate, ...){

  # flowUtils:::xmlRectangleGateWin(gate)
  dims <- lapply(parameters(gate), flowUtils:::xmlDimensionNode)
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
gateNode.rectangleGate <- function(gate, ...){
  # flowUtils:::xmlRectangleGateWin(gate)
  dims <- lapply(parameters(gate), function(x)
                flowUtils:::xmlDimensionNode(parameter=x, min=gate@min[x], max=gate@max[x]))
  xmlNode("RectangleGate"
          , namespace="gating"
          , attrs = gateAttr(...)
          , .children=dims)
}