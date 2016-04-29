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
#' outFile <- tempfile(fileext = ".xml")
#' GatingSet2GatingML(gs, outFile)
#'
#'
#' }
GatingSet2flowJo <- function(gs, outFile, ...){

  ws <- workspaceNode(gs)

  ## Write out to an XML file
  saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", localeToCharset()))
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
                                                xmlNode("Sample", datasetNode(gh)
                                                        , spilloverMatrixNode(gh)
                                                        , transformationNode(gh)
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


sampleNode <- function(gh, showHidden = FALSE){


}

