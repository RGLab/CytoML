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
gatingset_to_flowjo <- function(gs, outFile, showHidden = FALSE){
#  encoding <- localeToCharset()[1]
#  if(encoding == "ISO8859-1")
#    encoding <- "ISO-8859-1"
#  #have a dry run of saveXML served as a validity check on outFile to throw error at early stage instead of the end of long process
#  suppressWarnings(saveXML(xmlNode("Workspace"), file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", encoding)))
# 
  
  #NOTE we call a lot of flowWorkspace accessors, they need to be imported explicitly. Otherwise the user needs to load flowWorkspace explicitly before using CytoML.
  # see all the NOTES in R CMD check that have to do with "no visible global function / binding / variable". 
  

  gs_to_flowjo(gs@pointer, outFile, showHidden)
  
#  encoding <- localeToCharset()[1]
#  if(encoding == "ISO8859-1")
#  encoding <- "ISO-8859-1"
  ## Write out to an XML file (suppress the warning due to the usage of deprecated structure call in saveXML)
#  suppressWarnings(saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", encoding)
#                           )
#                   )
	
}
