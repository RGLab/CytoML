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
  if(!file.exists(CYTOLIBML_BIN))
    stop("cytolib-ml commandline tool is not found in ", CYTOLIBML_BIN)
  tmp <- tempfile()
  suppressMessages(save_gs(gs, tmp, cdf = "symlink"))
  res <- suppressWarnings(system2(CYTOLIBML_BIN, paste0(" --src=", tmp, " --dest=", outFile, " --showHidden=", showHidden), stderr = TRUE))
  if(length(res) > 0)
    stop(res)
}
