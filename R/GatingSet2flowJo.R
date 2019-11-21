#' @export
GatingSet2flowJo <- function(...){
  .Deprecated("gatingset_to_flowjo")
  gatingset_to_flowjo(...)
}

#' Convert a GatingSet to flowJo workspace
#'
#' @name gatingset_to_flowjo
#' @aliases GatingSet2flowJo
#' @param gs a GatingSet object
#' @param outFile the workspace file path to write
#' @param ... other arguments
#'        showHidden whether to include the hidden population nodes in the output
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
#' @importFrom flowWorkspace gs_clone gs_update_channels pData<- cs_unlock cs_lock gs_copy_tree_only cs_load_meta 
#' @export
#' @rdname gatingset_to_flowjo
gatingset_to_flowjo <- function(gs, outFile, showHidden = FALSE){
  if(!file.exists(CYTOLIBML_BIN))
    stop("cytolib-ml commandline tool is not found in ", CYTOLIBML_BIN)
  v1 <- packageVersion("cytolib")
  v2 <- system2(CYTOLIBML_BIN, " --cytolib-version", stdout = TRUE)
  if(v1!=v2)
    stop("CYTOLIBML_BIN is built with different cytolib version of from R package: ", v2, " vs ", v1)
  tmp <- tempfile()
  suppressMessages(save_gs(gs, tmp, cdf = "symlink"))
  res <- suppressWarnings(system2(CYTOLIBML_BIN, paste0(" --src=", tmp, " --dest=", outFile, " --showHidden=", showHidden), stderr = TRUE))
  if(length(res) > 0)
    stop(res)
}
