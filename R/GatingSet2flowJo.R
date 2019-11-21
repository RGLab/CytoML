#' @export
GatingSet2flowJo <- function(...){
  .Deprecated("gatingset_to_flowjo")
  gatingset_to_flowjo(...)
}

#' Convert a GatingSet to flowJo workspace (Deprecated by https://hub.docker.com/r/wjiang2/gs-to-flowjo)
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
  "docker run wjiang2/gs-to-flowjo"
  errcode <- system2("command", " -v docker", stdout = FALSE)
  if(errcode!=0)
    stop("'docker' command is not found! ")
  
  errcode <- system2("docker", " info", stdout = FALSE, stderr = FALSE)
  if(errcode!=0)
    stop("'docker' is not running properly! ")
  
  img <- "wjiang2/gs-to-flowjo"
  errcode <- system2("docker", paste0("  image inspect ", img), stdout = FALSE, stderr = FALSE)
  if(errcode!=0)
    stop("docker image '", img, "' is present! ")
  
  v1 <- packageVersion("cytolib")
  v2 <- system2("docker", paste0("run ", img, " --cytolib-version"), stdout = TRUE)
  if(v1!=v2)
    warning("docker image '", img, "' is built with different cytolib version of from R package: ", v2, " vs ", v1)
  
  tmp <- tempfile()
  suppressMessages(save_gs(gs, tmp))#todo:fix link="cdf"
  
  res <- suppressWarnings(system2("docker"
                                  , paste0("run"
                                           , " -v ", tmp, ":/gs"
                                           , " -v ", normalizePath(dirname(outFile)), ":/out "
                                           , img
                                           , " --src=/gs --dest=/out/", basename(outFile)
                                           , " --showHidden=", showHidden)
                                  , stderr = TRUE)
                          )
  if(length(res) > 0)
    stop(res)
}
