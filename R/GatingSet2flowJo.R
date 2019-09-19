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

  gs_to_flowjo(gs@pointer, outFile, showHidden)
  
  #restore meta from disk to prevent the change to be permanant
  cs_load_meta(gs_pop_get_data(gs))
#  encoding <- localeToCharset()[1]
#  if(encoding == "ISO8859-1")
#  encoding <- "ISO-8859-1"
  ## Write out to an XML file (suppress the warning due to the usage of deprecated structure call in saveXML)
#  suppressWarnings(saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", encoding)
#                           )
#                   )
	
}
