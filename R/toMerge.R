#' @importFrom flowCore colnames<-
#' @importFrom ncdfFlow colnames<-
myupdateChannels <- function(gs, map, all = TRUE){

  map <- flowWorkspace:::.preprocessMap(gs, map)

  #update gates and comps ,trans(c++ part)
  flowWorkspace:::.updateChannels(gs, map)

  #update the externally stored comps,trans (R part)
  if(!is.null(gs@compensation)){
    gs@compensation <- lapply(gs@compensation, function(comp){
          mat <- comp@spillover
          cols <- colnames(mat)
          new <- .matchCols(cols, map)

          colnames(mat) <- new
          compensation(mat)
    })
  }

  if(!is.null(gs@transformation)){
    cols <- names(gs@transformation)
    new <- .matchCols(cols, map)
    names(gs@transformation) <- new
  }

  #update flow data
  if(all){
    fs <- flowData(gs)
    cols <- colnames(fs)
    newCols <- .matchCols(cols, map)
    colnames(fs) <- newCols
    flowData(gs) <- fs
    gs
  }

}

.matchCols <- function(cols, map){
  sapply(cols, function(col){
    colInd <- match(col, map[["old"]])
    ifelse(is.na(colInd), col, map[["new"]][colInd])
  }, USE.NAMES = F)
}

#' light weight version of clone method
#' eventually will merge to flowWorkspace
clone.GatingSet <- function(x,...){

  clone <- x
  #clone c structure
  message("cloning tree structure...")
  clone@pointer <- flowWorkspace:::.cpp_CloneGatingSet(x@pointer,sampleNames(x))
  clone@guid <- flowWorkspace:::.uuid_gen()

  #deep copying flow Data
  message("cloning flow data...")
  fs <- flowData(x)
  if(isNcdf(x))
    fs_clone <- clone.ncdfFlowSet(fs,...)
  else
    fs_clone<-flowCore:::copyFlowSet(fs)

  flowData(clone) <- fs_clone

  message("GatingSet cloned!")
  clone
}
