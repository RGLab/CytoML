#' @useDynLib CytoML
NULL

#' save the event counts parsed from xml into c++ tree structure
#'
#' It is for internal use by the diva parser
#'
#' @param gh GatingHierarchy
#' @param node the unique gating path that uniquely identifies a population node
#' @param count integer number that is events count for the respective gating node directly parsed from xml file
#' @export
#' @examples
#' \dontrun{
#' set.count.xml(gh, "CD3", 10000)
#' }
set.count.xml <- function(gh, node, count){
  .set.count.xml(gh@pointer, sampleNames(gh), node, count)
}