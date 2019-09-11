library(XML)
library(flowCore)
library(flowWorkspace)
library(openCyto)
library(ggcyto)
library(data.table)
resultDir <- "expect_result"

cross_validate <- function(gs, outFile){
  
  #cross validatation test
  outFile_old <- tempfile(fileext = ".wsp")
  CytoML::gatingset_to_flowjo(gs, outFile_old)
  # f1 <- scan(outFile, what = "character", quiet = TRUE)
  # f2 <- scan(outFile_old, what = "character", quiet = TRUE)
  f1 <- xmlTreeParse(outFile)[[1]][[1]]
  f2 <- xmlTreeParse(outFile_old)[[1]][[1]]
  expect_equal(f1, f2)
  
  
}