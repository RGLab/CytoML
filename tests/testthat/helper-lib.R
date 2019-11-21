library(XML)
library(flowCore)
library(flowWorkspace)
library(openCyto)
library(ggcyto)
library(data.table)
resultDir <- "expect_result"

cross_validate <- function(gs, outFile){
  if(is.element("CytoML_old", installed.packages()[,1]))
  {
    #to be compatible with C version to avoid digits difference due to the pb archive
    tmp <- tempfile()
    save_gs(gs, tmp, cdf = "symlink")
    gs <- load_gs(tmp)
    #cross validatation test
    outFile_old <- tempfile(fileext = ".wsp")
    CytoML_old::gatingset_to_flowjo(gs, outFile_old)
  #go through XML package to eliminate the format difference (e.g. indent, namespaces,etc)  
    f1 <- xmlTreeParse(outFile)[[1]][[1]]
    f2 <- xmlTreeParse(outFile_old)[[1]][[1]]
    saveXML(f1, outFile)
    saveXML(f2, outFile_old)
    
    f1 <- scan(outFile, what = "character", quiet = TRUE)
    f2 <- scan(outFile_old, what = "character", quiet = TRUE)
    expect_equal(f1, f2)
    
  }
}