#############################################################
## This set tests Gating-ML 2.0 output.
#######################################
## It is based on runit.01.set1.R but this time, we 
## 1) Read the Gating-ML file into an empty enviroment
## 2) Save that enviroment into temporary Gating-ML 2.0 file
## 3) Empty the enviroment
## 4) Read the saved temporary Gating-ML file
## 5) Check that we retrieved all the gates with all the 
##    expected results correctly
#############################################################

fcsFile <-  system.file("extdata/Gml2/FCSFiles", "data1.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates1.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_1", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
gateFile2 <- tempfile(fileext=".gating-ml2.xml")
write.gatingML(flowEnv, gateFile2)
flowEnv=new.env()
read.gatingML(gateFile2, flowEnv)
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")

test.And1 <- function()
{
  gateId  <- "And1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.And2 <- function()
{
  gateId  <- "And2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.And3 <- function()
{
  gateId  <- "And3"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.And4 <- function()
{
  gateId  <- "And4"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Ellipse1 <- function()
{
  gateId  <- "Ellipse1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Ellipsoid3D <- function()
{
  gateId  <- "Ellipsoid3D"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FL2N_FL4N <- function()
{
  gateId  <- "FL2N-FL4N"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FL2N_FL4P <- function()
{
  gateId  <- "FL2N-FL4P"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FL2P_FL4N <- function()
{
  gateId  <- "FL2P-FL4N"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FL2P_FL4P <- function()
{
  gateId  <- "FL2P-FL4P"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FSCD_FL1P <- function()
{
  gateId  <- "FSCD-FL1P"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FSCD_SSCN_FL1N <- function()
{
  gateId  <- "FSCD-SSCN-FL1N"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FSCN_SSCN <- function()
{
  gateId  <- "FSCN-SSCN"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FSCN_SSCP_FL1P <- function()
{
  gateId  <- "FSCN-SSCP-FL1P"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.FSCP_SSCN_FL1N <- function()
{
  gateId  <- "FSCP-SSCN-FL1N"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Not1 <- function()
{
  gateId  <- "Not1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Or1 <- function()
{
  gateId  <- "Or1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Or2 <- function()
{
  gateId  <- "Or2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ParAnd2 <- function()
{
  gateId  <- "ParAnd2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ParAnd3 <- function()
{
  gateId  <- "ParAnd3"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Polygon1 <- function()
{
  gateId  <- "Polygon1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Polygon2 <- function()
{
  gateId  <- "Polygon2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Polygon3NS <- function()
{
  gateId  <- "Polygon3NS"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Polygon4 <- function()
{
  gateId  <- "Polygon4"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Range1 <- function()
{
  gateId  <- "Range1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Range2 <- function()
{
  gateId  <- "Range2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.RatRange1 <- function()
{
  gateId  <- "RatRange1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.RatRange1a <- function()
{
  gateId  <- "RatRange1a"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.RatRange1aBound <- function()
{
  gateId  <- "RatRange1aBound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.RatRange1Bound <- function()
{
  gateId  <- "RatRange1Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.RatRange2 <- function()
{
  gateId  <- "RatRange2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Rectangle1 <- function()
{
  gateId  <- "Rectangle1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Rectangle2 <- function()
{
  gateId  <- "Rectangle2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Rectangle3 <- function()
{
  gateId  <- "Rectangle3"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Rectangle4 <- function()
{
  gateId  <- "Rectangle4"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Rectangle5 <- function()
{
  gateId  <- "Rectangle5"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScalePar1 <- function()
{
  gateId  <- "ScalePar1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange1 <- function()
{
  gateId  <- "ScaleRange1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange1Bound <- function()
{
  gateId  <- "ScaleRange1Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange1c <- function()
{
  gateId  <- "ScaleRange1c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange2 <- function()
{
  gateId  <- "ScaleRange2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange2Bound <- function()
{
  gateId  <- "ScaleRange2Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange2c <- function()
{
  gateId  <- "ScaleRange2c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange2cBound <- function()
{
  gateId  <- "ScaleRange2cBound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange3 <- function()
{
  gateId  <- "ScaleRange3"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange3Bound <- function()
{
  gateId  <- "ScaleRange3Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange3c <- function()
{
  gateId  <- "ScaleRange3c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange4 <- function()
{
  gateId  <- "ScaleRange4"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange4Bound <- function()
{
  gateId  <- "ScaleRange4Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange4c <- function()
{
  gateId  <- "ScaleRange4c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange5 <- function()
{
  gateId  <- "ScaleRange5"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange5c <- function()
{
  gateId  <- "ScaleRange5c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange6 <- function()
{
  gateId  <- "ScaleRange6"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange6Bound <- function()
{
  gateId  <- "ScaleRange6Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange6c <- function()
{
  gateId  <- "ScaleRange6c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange7c <- function()
{
  gateId  <- "ScaleRange7c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRange8c <- function()
{
  gateId  <- "ScaleRange8c"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRect1 <- function()
{
  gateId  <- "ScaleRect1"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRect1Bound <- function()
{
  gateId  <- "ScaleRect1Bound"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.ScaleRect1Bound2 <- function()
{
  gateId  <- "ScaleRect1Bound2"
  csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
  expectedResult <- read.csv(csvFile, header = FALSE)
  CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}
