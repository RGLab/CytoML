#############################################################
## This set tests Gating-ML 2.0 output.
#######################################
## It is based on runit.05.set5.R but this time, we 
## 1) Read the Gating-ML file into an empty enviroment
## 2) Save that enviroment into temporary Gating-ML 2.0 file
## 3) Empty the enviroment
## 4) Read the saved temporary Gating-ML file
## 5) Check that we retrieved all the gates with all the 
##    expected results correctly
#############################################################

fcsFile <-  system.file("extdata/Gml2/FCSFiles", "9399_1_3_NKR.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates5.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_5", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
gateFile2 <- tempfile(fileext=".gating-ml2.xml")
write.gatingML(flowEnv, gateFile2)
flowEnv=new.env()
read.gatingML(gateFile2, flowEnv)
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")

test.myAnd1 <- function()
{
    gateId  <- "myAnd1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myAnd2 <- function()
{
    gateId  <- "myAnd2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myAnd3 <- function()
{
    gateId  <- "myAnd3"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myAnd4 <- function()
{
    gateId  <- "myAnd4"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myNotNot <- function()
{
    gateId  <- "myNotNot"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myOr1 <- function()
{
    gateId  <- "myOr1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myOr2 <- function()
{
    gateId  <- "myOr2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myOr3 <- function()
{
    gateId  <- "myOr3"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myOr4 <- function()
{
    gateId  <- "myOr4"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygon1 <- function()
{
    gateId  <- "myPolygon1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygon2 <- function()
{
    gateId  <- "myPolygon2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Not_myPolygon1 <- function()
{
    gateId  <- "Not_myPolygon1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Not_myPolygon2 <- function()
{
    gateId  <- "Not_myPolygon2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}
