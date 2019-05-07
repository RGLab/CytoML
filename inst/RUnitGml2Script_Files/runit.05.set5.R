#############################################################
## This set implements additional compliance tests that
## were not included with Gating-ML 2.0. We have used R to
## generate the expected results and therefore, these tests
## are more just a sanity check whether the results are 
## consistent. Also, here, we are checking a few concepts
## that are not checked with the official tests.
#############################################################

fcsFile <-  system.file("extdata/Gml2/FCSFiles", "9399_1_3_NKR.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates5.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_5", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
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
