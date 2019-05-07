#############################################################
## This set tests Gating-ML 2.0 output.
#######################################
## It is based on runit.04.set4.R but this time, we 
## 1) Read the Gating-ML file into an empty enviroment
## 2) Save that enviroment into temporary Gating-ML 2.0 file
## 3) Empty the enviroment
## 4) Read the saved temporary Gating-ML file
## 5) Check that we retrieved all the gates with all the 
##    expected results correctly
#############################################################

fcsFile <-  system.file("extdata/Gml2/FCSFiles", "9399_1_3_NKR.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates4.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_4", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
gateFile2 <- tempfile(fileext=".gating-ml2.xml")
write.gatingML(flowEnv, gateFile2)
flowEnv=new.env()
read.gatingML(gateFile2, flowEnv)
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")

test.myQuadrant_NN <- function()
{
    gateId  <- "myQuadrant_NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant_NP <- function()
{
    gateId  <- "myQuadrant_NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant_PN <- function()
{
    gateId  <- "myQuadrant_PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant_PP <- function()
{
    gateId  <- "myQuadrant_PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant2_NN <- function()
{
    gateId  <- "myQuadrant2_NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant2_NP <- function()
{
    gateId  <- "myQuadrant2_NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant2_PN <- function()
{
    gateId  <- "myQuadrant2_PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant2_PP <- function()
{
    gateId  <- "myQuadrant2_PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant3_NN <- function()
{
    gateId  <- "myQuadrant3_NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant3_NP <- function()
{
    gateId  <- "myQuadrant3_NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant3_PN <- function()
{
    gateId  <- "myQuadrant3_PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant3_PP <- function()
{
    gateId  <- "myQuadrant3_PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant4_NN <- function()
{
    gateId  <- "myQuadrant4_NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant4_NP <- function()
{
    gateId  <- "myQuadrant4_NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant4_PN <- function()
{
    gateId  <- "myQuadrant4_PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuadrant4_PP <- function()
{
    gateId  <- "myQuadrant4_PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRange1 <- function()
{
    gateId  <- "myRange1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRange2 <- function()
{
    gateId  <- "myRange2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRange3 <- function()
{
    gateId  <- "myRange3"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRange4 <- function()
{
    gateId  <- "myRange4"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRange5 <- function()
{
    gateId  <- "myRange5"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}
