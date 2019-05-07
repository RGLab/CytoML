#############################################################
## This set implements additional compliance tests that
## were not included with Gating-ML 2.0. We have used R to
## generate the expected results and therefore, these tests
## are more just a sanity check whether the results are 
## consistent. Also, here, we are checking a few concepts
## that are not checked with the official tests.
#############################################################

fcsFile <-  system.file("extdata/Gml2/FCSFiles", "9399_1_3_NKR.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates3.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_3", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")

test.my3DRectangleGate <- function()
{
    gateId  <- "my3DRectangleGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myBooleanAnd <- function()
{
    gateId  <- "myBooleanAnd"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myBooleanNot <- function()
{
    gateId  <- "myBooleanNot"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myBooleanOr <- function()
{
    gateId  <- "myBooleanOr"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myBooleanOrWithParent <- function()
{
    gateId  <- "myBooleanOrWithParent"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myEllipseGate <- function()
{
    gateId  <- "myEllipseGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myEllipsoidGate <- function()
{
    gateId  <- "myEllipsoidGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGate <- function()
{
    gateId  <- "myPolygonGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGate2ArcSinHLin <- function()
{
    gateId  <- "myPolygonGate2ArcSinHLin"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGate3LogLin <- function()
{
    gateId  <- "myPolygonGate3LogLin"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomInvertedAlreadySpillover <- function()
{
    gateId  <- "myPolygonGateWithCustomInvertedAlreadySpillover"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWithCustInvAlrSpil", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomNonSquareSpectrumMatrix <- function()
{
    gateId  <- "myPolygonGateWithCustomNonSquareSpectrumMatrix"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWithCustNonSqSpecMat", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomNonSquareSpectrumMatrixInvertedAlready <- function()
{
    gateId  <- "myPolygonGateWithCustomNonSquareSpectrumMatrixInvertedAlready"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWCustNonSqSpecInvAlrd", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomNonSquareSpectrumMatrixOnArcSinH <- function()
{
    gateId  <- "myPolygonGateWithCustomNonSquareSpectrumMatrixOnArcSinH"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWCustNonSqSpecArcSinH", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomSpillover <- function()
{
    gateId  <- "myPolygonGateWithCustomSpillover"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithCustomSpilloverAndArcSinH <- function()
{
    gateId  <- "myPolygonGateWithCustomSpilloverAndArcSinH"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWCustSpillAndArcSinH", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithFCSSpillover <- function()
{
    gateId  <- "myPolygonGateWithFCSSpillover"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithFCSSpilloverAndArcSinH <- function()
{
    gateId  <- "myPolygonGateWithFCSSpilloverAndArcSinH"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myPolygonWFCSSpillAndArcSinH", ".txt", sep="")	
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithoutSpillover <- function()
{
    gateId  <- "myPolygonGateWithoutSpillover"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myPolygonGateWithSpilloverSameAsFCS <- function()
{
    gateId  <- "myPolygonGateWithSpilloverSameAsFCS"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRangeGate1 <- function()
{
    gateId  <- "myRangeGate1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRangeGate2 <- function()
{
    gateId  <- "myRangeGate2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate <- function()
{
    gateId  <- "myRectangleGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate2bHyperlog <- function()
{
    gateId  <- "myRectangleGate2bHyperlog"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate2Logicle <- function()
{
    gateId  <- "myRectangleGate2Logicle"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate3bHyperlogArcSinH <- function()
{
    gateId  <- "myRectangleGate3bHyperlogArcSinH"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate3LogicleArcSinH <- function()
{
    gateId  <- "myRectangleGate3LogicleArcSinH"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate4bHyperlogArcSinHFCSCompensated <- function()
{
    gateId  <- "myRectangleGate4bHyperlogArcSinHFCSCompensated"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myRect4bHyperlogArcSinHFCSComp", ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myRectangleGate4LogicleArcSinHFCSCompensated <- function()
{
    gateId  <- "myRectangleGate4LogicleArcSinHFCSCompensated"
    # csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", "myRect4LogicleArcSinHFCSComp", ".txt", sep="")	
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1 <- function()
{
    gateId  <- "Q1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1A <- function()
{
    gateId  <- "Q1A"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1B <- function()
{
    gateId  <- "Q1B"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1C <- function()
{
    gateId  <- "Q1C"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1D <- function()
{
    gateId  <- "Q1D"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q1E <- function()
{
    gateId  <- "Q1E"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2 <- function()
{
    gateId  <- "Q2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2A <- function()
{
    gateId  <- "Q2A"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2B <- function()
{
    gateId  <- "Q2B"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2C <- function()
{
    gateId  <- "Q2C"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2D <- function()
{
    gateId  <- "Q2D"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q2E <- function()
{
    gateId  <- "Q2E"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3 <- function()
{
    gateId  <- "Q3"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3A <- function()
{
    gateId  <- "Q3A"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3B <- function()
{
    gateId  <- "Q3B"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3C <- function()
{
    gateId  <- "Q3C"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3D <- function()
{
    gateId  <- "Q3D"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q3E <- function()
{
    gateId  <- "Q3E"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4 <- function()
{
    gateId  <- "Q4"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4A <- function()
{
    gateId  <- "Q4A"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4B <- function()
{
    gateId  <- "Q4B"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4C <- function()
{
    gateId  <- "Q4C"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4D <- function()
{
    gateId  <- "Q4D"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q4E <- function()
{
    gateId  <- "Q4E"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q5 <- function()
{
    gateId  <- "Q5"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q6 <- function()
{
    gateId  <- "Q6"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.Q7 <- function()
{
    gateId  <- "Q7"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

