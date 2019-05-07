#############################################################
## This set tests Gating-ML 2.0 output.
## In this case, we manually create several Quad gates and
## save the Gating-ML. It should be saved as quad gates.
## Then we read the quad gates, we wil obtain rectangle gates
## but the results should be the same as expected.
## The quad gates are combined with compensation and/or
## scale transformations.
## In addition, this also tests the Gating-ML 1.5 ratio 
## transform being saved in Gating-ML 2.0, then retrieved
## as Gating-ML 2.0 fratio with the same results.
## Additional tests include asinhtGml2 with a directly 
## embedded (rather than referenced) ratio, filters rather 
## than filter references for Boolean gates,  
## asinht from Gating-ML 1.5
#############################################################

csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_6", package = "gatingMLData")

flowEnv=new.env()

myQuad <- quadGate(filterId = "myQuad", "FSC-A" = 15000, "SSC-A" = 16000)
flowEnv[['myQuad']] <- myQuad
rm(myQuad)

myCompQuad <- quadGate(filterId = "myCompQuad", "PE-A" = 100, "PerCP-Cy5-5-A" = 200)
compPars = list(
    compensatedParameter(parameters="PE-A", spillRefId="SpillFromFCS", transformationId=paste("PE-A", "_compensated_according_to_FCS", sep=""), searchEnv=flowEnv),
    compensatedParameter(parameters="PerCP-Cy5-5-A", spillRefId="SpillFromFCS", transformationId=paste("PerCP-Cy5-5-A", "_compensated_according_to_FCS", sep=""), searchEnv=flowEnv)
)
myCompQuad@parameters = new("parameters", compPars)
flowEnv[['myCompQuad']] <- myCompQuad
rm(myCompQuad)
rm(compPars)

myTrQuad <- quadGate(filterId = "myTrQuad", "APC-A" = 0.5, "APC-Cy7-A" = 0.5)
trArcSinH1 = asinhtGml2(parameters = "APC-A", T = 1000, M = 4.5, A = 0, transformationId="trArcSinH1")
trLogicle1 = logicletGml2(parameters = "APC-Cy7-A", T = 1000, W = 0.5, M = 4.5, A = 0, transformationId="trLogicle1")
flowEnv[['trArcSinH1']] <- trArcSinH1
flowEnv[['trLogicle1']] <- trLogicle1
trPars = list(
    transformReference("trArcSinH1", flowEnv),
    transformReference("trLogicle1", flowEnv)
)
myTrQuad@parameters = new("parameters", trPars)
flowEnv[['myTrQuad']] <- myTrQuad
rm(trPars)
rm(myTrQuad)
rm(trArcSinH1)
rm(trLogicle1)

myTrCompQuad <- quadGate(filterId = "myTr!Comp Quad", "APC-A" = 0.5, "APC-Cy7-A" = 0.5)
trArcSinH2 = asinhtGml2(parameters = "APC-A", T = 1000, M = 4, A = 0, transformationId="trArcSinH2")
trLogicle2 = logicletGml2(parameters = "APC-Cy7-A", T = 1000, W = 0.3, M = 4.5, A = 0, transformationId="trLogicle2")
trArcSinH2@parameters = compensatedParameter(parameters="APC-A", spillRefId="SpillFromFCS", transformationId=paste("FL3-H", "_compensated_according_to_FCS", sep=""), searchEnv=flowEnv)
trLogicle2@parameters = compensatedParameter(parameters="APC-Cy7-A", spillRefId="SpillFromFCS", transformationId=paste("FL4-H", "_compensated_according_to_FCS", sep=""), searchEnv=flowEnv)
trPars = list(trArcSinH2,trLogicle2)
myTrCompQuad@parameters = new("parameters", trPars)
flowEnv[['myTr!Comp Quad']] <- myTrCompQuad
rm(trPars)
rm(myTrCompQuad)
rm(trArcSinH2)
rm(trLogicle2)

rat1 <- ratio("FSC-A", "SSC-A", transformationId = "rat1")
gate1 <- rectangleGate(filterId="gate1", "rat1"=c(0.8, 1.4))
gate1@parameters = new("parameters", list(rat1))
flowEnv[['gate1']] <- gate1
rm('gate1')
rm('rat1')

trArcSinH = asinhtGml2(parameters = "rat2", T = 1000, M = 4.5, A = 0, transformationId="trArcSinH")
rat2 <- ratio("FSC-A", "APC-A", transformationId = "rat2")
trArcSinH@parameters = rat2
gate2 <- rectangleGate(filterId="gate2", "rat2"=c(0.6, 1.3))
gate2@parameters = new("parameters", list(trArcSinH))
flowEnv[['gate2']] <- gate2
rm('gate2')
rm('rat2')
rm('trArcSinH')

rg1 <- rectangleGate(filterId="rg1", list("FSC-A"=c(0200, 16000), "SSC-A"=c(0, 34000)))
rg2 <- rectangleGate(filterId="rg2", list("PE-A"=c(100, 8000), "APC-Cy7-A"=c(0, 59000)))
orGate <- new("unionFilter", filterId="orGate", filters=list(rg1, rg2))
flowEnv[['orGate']] <- orGate
andGate <- new("intersectFilter", filterId="andGate", filters=list(rg1, rg2))
flowEnv[['andGate']] <- andGate
notGate <- new("complementFilter", filterId="notGate", filters=list(rg1))
flowEnv[['notGate']] <- notGate
parentGate <- new("subsetFilter", filterId="parentGate", filters=list(rg1, rg2))
flowEnv[['parentGate']] <- parentGate
rm(list=c('rg1', 'rg2', 'orGate', 'andGate', 'notGate', 'parentGate'))

trArcSinHGml1.5 = asinht(parameters = "APC-A", a = 1, b = 1, transformationId="trArcSinHGml1.5")
gateAsinhGml1.5 <- rectangleGate(filterId="gateAsinhGml1.5", "trArcSinHGml1.5"=c(0.3, 4.7))
gateAsinhGml1.5@parameters = new("parameters", list(trArcSinHGml1.5))
flowEnv[['gateAsinhGml1.5']] <- gateAsinhGml1.5
rm(list=c('gateAsinhGml1.5', 'trArcSinHGml1.5'))

gateFile <- tempfile(fileext=".gating-ml2.xml")
write.gatingML(flowEnv, gateFile)

### Now read the Gating-ML back in and check 

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
fcsFile <-  system.file("extdata/Gml2/FCSFiles", "9399_1_3_NKR.fcs", package = "gatingMLData")
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")

### Now we check all the expected results.

test.myCompQuad.NN <- function()
{
    gateId  <- "myCompQuad.NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myCompQuad.NP <- function()
{
    gateId  <- "myCompQuad.NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myCompQuad.PN <- function()
{
    gateId  <- "myCompQuad.PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myCompQuad.PP <- function()
{
    gateId  <- "myCompQuad.PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuad.NN <- function()
{
    gateId  <- "myQuad.NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuad.NP <- function()
{
    gateId  <- "myQuad.NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuad.PN <- function()
{
    gateId  <- "myQuad.PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myQuad.PP <- function()
{
    gateId  <- "myQuad.PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTrQuad.NN <- function()
{
    gateId  <- "myTrQuad.NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTrQuad.NP <- function()
{
    gateId  <- "myTrQuad.NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTrQuad.PN <- function()
{
    gateId  <- "myTrQuad.PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTrQuad.PP <- function()
{
    gateId  <- "myTrQuad.PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTr.Comp.Quad.NN <- function()
{
    gateId  <- "myTr.Comp.Quad.NN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTr.Comp.Quad.NP <- function()
{
    gateId  <- "myTr.Comp.Quad.NP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTr.Comp.Quad.PN <- function()
{
    gateId  <- "myTr.Comp.Quad.PN"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.myTr.Comp.Quad.PP <- function()
{
    gateId  <- "myTr.Comp.Quad.PP"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.gate1 <- function()
{
    gateId  <- "gate1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.gate2 <- function()
{
    gateId  <- "gate2"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.andGate <- function()
{
    gateId  <- "andGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.notGate <- function()
{
    gateId  <- "notGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.orGate <- function()
{
    gateId  <- "orGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.parentGate <- function()
{
    gateId  <- "parentGate"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header = FALSE)
    CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

test.failNicelyWithCompoundTransformsASinH <- function()
{
    require("RUnit")
    flowEnvX = new.env()
    XtrArcSinH1 = asinhtGml2(parameters = "FL1-H", T = 1000, M = 4.5, A = 0, transformationId="XtrArcSinH1")
    XtrArcSinH2 = asinhtGml2(parameters = "XtrArcSinH1", T = 1000, M = 4.5, A = 0, transformationId="XtrArcSinH2")
    XtrArcSinH2@parameters = XtrArcSinH1
    myRectGate <- rectangleGate(filterId="myRectGate", list("XtrArcSinH2"=c(200, 600)))
    myRectGate@parameters = new("parameters", list(XtrArcSinH2))
    flowEnvX[['myRectGate']] <- myRectGate
    rm(list=c('XtrArcSinH1', 'XtrArcSinH2', 'myRectGate'))
    x <- tryCatch(write.gatingML(flowEnvX), error = function(e) { e })
    expectedError = x$message == "Unexpected parameter class asinhtGml2, compound transformations are not supported in Gating-ML 2.0." 
    RUnit:::checkTrue(expectedError, "Did not get the error message that we were hoping for.")
}

test.failNicelyWithCompoundTransformsLogicle <- function()
{
    require("RUnit")
    flowEnvX = new.env()
    logicle1 = logicletGml2(parameters = "FL1-H", T = 1000, M = 4.5, A = 0, transformationId="logicle1")
    logicle2 = logicletGml2(parameters = "logicle1", T = 1000, M = 4.5, A = 0, transformationId="logicle2")
    logicle2@parameters = logicle1
    myRect <- rectangleGate(filterId="myRect", list("logicle2"=c(0, .6)))
    myRect@parameters = new("parameters", list(logicle2))
    flowEnvX[['myRect']] <- myRect
    rm(list=c('logicle1', 'logicle2', 'myRect'))
    x <- tryCatch(write.gatingML(flowEnvX), error = function(e) { e })
    expectedError = x$message == "Unexpected parameter class logicletGml2, compound transformations are not supported in Gating-ML 2.0."
    RUnit:::checkTrue(expectedError, "Did not get the error message that we were hoping for.")
}

test.gateAsinhGml1.5 <- function()
{
	gateId  <- "gateAsinhGml1.5"
	csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
	expectedResult <- read.csv(csvFile, header = FALSE)
	CytoML:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

