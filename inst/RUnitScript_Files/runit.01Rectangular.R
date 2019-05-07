  fcsFile<-system.file("extdata/List-modeDataFiles","int-gating_test_file.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","01Rectangular.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/01Rectangular",package="gatingMLData"))

  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)

test.LessThanMin <- function() 
{
    gateId="LessThanMin"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.EqualToMin <- function() 
{
    gateId="EqualToMin"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.GreaterThanMin <- function() 
{
    gateId="GreaterThanMin"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.LessThanMax <- function() 
{
    gateId="LessThanMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.EqualToMax <- function() 
{
    gateId="EqualToMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.GreaterThanMax <- function() 
{
    gateId="GreaterThanMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.BetweenMinAndMax <- function() 
{
    gateId="BetweenMinAndMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.EqualToMinAndLessThanMax <- function() 
{
    gateId="EqualToMinAndLessThanMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.GreaterThanMinAndEqualToMax <- function() 
{
    gateId="GreaterThanMinAndEqualToMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

# test.EqualToMinAndMax <- function() 
# {   
#     gateId="EqualToMinAndMax"
#     csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#     expectedResult=read.csv(csvFile,header=TRUE)
#     CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
# }

test.GreaterThanMinAndMax <- function() 
{
    gateId="GreaterThanMinAndMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.LessThanMinAndMax <- function() 
{
    gateId="LessThanMinAndMax"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

# test.MinGreaterThanMax <- function() 
# {   
#     gateId="MinGreaterThanMax"
#     csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#     expectedResult=read.csv(csvFile,header=TRUE)
#     CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
# }

test.InNoDimensions <- function() 
{
    gateId="InNoDimensions"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.InOneDimensions <- function() 
{
    gateId="InOneDimensions"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.InAllDimensions <- function() 
{
    gateId="InAllDimensions"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}


