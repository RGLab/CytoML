  fcsFile<-system.file("extdata/List-modeDataFiles","fcs2_int16_13367ev_8par_GvHD.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","02CtSRectangular.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/02CtSRectangular",package="gatingMLData"))
 
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)

test.CtSR_01 <- function() 
{   
    gateId="CtSR_01"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_02 <- function() 
{   
    gateId="CtSR_02"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_03 <- function() 
{   
    gateId="CtSR_03"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_04 <- function() 
{   
    gateId="CtSR_04"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_05 <- function() 
{   
    gateId="CtSR_05"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_06 <- function() 
{   
    gateId="CtSR_06"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_07 <- function() 
{   
    gateId="CtSR_07"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_08 <- function() 
{   
    gateId="CtSR_08"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_09 <- function() 
{   
    gateId="CtSR_09"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_10 <- function() 
{   
    gateId="CtSR_10"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_11 <- function() 
{   
    gateId="CtSR_11"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_12 <- function() 
{   
    gateId="CtSR_12"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_13 <- function() 
{   
    gateId="CtSR_13"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_14 <- function() 
{   
    gateId="CtSR_14"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_15 <- function() 
{   
    gateId="CtSR_15"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_16 <- function() 
{   
    gateId="CtSR_16"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_17 <- function() 
{   
    gateId="CtSR_17"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_18 <- function() 
{   
    gateId="CtSR_18"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_19 <- function() 
{   
    gateId="CtSR_19"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_20 <- function() 
{   
    gateId="CtSR_20"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.CtSR_21 <- function() 
{   
    gateId="CtSR_21"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}