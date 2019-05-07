  fcsFile<-system.file("extdata/List-modeDataFiles","int-homogenous_matrix.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","03LargeRectangular.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/03LargeRectangular",package="gatingMLData"))
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)

test.Gate01 <- function() 
{   
    gateId="Gate01"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate02 <- function() 
{   
    gateId="Gate02"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate03 <- function() 
{   
    gateId="Gate03"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate04 <- function() 
{   
    gateId="Gate04"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate05 <- function() 
{   
    gateId="Gate05"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate06 <- function() 
{   
    gateId="Gate06"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}


test.Gate07 <- function() 
{   
    gateId="Gate07"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate08 <- function() 
{   
    gateId="Gate08"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate11 <- function() 
{   
    gateId="Gate11"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate12 <- function() 
{   
    gateId="Gate12"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate13 <- function() 
{   
    gateId="Gate13"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}


test.Gate14 <- function() 
{   
    gateId="Gate14"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate15 <- function() 
{   
    gateId="Gate15"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate16 <- function() 
{   
    gateId="Gate16"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate17 <- function() 
{   
    gateId="Gate17"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate18 <- function() 
{   
    gateId="Gate18"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate21 <- function() 
{   
    gateId="Gate22"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate21 <- function() 
{   
    gateId="Gate22"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate23 <- function() 
{   
    gateId="Gate23"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate24 <- function() 
{   
    gateId="Gate24"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate25 <- function() 
{   
    gateId="Gate25"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate26 <- function() 
{   
    gateId="Gate26"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate27 <- function() 
{   
    gateId="Gate27"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate28 <- function() 
{   
    gateId="Gate28"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate29 <- function() 
{   
    gateId="Gate29"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate30 <- function() 
{   
    gateId="Gate30"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate31 <- function() 
{   
    gateId="Gate31"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate32 <- function() 
{   
    gateId="Gate32"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate33 <- function() 
{   
    gateId="Gate33"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate34 <- function() 
{   
    gateId="Gate34"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate35 <- function() 
{   
    gateId="Gate35"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate36 <- function() 
{   
    gateId="Gate36"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate37 <- function() 
{   
    gateId="Gate37"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate38 <- function() 
{   
    gateId="Gate38"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate39 <- function() 
{   
    gateId="Gate39"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate40 <- function() 
{   
    gateId="Gate40"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate41 <- function() 
{   
    gateId="Gate41"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate42 <- function() 
{   
    gateId="Gate42"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate43 <- function() 
{   
    gateId="Gate43"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate44 <- function() 
{   
    gateId="Gate44"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate45 <- function() 
{   
    gateId="Gate45"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gate46 <- function() 
{   
    gateId="Gate46"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep1 <- function() 
{   
    gateId="Gatep1"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep2 <- function() 
{   
    gateId="Gatep2"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep3 <- function() 
{   
    gateId="Gatep3"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep4 <- function() 
{   
    gateId="Gatep4"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep5 <- function() 
{   
    gateId="Gatep5"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep6 <- function() 
{   
    gateId="Gatep6"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}

test.Gatep7 <- function() 
{   
    gateId="Gatep7"
    csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
    expectedResult=read.csv(csvFile,header=TRUE)
    CytoML:::performGateTest(gateId,fcs, expectedResult,flowEnv)
}