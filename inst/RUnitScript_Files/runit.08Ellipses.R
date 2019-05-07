  fcsFile<-system.file("extdata/List-modeDataFiles","int-gating_test_file_4D.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","08Ellipses.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/08Ellipses",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)


test.Ellipse_01i<- function() {
  gateId<-"Ellipse_01i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_02i<- function() {
  gateId<-"Ellipse_02i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_03o<- function() {
  gateId<-"Ellipse_03o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_04i<- function() {
  gateId<-"Ellipse_04i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_05i<- function() {
  gateId<-"Ellipse_05i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_06o<- function() {
  gateId<-"Ellipse_06o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_07i<- function() {
  gateId<-"Ellipse_07i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_08o<- function() {
  gateId<-"Ellipse_08o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_09i<- function() {
  gateId<-"Ellipse_09i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_10o<- function() {
  gateId<-"Ellipse_10o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_11i<- function() {
  gateId<-"Ellipse_11i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_12o<- function() {
  gateId<-"Ellipse_12o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_13o<- function() {
  gateId<-"Ellipse_13o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_14i<- function() {
  gateId<-"Ellipse_14i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_15i<- function() {
  gateId<-"Ellipse_15i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_16o<- function() {
  gateId<-"Ellipse_16o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_17o<- function() {
  gateId<-"Ellipse_17o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_18i<- function() {
  gateId<-"Ellipse_18i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_19i<- function() {
  gateId<-"Ellipse_19i"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Ellipse_20o<- function() {
  gateId<-"Ellipse_20o"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}