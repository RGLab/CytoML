  fcsFile<-system.file("extdata/List-modeDataFiles","int-10000_events_random.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","16LargeBooleanGates.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/16LargeBooleanGates",package="gatingMLData"))
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)

test.Bool1<- function() {
  gateId<-"Bool1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect1<- function() {
  gateId<-"Rect1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect10<- function() {
  gateId<-"Rect10"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect2<- function() {
  gateId<-"Rect2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect3<- function() {
  gateId<-"Rect3"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect4<- function() {
  gateId<-"Rect4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect5<- function() {
  gateId<-"Rect5"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect6<- function() {
  gateId<-"Rect6"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect7<- function() {
  gateId<-"Rect7"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect8<- function() {
  gateId<-"Rect8"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.Rect9<- function() {
  gateId<-"Rect9"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}