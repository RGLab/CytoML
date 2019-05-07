
  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_8_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","19TrCombination.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/19TrCombination",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
 
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)


test.TrComG01<- function() {
  gateId<-"TrComG01"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG02<- function() {
  gateId<-"TrComG02"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG03<- function() {
  gateId<-"TrComG03"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG04<- function() {
  gateId<-"TrComG04"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG05<- function() {
  gateId<-"TrComG05"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG06<- function() {
  gateId<-"TrComG06"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG07<- function() {
  gateId<-"TrComG07"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG08<- function() {
  gateId<-"TrComG08"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG09<- function() {
  gateId<-"TrComG09"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrComG10<- function() {
  gateId<-"TrComG10"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}