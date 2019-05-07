  fcsFile<-system.file("extdata/List-modeDataFiles","int-10000_events_random.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","06LargePolygons.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/06LargePolygons",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)


test.G100pts<- function() {
  gateId<-"G100pts"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G10pts<- function() {
  gateId<-"G10pts"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G250pts<- function() {
  gateId<-"G250pts"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}