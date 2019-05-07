  fcsFile<-system.file("extdata/List-modeDataFiles","int-10000_events_random.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","13LargeDecisionTrees.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/13LargeDecisionTrees",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)



test.dtree1<- function() {
  gateId<-"dtree1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}