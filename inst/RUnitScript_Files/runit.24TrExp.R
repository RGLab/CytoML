  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_6_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","24TrExp.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/24TrExp",package="gatingMLData"))

  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
 
test.TrExpG1<- function() {
	gateId<-"TrExpG1"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrExpG2<- function() {
	gateId<-"TrExpG2"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrExpG3<- function() {
	gateId<-"TrExpG3"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrExpG4<- function() {
	gateId<-"TrExpG4"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

