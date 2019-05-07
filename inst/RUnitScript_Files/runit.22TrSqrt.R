  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_6_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","22TrSqrt.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/22TrSqrt",package="gatingMLData"))
 
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)

test.TrSqrtG1<- function() {
	gateId<-"TrSqrtG1"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG2<- function() {
	gateId<-"TrSqrtG2"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG3<- function() {
	gateId<-"TrSqrtG3"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG4<- function() {
	gateId<-"TrSqrtG4"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG5<- function() {
	gateId<-"TrSqrtG5"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG6<- function() {
	gateId<-"TrSqrtG6"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG7<- function() {
	gateId<-"TrSqrtG7"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG8<- function() {
	gateId<-"TrSqrtG8"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSqrtG9<- function() {
	gateId<-"TrSqrtG9"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}