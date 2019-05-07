  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_6_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","26TrSinH.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/26TrSinH",package="gatingMLData"))
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
 
test.TrSHG1<- function() {
	gateId<-"TrSHG1"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSHG2<- function() {
	gateId<-"TrSHG2"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSHG3<- function() {
	gateId<-"TrSHG3"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSHG4<- function() {
	gateId<-"TrSHG4"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSHG5<- function() {
	gateId<-"TrSHG5"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrSHG6<- function() {
	gateId<-"TrSHG6"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}