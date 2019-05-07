  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_6_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","21TrQuadratic.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/21TrQuadratic",package="gatingMLData"))
 
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
 
test.TrQ_G1<- function() {
	gateId<-"TrQ_G1"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrQ_G2<- function() {
	gateId<-"TrQ_G2"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrQ_G3<- function() {
	gateId<-"TrQ_G3"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrQ_G4<- function() {
	gateId<-"TrQ_G4"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrQ_G5<- function() {
	gateId<-"TrQ_G5"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrQ_G6<- function() {
	gateId<-"TrQ_G6"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}