  fcsFile<-system.file("extdata/List-modeDataFiles","int-10_events_8_parameters.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","20TrRatio.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/20TrRatio",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
 
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)



test.TrGr1<- function() {
	gateId<-"TrGr1"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}



test.TrGr2<- function() {
	gateId<-"TrGr2"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr3<- function() {
	gateId<-"TrGr3"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr4<- function() {
	gateId<-"TrGr4"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr5<- function() {
	gateId<-"TrGr5"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr6<- function() {
	gateId<-"TrGr6"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr7<- function() {
	gateId<-"TrGr7"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr8<- function() {
	gateId<-"TrGr8"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr9<- function() {
	gateId<-"TrGr9"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.TrGr10<- function() {
	gateId<-"TrGr10"
	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
	expectedResult<-read.csv(csvFile,header=TRUE)
	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}
# 
# test.TrR2<- function() {
# 	gateId<-"TrR2"
# 	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
# 	expectedResult<-read.csv(csvFile,header=TRUE)
# 	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.TrR3<- function() {
# 	gateId<-"TrR3"
# 	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
# 	expectedResult<-read.csv(csvFile,header=TRUE)
# 	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.TrR5<- function() {
# 	gateId<-"TrR5"
# 	csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
# 	expectedResult<-read.csv(csvFile,header=TRUE)
# 	CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }