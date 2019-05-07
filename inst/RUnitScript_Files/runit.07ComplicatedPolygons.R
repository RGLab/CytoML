  fcsFile<-system.file("extdata/List-modeDataFiles","int-15_scatter_events.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","07ComplicatedPolygons.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/07ComplicatedPolygons",package="gatingMLData"))
    
  #source("~/R_HOME/proj/xmlTestSuite/RUnitScript_Files/CytoML:::performGateTest.R")
  
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
   # expectedResults<-read.csv(csvFile,header=TRUE)


test.G1i1<- function() {
  gateId<-"G1i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G1i2<- function() {
  gateId<-"G1i2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G1i3<- function() {
  gateId<-"G1i3"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G1i4<- function() {
  gateId<-"G1i4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G2i1<- function() {
  gateId<-"G2i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G2i2<- function() {
  gateId<-"G2i2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i1<- function() {
  gateId<-"G3i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i2<- function() {
  gateId<-"G3i2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i3<- function() {
  gateId<-"G3i3"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i4<- function() {
  gateId<-"G3i4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i5<- function() {
  gateId<-"G3i5"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i6<- function() {
  gateId<-"G3i6"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G3i7<- function() {
  gateId<-"G3i7"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G4i1<- function() {
  gateId<-"G4i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G4i2<- function() {
  gateId<-"G4i2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G4i3<- function() {
  gateId<-"G4i3"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G4i4<- function() {
  gateId<-"G4i4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G5i1<- function() {
  gateId<-"G5i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G5i2<- function() {
  gateId<-"G5i2"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G5i3<- function() {
  gateId<-"G5i3"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G5i4<- function() {
  gateId<-"G5i4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G5i5<- function() {
  gateId<-"G5i5"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i100<- function() {
  gateId<-"G6i100"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i1000<- function() {
  gateId<-"G6i1000"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i10000<- function() {
  gateId<-"G6i10000"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i101<- function() {
  gateId<-"G6i101"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i1024<- function() {
  gateId<-"G6i1024"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i16<- function() {
  gateId<-"G6i16"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i2347<- function() {
  gateId<-"G6i2347"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i4<- function() {
  gateId<-"G6i4"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G6i8<- function() {
  gateId<-"G6i8"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.G7i1<- function() {
  gateId<-"G7i1"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}