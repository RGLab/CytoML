  fcsFile<-system.file("extdata/List-modeDataFiles","int-gating_test_file.fcs",package="gatingMLData")
  gateFile <- system.file("extdata/Gating-MLFiles","14BooleanGates.xml",package="gatingMLData")
  csvFile<-paste(system.file("extdata/ExpectedResults/14BooleanGates",package="gatingMLData"))
  flowEnv=new.env()
  read.gatingML(gateFile,flowEnv)
  fcs <- read.FCS(fcsFile,transformation=FALSE)
 
test.AndAllDefinedTwoArgAllIn<- function() {
  gateId<-"AndAllDefinedTwoArgAllIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndManyArgs0In<- function() {
  gateId<-"AndManyArgs0In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndManyArgs1In<- function() {
  gateId<-"AndManyArgs1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndManyArgsAllIn<- function() {
  gateId<-"AndManyArgsAllIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndManyArgsSomeIn<- function() {
  gateId<-"AndManyArgsSomeIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndMixedTwoArg1In<- function() {
  gateId<-"AndMixedTwoArg1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndTwoArg0In<- function() {
  gateId<-"AndTwoArg0In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndTwoArg1In<- function() {
  gateId<-"AndTwoArg1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.AndTwoArgAllIn<- function() {
  gateId<-"AndTwoArgAllIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}
# 
# test.In1<- function() {
#   gateId<-"In1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.In2<- function() {
#   gateId<-"In2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.In3<- function() {
#   gateId<-"In3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.In4<- function() {
#   gateId<-"In4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.In5<- function() {
#   gateId<-"In5"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.In6<- function() {
#   gateId<-"In6"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }

# test.NestedIn1<- function() {
#   gateId<-"NestedIn1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedIn2<- function() {
#   gateId<-"NestedIn2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedIn3<- function() {
#   gateId<-"NestedIn3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedOut1<- function() {
#   gateId<-"NestedOut1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedOut2<- function() {
#   gateId<-"NestedOut2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedOut3<- function() {
#   gateId<-"NestedOut3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.NestedOut4<- function() {
#   gateId<-"NestedOut4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }

test.NotEventInReferencedGate<- function() {
  gateId<-"NotEventInReferencedGate"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.NotEventNotInDefinedGate<- function() {
  gateId<-"NotEventNotInDefinedGate"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.NotEventNotInReferencedGate<- function() {
  gateId<-"NotEventNotInReferencedGate"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrAllDefinedTwoArg0In<- function() {
  gateId<-"OrAllDefinedTwoArg0In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrManyArgs0In<- function() {
  gateId<-"OrManyArgs0In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrManyArgs1In<- function() {
  gateId<-"OrManyArgs1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrManyArgsAllIn<- function() {
  gateId<-"OrManyArgsAllIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrManyArgsSomeIn<- function() {
  gateId<-"OrManyArgsSomeIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrMixedTwoArg1In<- function() {
  gateId<-"OrMixedTwoArg1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrTwoArg0In<- function() {
  gateId<-"OrTwoArg0In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrTwoArg1In<- function() {
  gateId<-"OrTwoArg1In"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.OrTwoArgAllIn<- function() {
  gateId<-"OrTwoArgAllIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}
# 
# test.Out1<- function() {
#   gateId<-"Out1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.Out2<- function() {
#   gateId<-"Out2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.Out3<- function() {
#   gateId<-"Out3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.Out4<- function() {
#   gateId<-"Out4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.Out5<- function() {
#   gateId<-"Out5"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test.Out6<- function() {
#   gateId<-"Out6"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbi1<- function() {
#   gateId<-"_xorbi1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbi2<- function() {
#   gateId<-"_xorbi2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbi3<- function() {
#   gateId<-"_xorbi3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbi4<- function() {
#   gateId<-"_xorbi4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbo1<- function() {
#   gateId<-"_xorbo1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbo2<- function() {
#   gateId<-"_xorbo2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbo3<- function() {
#   gateId<-"_xorbo3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorbo4<- function() {
#   gateId<-"_xorbo4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }

test.XorBothIn<- function() {
  gateId<-"XorBothIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.XorBothOut<- function() {
  gateId<-"XorBothOut"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}
# 
# test._xoroi1<- function() {
#   gateId<-"_xoroi1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xoroi2<- function() {
#   gateId<-"_xoroi2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xoroi3<- function() {
#   gateId<-"_xoroi3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xoroi4<- function() {
#   gateId<-"_xoroi4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }

test.XorOneIn<- function() {
  gateId<-"XorOneIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}

test.XorOtherIn<- function() {
  gateId<-"XorOtherIn"
      csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
      expectedResult<-read.csv(csvFile,header=TRUE)
      CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
}
# 
# test._xorothi1<- function() {
#   gateId<-"_xorothi1"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorothi2<- function() {
#   gateId<-"_xorothi2"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorothi3<- function() {
#   gateId<-"_xorothi3"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }
# 
# test._xorothi4<- function() {
#   gateId<-"_xorothi4"
#       csvFile<-paste(csvFile,"/",gateId,".txt",sep="")
#       expectedResult<-read.csv(csvFile,header=TRUE)
#       CytoML:::performGateTest(gateId,fcs,expectedResult,flowEnv)
# }