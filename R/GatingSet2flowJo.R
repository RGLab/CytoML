GatingSet2flowJo <- function(gs, outFile, showHidden = FALSE){

  #convert comp and trans as GML2 compatible format and save to env

  flowEnv <- new.env(parent = emptyenv())
  res <- export_comp_trans(gs, flowEnv, cytobank.default.scale = cytobank.default.scale, type = "cytobank")
  #convert gates to GML2
  export_gates_cytobank(gs, flowEnv, res[["trans.Gm2objs"]], res[["trans"]], res[["compId"]], showHidden = showHidden)

  tmp <- tempfile(fileext = ".xml")#ensure correct file extension for xmlTreeParse to work
  flowUtils::write.gatingML(flowEnv, tmp)
  tree <- xmlTreeParse(tmp, trim = FALSE)
  root <- xmlRoot(tree)
  # browser()

  root <- addCustomInfo(root, gs, flowEnv, showHidden = showHidden, cytobank.default.scale = cytobank.default.scale)
  #add pop (GateSet/BooleanAndGate)
  root <- addGateSets(root, gs, flowEnv[["guid_mapping"]], showHidden = showHidden)
  #add experiment info to custom node
  root <- addExperimentInfo(root)
  saveXML(root, file = outFile)
}
