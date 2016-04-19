GatingSet2flowJo <- function(gs, outDir, showHidden = FALSE){
  if(file.exists(outDir)){

  }else{
    dir.create(path = outDir)
  }
  #do the dir normalization again after it is created
  outDir <- normalizePath(outDir,mustWork = TRUE)

  for(sn in sampleNames(gs)){
    #convert comp and trans as GML2 compatible format and save to env
    flowEnv <- new.env(parent = emptyenv())
    res <- export_comp_trans(gs[sn], flowEnv)
    #convert gates to GML2
    export_gates_flowJo(gs[[sn]], flowEnv, res[["trans.Gm2objs"]], res[["trans"]], res[["compId"]], showHidden = showHidden)

    outFile <- file.path(outDir, paste0(sn, ".xml"))
    flowUtils::write.gatingML(flowEnv, outFile)
  }
  message("GatingML files are saved in ", outDir)
#   tree <- xmlTreeParse(tmp, trim = FALSE)
#   root <- xmlRoot(tree)
#   # browser()
#
#   root <- addCustomInfo(root, gs, flowEnv, showHidden = showHidden, ...)
#   #add pop (GateSet/BooleanAndGate)
#   root <- addGateSets(root, gs, flowEnv[["guid_mapping"]], showHidden = showHidden)
#   #add experiment info to custom node
#   root <- addExperimentInfo(root)
#   saveXML(root, file = outFile)
}

export_gates_flowJo <- function(gh, flowEnv, trans.Gm2objs, trans, compId, showHidden, rescale.gate = TRUE)
{
  #add gates and pops(as GateSets)
  nodePaths <- getNodes(gh, showHidden = showHidden)[-1]
  rng <- range(getData(gh, use.exprs = FALSE))
  for(nodePath in nodePaths){
    gate <- getGate(gh, nodePath)
    popName <- basename(nodePath)

    #cytobank does not support negated gate
    #we have to create inverse gate on our end
    if(flowWorkspace:::isNegated(gh, nodePath)){
      gate <- inverse(gate, rng)
    }
    #transform to raw scale
    #and attach comp and trans reference to parameters
    gate <- processGate(gate, trans.Gm2objs, compId, flowEnv, rescale.gate, trans)

    parent <- getParent(gh, nodePath)
    if(parent != "root"){
      parent_gate <- getGate(gh, parent)
      identifier(gate) <- "" # strip the filterId of the leaf gate so we don't write extraneous XML
      gate <- new("subsetFilter", filters=list(gate, parent_gate), filterId = popName)
    }

    #add gate
    flowEnv[[nodePath]] <- gate

  }
}

