context("gatingset_to_flowjo ..")
skip_if_not((check_docker_status()[1]=="docker_ok" || check_binary_status()=="binary_ok"))
test_that("autogating--tcell", {
  
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  #load raw FCS
  fs <- read.flowSet(file.path(dataDir,"CytoTrol_CytoTrol_1.fcs"))
  gs <- GatingSet(fs)

  #compensate
  comp <- spillover(fs[[1]])[["SPILL"]]
  chnls <- colnames(comp)
  comp <- compensation(comp)
  gs <- compensate(gs, comp)

  #transform
  trans <- flowjo_biexp_trans()
  trans <- transformerList(chnls, trans)
  gs <- transform(gs, trans)

  #run auto gating
  gtFile.orig <- system.file("extdata/gating_template/tcell.csv", package = "openCyto")
  gtFile <- tempfile()
  tbl <- data.table::fread(gtFile.orig)
  tbl[5, gating_args:= "gate_range = c(1e3, 3e3)"]
  tbl[c(8,11), gating_args:= "gate_range = c(2e3, 3e3)"]
  write.csv(tbl, file = gtFile)
  gt <- gatingTemplate(gtFile)
  expect_warning(gating(gt, gs))

  gt_toggle_helpergates(gt, gs) #hide the helper gates
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)

  #cross validatation test
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = dataDir)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 6e-4)

  ####################
  #use logicle
  ####################
  gs <- GatingSet(fs)
  gs <- compensate(gs, comp)
  trans <- estimateLogicle(gs[[1]], chnls)
  gs <- transform(gs, trans)
  gt <- gatingTemplate(gtFile.orig)
  expect_warning(gating(gt, gs))
  gt_toggle_helpergates(gt, gs) #hide the helper gates
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo

  gatingset_to_flowjo(gs, outFile)
  #cross validatation test
  cross_validate(gs, outFile)
  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = dataDir)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 6e-4)

})
test_that("gatingset_to_flowjo: manual gates with calibration table parsed and stored as biexp ",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  stats.orig <- gh_pop_compare_stats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  
  #cross validatation test
  cross_validate(gs, outFile)

  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = dataDir)
  stats.new <- gh_pop_compare_stats(gs1[[1]])
  expect_equal(stats.orig, stats.new, tol = 5e-3)
})

test_that("gatingset_to_flowjo: export clustering results as derived parameters ",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  gh <- gs[[1]]
  params <- parameters(gh_pop_get_gate(gh, "CD4"))
  gs_pop_remove("CD4", gs = gs)
  gs_pop_remove("CD8", gs = gs)
  gs_pop_remove("DNT", gs = gs)
  gs_pop_remove("DPT", gs = gs)
  #run flowClust

  fr <- gh_pop_get_data(gh, "CD3+")
  library(flowClust)
  res <- flowClust(fr, varNames = params, K = 2, nu = 1, trans = 0)
  # plot(res, data = fr)
  #add results as factor
  Map <- selectMethod("Map", sig = "flowClust")
  res <- Map(res)
  res <- as.factor(res)
  pop_add(res, gh, parent = "CD3+", name = "flowclust")

  rect <- rectangleGate(`<B710-A>` = c(500, 1500), `<R780-A>` = c(3500, 4000))
  pop_add(rect, gh, parent = "flowclust_1", name = "rect")
  recompute(gh)
  stats.orig <- gh_pop_compare_stats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  # outFile <- "~/test.wsp"
  gatingset_to_flowjo(gs, outFile)
  
  #cross validatation test
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = dataDir)
  stats.new <- gh_pop_compare_stats(gs1[[1]])
  expect_equal(stats.orig[-(5:7)], stats.new, tol = 5e-3)
})

test_that("gatingset_to_flowjo: handle special encoding in keywords ",{
  data(GvHD)
  fs<-GvHD[1:3]
  gs <- GatingSet(fs)
  biexpTrans <- flowjo_biexp_trans(channelRange=4096, maxValue=262144, pos=4.5,neg=0, widthBasis=-10)
  transList <- transformerList(colnames(fs[[1]])[3:6], biexpTrans)
  gs<-transform(gs,transList)
  fs_trans<- gs_pop_get_data(gs)
  
  ###Adding the cluster
  clean.inds <- lapply(1:length(fs_trans), function(i1) return(list(ind=which(exprs(fs_trans[[i1]])[,"Time"]>793))))
  clean.clust <- lapply(1:length(fs_trans), function(x){
    vec<-rep(0,nrow(fs_trans[[x]]))
    if (length(clean.inds[[x]]$ind)>0)
    {
      
      vec[clean.inds[[x]]$ind]<-1
    }
    # }else{
    #   vec[[1]]<-1
    # }
    vec <- as.factor(vec)
    levels(vec) <- c("0", "1")
    return(vec)
  })
  names(clean.clust)<-sampleNames(fs_trans)
  
  gs_pop_add(gs,clean.clust, parent="root",name = "Clean")
  recompute(gs)
  
  #add one gate
  rg <- rectangleGate("FSC-H"=c(200,400), "SSC-H"=c(250, 400),
                      filterId="rectangle")
  
  
  
  nodeID<-gs_pop_add(gs, rg,parent="Clean_0")#it is added to root node by default if parent is not specified
  recompute(gs)
  # autoplot(gs, "rectangle")
  
  #add a quadGate
  qg <- quadGate("FL1-H"=1e3, "FL2-H"=1.5e3)
  nodeIDs<-gs_pop_add(gs,qg,parent="rectangle")
  recompute(gs)

  outFile <- tempfile(fileext = ".wsp")
  outDir <- dirname(outFile)
  gatingset_to_flowjo(gs, outFile)
  
  #cross validatation test
  cross_validate(gs, outFile)
  
  write.flowSet(fs, outDir)
  ws <- open_flowjo_xml(outFile)

  # Make duplicate fcs directory below top directory to test file match cases
  copy_dir <- file.path(outDir, "fcs_copies")
  dir.create(copy_dir)
  file.copy(file.path(outDir, paste0(sampleNames(fs), ".fcs")), copy_dir)
  # The fcs files in copy_dir should interfere
  expect_error(flowjo_to_gatingset(ws, name = 1), "Multiple FCS files match", class = "error")
  # Use greedy_match to avoid the duplicate
  gs2 <- flowjo_to_gatingset(ws, name = 1, greedy_match = TRUE)
  # stats1 <- gh_pop_compare_stats(gs)
  expect_is(gs2, "GatingSet")
})
