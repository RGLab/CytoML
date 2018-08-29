
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
  trans <- flowJo_biexp_trans()
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

  toggle.helperGates(gt, gs) #hide the helper gates
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
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
  toggle.helperGates(gt, gs) #hide the helper gates
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo

  GatingSet2flowJo(gs, outFile)
  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 6e-4)

})
test_that("GatingSet2flowJo: manual gates with calibration table parsed and stored as biexp ",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])
  expect_equal(stats.orig, stats.new, tol = 5e-3)
})

test_that("GatingSet2flowJo: export clustering results as derived parameters ",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  gh <- gs[[1]]
  params1 <- parameters(getGate(gh, "CD4"))
  params2 <- parameters(getGate(gh, "CD4/CCR7- 45RA+"))
  
  Rm("CD4", gs)
  Rm("CD8", gs)
  Rm("DNT", gs)
  Rm("DPT", gs)
  #run flowClust
  
  fr <- getData(gh, "CD3+")
  library(flowClust)
  res1 <- flowClust(fr, varNames = params1, K = 2, nu = 1, trans = 0)
  # plot(res1, data = fr)
  #add results as factor
  res1 <- Map(res1)
  res1 <- as.factor(res1)
  add(gh, res1, parent = "CD3+", name = "flowclust1")
  
  res2 <- flowClust(fr, varNames = params2, K = 3, nu = 1, trans = 0)
  # plot(res2, data = fr)
  #add results as factor
  res2 <- Map(res2)
  res2 <- as.factor(res2)
  add(gh, res2, parent = "CD3+", name = "flowclust2")
  
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  # outFile <- "~/test.wsp"
  expect_message(GatingSet2flowJo(gs, outFile), "DerivedParameter")
  
  # #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir, execute = FALSE)
  stats.new <- getPopStats(gs1[[1]])
  expect_equivalent(stats.orig[,c(1,3,5)], stats.new[,c(2,4,5)])
})

