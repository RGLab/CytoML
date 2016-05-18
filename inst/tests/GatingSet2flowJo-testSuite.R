context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

test_that("NotNode ",{
  thisPath <- file.path(path, "combineNode/OrNode")
  wsFile <- file.path(thisPath, "Test_EW.wsp")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, path = file.path(path))
  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, sampNloc = "sampleNode")
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new)

})


test_that("Time gate ",{
  thisPath <- file.path(path, "flin")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, subset = 1)
  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, sampNloc = "sampleNode")
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new)
})

test_that("Time gate2--when computed timestep is very different from $TIMESTEP ",{
  thisPath <- file.path(path, "timegate")
  wsFile <- file.path(thisPath, "MX1 Analysis VISC.xml")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws,name="Group 1",subset=11)
  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, sampNloc = "sampleNode")
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new, tol = 1.3e-5)
})
test_that("EllipsoidGate defined on log-transformed channels ",{
  thisPath <- file.path(path, "ellipsoid_log")
  wsFile <- file.path(thisPath, "xml_spillover2.xml")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name=1, execute = T, sampNloc = "sampleNode", subset = "spillover_B2.fcs")

  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, sampNloc = "sampleNode")
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new)
})
test_that("GatingSet2flowJo: rectangleGate + boolgate",{
  dataDir <- "/fh/fast/gottardo_r/mike_working/wsTestSuite/curlyQuad/example1"
  ws <- openWorkspace(file.path(dataDir, "20151208_TBNK_DS.xml"))
  gs <- parseWorkspace(ws, subset = 1, name = 2)


  #   #rename the node since these names with special characters seems to fail in reference node finding
  #   setNode(gs, "Q2: CD16+56 PE-A+ , CD19 APC-A+",  "Q2")
  #   setNode(gs, "Q4: CD16+56 PE-A- , CD19 APC-A-", "Q4")
  #   #add boolgate
  #   bf <- booleanFilter(Q2|Q4)
  #   add(gs, bf, name = "Q2 or Q4", parent = "Non T cells")
  #   recompute(gs)
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #load it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])
  expect_equal(stats.orig, stats.new)
})

test_that("GatingSet2flowJo: manual gates with calibration table stored ",{
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
    expect_equal(stats.orig, stats.new)
      })

test_that("GatingSet2flowJo: no comp + fasinh ",{
  thisPath <- file.path(path, "PROVIDE")
  wsFile <- file.path(thisPath, "batch1 local and week 53.wsp")

  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, subset = 3, sampNloc = "sampleNode", additional.keys = NULL)
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, sampNloc = "sampleNode", additional.keys = NULL)
  stats.new <- getPopStats(gs1[[1]])
  expect_equal(stats.orig, stats.new)
})

test_that("GatingSet2flowJo: no transformation",{

  gs <- load_gs("/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/manual/gslist-bcell/cgRoygodqg")
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  expect_error(GatingSet2flowJo(gs, outFile), "No transformation is found in GatingSet!")

})

test_that("GatingSet2flowJo: automated gates+hidden gate + Infinity",{
  thisPath <- file.path(path, "gatingML/ics")
  #load the original automated gating set
  gs <- load_gs(file.path(thisPath, "autogating"))
  gt <- openCyto::gatingTemplate(file.path(thisPath, "template/gt_080.csv"))
  flowIncubator::toggle.helperGates(gt, gs) #hide the helper gates
  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new, tol = 2e-4)
})


test_that("tcell", {
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
  gtFile <- tempfile()
  tbl <- data.table::fread(system.file("extdata/gating_template/tcell.csv", package = "openCyto"))
  tbl[5, gating_args:= "gate_range = c(1e3, 3e3)"]
  tbl[c(8,11), gating_args:= "gate_range = c(2e3, 3e3)"]
  write.csv(tbl, file = gtFile)
  gt <- gatingTemplate(gtFile, autostart = 1L)
  gating(gt, gs)

  flowIncubator::toggle.helperGates(gt, gs) #hide the helper gates
  stats.orig <- getPopStats(gs[[1]])[, list(flowCore.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])[, list(flowCore.count, node)]
  expect_equal(stats.orig, stats.new, tol = 5e-4)


})
