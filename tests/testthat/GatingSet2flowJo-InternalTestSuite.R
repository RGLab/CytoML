context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

test_that("GatingSet2flowJo: forward slash ",{
  thisPath <- file.path(path, "slash_issue_vX")
  gs <- load_gs(file.path(thisPath, "gs"))

  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, additional.keys = NULL)
  stats.new <- getPopStats(gs1[[1]])
  expect_equal(stats.orig[,openCyto.freq], stats.new[,openCyto.freq])
})

test_that("OrNode ",{
  thisPath <- file.path(path, "combineNode/OrNode")
  wsFile <- file.path(thisPath, "Test_EW.wsp")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile, sampNloc = "sampleNode")
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})


test_that("Time gate ",{
  thisPath <- file.path(path, "flin")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, subset = "Specimen_001_L11147 W-4_001.fcs")
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile, sampNloc = "sampleNode")
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})

test_that("Time gate2--when computed timestep is very different from $TIMESTEP ",{
  thisPath <- file.path(path, "timegate")
  wsFile <- file.path(thisPath, "MX1 Analysis VISC.xml")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws,name=1)
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile, sampNloc = "sampleNode")
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 1.3e-5)
})
test_that("EllipsoidGate defined on log-transformed channels ",{
  thisPath <- file.path(path, "ellipsoid_log")
  wsFile <- file.path(thisPath, "xml_spillover2.xml")
  ws <- openWorkspace(wsFile, sampNloc = "sampleNode")
  gs <- parseWorkspace(ws, name=1, execute = T, subset = "spillover_B2.fcs")

  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile, sampNloc = "sampleNode")
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})
test_that("GatingSet2flowJo: rectangleGate + boolgate",{
  dataDir <- file.path(path, "curlyQuad/example1")
  ws <- openWorkspace(file.path(dataDir, "20151208_TBNK_DS.xml"))
  gs <- parseWorkspace(ws, subset = 1, name = 2)

  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #load it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = dataDir)
  stats.new <- getPopStats(gs1[[1]])
  expect_equal(stats.orig, stats.new, tol = 3e-3)
})


test_that("GatingSet2flowJo: no comp + fasinh ",{
  thisPath <- file.path(path, "PROVIDE")
  wsFile <- file.path(thisPath, "batch1 local and week 53.wsp")

  ws <- openWorkspace(wsFile, sampNloc = "sampleNode")
  gs <- parseWorkspace(ws, name = 1, subset = 3, additional.keys = NULL)
  stats.orig <- getPopStats(gs[[1]])[order(node), list(node, openCyto.count)]

  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile, sampNloc = "sampleNode")
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath, additional.keys = NULL)
  stats.new <- getPopStats(gs1[[1]])[order(node), list(node, openCyto.count)]
  expect_equal(stats.orig, stats.new)
})

test_that("GatingSet2flowJo: no transformation",{

  gs <- load_gs("/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/manual/gslist-bcell/cgRoygodqg")
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  expect_error(GatingSet2flowJo(gs, outFile), "No transformation is found in GatingSet!")

})

test_that("GatingSet2flowJo: automated gates+hidden gate + Infinity + boolean gate",{
  thisPath <- file.path(path, "gatingML/ics")
  #load the original automated gating set
  gs <- load_gs(file.path(thisPath, "autogating"))
  gt <- openCyto::gatingTemplate(file.path(thisPath, "template/gt_080.csv"))
  toggle.helperGates(gt, gs) #hide the helper gates

  #add AND gate
  bf <- booleanFilter(cd4/GzB&cd4/Prf)
  add(gs, bf, name = "bool1", parent = "cd4")
  #Or gate
  bf <- booleanFilter(cd4/GzB|cd4/Prf)
  add(gs, bf, name = "bool2", parent = "cd4")
  #Not gate
  bf <- booleanFilter(!cd4/GzB)
  add(gs, bf, name = "bool3", parent = "cd4")
  #combine & and !
  bf <- booleanFilter(cd4/GzB&!cd4/Prf)
  add(gs, bf, name = "bool4", parent = "cd4")
  #combine & and !
  bf <- booleanFilter(cd4/GzB|!cd4/Prf)
  add(gs, bf, name = "bool5", parent = "cd4")
  recompute(gs)
  #mixture of & , |
  bf <- booleanFilter(cd4/GzB|cd4/Prf&cd4/IFNg)
  add(gs, bf, name = "bool6", parent = "cd4")
  recompute(gs)
  expect_error(GatingSet2flowJo(gs, outFile), "And gate and Or gate can't not be used together!")
  Rm("bool6", gs)

  # autoplot(gs[[1]], getChildren(gs[[1]], "cd4"))
  getTotal(gs[[1]], "bool5")
  # plotGate(gs, "bool4", bool = T)
  stats.orig <- getPopStats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  GatingSet2flowJo(gs, outFile)

  #parse it back in
  ws <- openWorkspace(outFile)
  gs1 <- parseWorkspace(ws, name = 1, path = thisPath)
  stats.new <- getPopStats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(getTotal(gs1[[1]], "Prf-"), 90423)
  expect_equal(stats.orig, stats.new[-26,], tol = 1.6e-4)
})

