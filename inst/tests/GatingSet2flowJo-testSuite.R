context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

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
test_that("GatingSet2flowJo: no transformation + boolean Gate",{

  gs <- load_gs("/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/manual/gslist-bcell/cgRoygodqg")
  stats.orig <- getPopStats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  expect_error(
    GatingSet2flowJo(gs, outFile)
    # , "no transformation"
    )

})

test_that("GatingSet2flowJo: automated gates",{
  thisPath <- file.path(path, "gatingML/ics")
  #load the original automated gating set
  gs <- load_gs(file.path(thisPath, "autogating"))
  gt <- openCyto::gatingTemplate(file.path(thisPath, "template/gt_080.csv"))
  flowIncubator::toggle.helperGates(gt, gs) #hide the helper gates
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
