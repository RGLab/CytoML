context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/gatingML"

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

test_that("GatingSet2flowJo: rectangleGate",{
  dataDir <- "/fh/fast/gottardo_r/mike_working/wsTestSuite/curlyQuad/example1"
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
  expect_equal(stats.orig, stats.new)
})