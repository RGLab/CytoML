context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/gatingML"

test_that("gatingML-cytobank parsing: custom comp and gates with prefixed channels ",{
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