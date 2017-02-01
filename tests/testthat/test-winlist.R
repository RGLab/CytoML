context("parse winlist protocal ..")

test_that("tcell", {
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  xmlfile <- system.file('extdata/NewProtocol.wlx', package = "CytoML")

  gs <- winlist2GatingSet(xmlfile, dataDir)


  #' ## verify the stats are correct
  stats <- getPopStats(gs[[1]])

  expect_equal(stats[, flowJo.count], stats[, flowCore.count], tolerance = 0.0018)


  expect_equal(getNodes(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4','/P1/P2/P3/P4/P5'))

})
