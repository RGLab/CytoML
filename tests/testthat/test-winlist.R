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

test_that("",{

  #export the gs_orig to xml
  outFile <- tempfile(fileext = ".wlx")
  expect_warning(GatingSet2winlist(gs, outFile))
  #read the exported gatingML back in
  # con <- file("/dev/null", "r")      avoid /dev/null for windows
  tmp <- tempfile()
  con <- file(tmp, "w")
  #mute the error message printed to stderr() by flowUtils
  sink(con, type = "message")
  gs1 <- winlist2GatingSet(outFile, fcsFiles)
  sink(NULL, type = "message")
  close(con)
  stats.orig <- getPopStats(gs)
  stats.new <- getPopStats(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y])

})