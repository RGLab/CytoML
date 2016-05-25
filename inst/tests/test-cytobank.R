
test_that("gatingML-cytobank parsing: cytotrol tcell",{
  xmlfile <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "CytoML")
  fcsFiles <<- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
  gs <<- parse.gatingML(xmlfile, fcsFiles)


  #' ## verify the stats are correct
  statsfile <- system.file("extdata/cytotrol_tcell_cytobank_counts.csv", package = "CytoML")
  dt_merged <- compare.counts(gs, statsfile, id.vars = "population")


  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)



})
test_that("gatingML-cytobank exporting: cytotrol tcell",{

  #export the gs_orig to xml
  outFile <- tempfile(fileext = ".xml")
  expect_warning(GatingSet2GatingML(gs, outFile))
  #read the exported gatingML back in
  gs1 <- parse.gatingML(outFile, fcsFiles)
  stats.orig <- getPopStats(gs)
  stats.new <- getPopStats(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x], stats[, Count.y])

})