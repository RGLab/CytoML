test_that("flowJo to cytobank",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  #rm negated gate since we have some issue when load the inversed cytobank gate back in
  moveNode(gs, "singlets", "root")
  Rm("not debris", gs)

  outFile <- tempfile(fileext = ".xml")
  expect_warning(GatingSet2cytobank(gs, outFile))

  fcsFiles <<- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
  tmp <- tempfile()
  con <- file(tmp, "w")
  sink(con, type = "message")
  gs1 <- cytobank2GatingSet(outFile, fcsFiles)
  sink(NULL, type = "message")


  stats.orig <- getPopStats(gs)
  stats.new <- getPopStats(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y], tol = 2e-3)

})


test_that("gatingML-cytobank parsing: cytotrol tcell",{
  xmlfile <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "CytoML")

  gs <<- cytobank2GatingSet(xmlfile, fcsFiles)


  #' ## verify the stats are correct
  statsfile <- system.file("extdata/cytotrol_tcell_cytobank_counts.csv", package = "CytoML")
  dt_merged <- compare.counts(gs, statsfile, id.vars = "population", skip = "FCS Filename")


  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)



})

test_that("gatingML-cytobank parsing: cytotrol tcell--logtGml",{
  xmlfile <- system.file("extdata/logtGml.xml", package = "CytoML")

  gs1 <- cytobank2GatingSet(xmlfile, fcsFiles)

  #' ## verify the stats are correct
  statsfile <- system.file("extdata/cytotrol_tcell_cytobank_logt_counts.csv", package = "CytoML")
  dt_merged <- compare.counts(gs1, statsfile, id.vars = "population", skip = "FCS Filename")

  expect_equal(nrow(dt_merged), 5)
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)



})

test_that("gatingML-cytobank exporting: cytotrol tcell",{

  #export the gs_orig to xml
  outFile <- tempfile(fileext = ".xml")
  expect_warning(GatingSet2cytobank(gs, outFile))
  #read the exported gatingML back in
  # con <- file("/dev/null", "r")      avoid /dev/null for windows
  tmp <- tempfile()
  con <- file(tmp, "w")
  #mute the error message printed to stderr() by flowUtils
  sink(con, type = "message")
  gs1 <- cytobank2GatingSet(outFile, fcsFiles)
  sink(NULL, type = "message")

  stats.orig <- getPopStats(gs)
  stats.new <- getPopStats(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y])

  #enable custom scale
  GatingSet2cytobank(gs, outFile, cytobank.default.scale =F)

  sink(con, type = "message")
  gs1 <- cytobank2GatingSet(outFile, fcsFiles)
  sink(NULL, type = "message")
  stats.new <- getPopStats(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y])

  close(con)
})

test_that("autogating to cytobank--tcell", {
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  #load raw FCS
  fs <- read.flowSet(file.path(dataDir,"CytoTrol_CytoTrol_1.fcs"))
  outFile <- tempfile(fileext = ".xml")


  #compensate
  comp <- spillover(fs[[1]])[["SPILL"]]
  chnls <- colnames(comp)
  comp <- compensation(comp)


  #run auto gating
  gtFile.orig <- system.file("extdata/gating_template/tcell.csv", package = "openCyto")


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
  stats.orig <- getPopStats(gs[[1]])[order(node), list(openCyto.count, node)]
  #output to cytobank

  GatingSet2cytobank(gs, outFile, cytobank.default.scale = F)
  #parse it back in
  gs1 <- cytobank2GatingSet(outFile, file.path(dataDir, "CytoTrol_CytoTrol_1.fcs"))
  stats.new <- getPopStats(gs1[[1]])[order(node), list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 5e-4)

})