context("gatingset_to_cytobank ..")

test_that("flowJo to cytobank",{
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  #rm negated gate since we have some issue when load the inversed cytobank gate back in
  gh_pop_move(gs, "singlets", "root")
  gs_pop_remove("not debris", gs = gs)

  outFile <- tempfile(fileext = ".xml")
  expect_warning(gatingset_to_cytobank(gs, outFile))

  fcsFiles <<- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
  tmp <- tempfile()
  con <- file(tmp, "w")
  sink(con, type = "message")
  gs1 <- cytobank_to_gatingset(outFile, fcsFiles)
  sink(NULL, type = "message")


  stats.orig <- gs_pop_get_count_fast(gs)
  stats.new <- gs_pop_get_count_fast(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y], tol = 2e-3)

})


test_that("gatingML-cytobank parsing: cytotrol tcell",{
  acsfile <- system.file("extdata/cytobank_experiment.acs", package = "CytoML")
  ce <- open_cytobank_experiment(acsfile)
  xmlfile <- ce$gatingML
  fcsFiles <- list.files(ce$fcsdir, full.names = TRUE)
  gs <<- cytobank_to_gatingset(xmlfile, fcsFiles)


  #' ## verify the stats are correct
  statsfile <- ce$attachments[1]
  dt_merged <- gs_compare_cytobank_counts(gs, statsfile, id.vars = "population", skip = "FCS Filename")
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)
  expect_equal(names(pData(gs)), c("name"))
  expect_equal(markernames(gs)[c(1,4)], c("CD4 PcpCy55", "CD3 V450"))
  
  #parse from ce
  gs <- cytobank_to_gatingset(ce)
  dt_merged <- gs_compare_cytobank_counts(gs, statsfile, id.vars = "population", skip = "FCS Filename")
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)
  expect_equal(names(pData(gs)), c("name", "Conditions", "Individuals"))
  expect_equal(markernames(gs)[c(1,4)], c("CD4", "CD3"))
})

test_that("gatingML-cytobank parsing: cytotrol tcell--logtGml",{
  xmlfile <- system.file("extdata/logtGml.xml", package = "CytoML")

  gs1 <- cytobank_to_gatingset(xmlfile, fcsFiles)

  #' ## verify the stats are correct
  statsfile <- system.file("extdata/cytotrol_tcell_cytobank_logt_counts.csv", package = "CytoML")
  dt_merged <- gs_compare_cytobank_counts(gs1, statsfile, id.vars = "population", skip = "FCS Filename")

  expect_equal(nrow(dt_merged), 5)
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)



})

test_that("gatingML-cytobank exporting: cytotrol tcell",{

  #export the gs_orig to xml
  outFile <- tempfile(fileext = ".xml")
  expect_warning(gatingset_to_cytobank(gs, outFile))
  #read the exported gatingML back in
  # con <- file("/dev/null", "r")      avoid /dev/null for windows
  tmp <- tempfile()
  con <- file(tmp, "w")
  #mute the error message printed to stderr() by flowUtils
  sink(con, type = "message")
  gs1 <- cytobank_to_gatingset(outFile, fcsFiles)
  sink(NULL, type = "message")

  stats.orig <- gs_pop_get_count_fast(gs)
  stats.new <- gs_pop_get_count_fast(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y])

  #enable custom scale
  gatingset_to_cytobank(gs, outFile, cytobank.default.scale =F)

  sink(con, type = "message")
  gs1 <- cytobank_to_gatingset(outFile, fcsFiles)
  sink(NULL, type = "message")
  stats.new <- gs_pop_get_count_fast(gs1)
  stats <- merge(stats.orig, stats.new, by = c("name", "Population", "Parent"))

  expect_equal(stats[, Count.x/ParentCount.x], stats[, Count.y/ParentCount.y])

  close(con)
})

test_that("autogating to cytobank--tcell", {
  set.seed(1)
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
  stats.orig <- gh_pop_compare_stats(gs[[1]])[order(node), list(openCyto.count, node)]
  #output to cytobank

  gatingset_to_cytobank(gs, outFile, cytobank.default.scale = F)
  #parse it back in
  gs1 <- cytobank_to_gatingset(outFile, file.path(dataDir, "CytoTrol_CytoTrol_1.fcs"))
  stats.new <- gh_pop_compare_stats(gs1[[1]])[order(node), list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 6e-4)

})
