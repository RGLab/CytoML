context("gatingset_to_cytobank ..")


test_that("multipanel",{
  acsfile <- "~/rglab/workspace/CytoML/wsTestSuite/gatingML/multipanel/experiment_36089_Dec-05-2020_08-49-PM.acs"
  skip_if_not(file.exists(acsfile))
  ce <- open_cytobank_experiment(acsfile)
  pn <- "Panel 2"
  expect_equal(length(get_panel_per_file(ce)), 3)
  expect_equal(length(get_panel_per_file(ce, pn)), 1)
  
  expect_error(ce_get_channels(ce), "not consistent")
  expect_equal(length(ce_get_channels(ce, pn)), 16)
  
  expect_warning(length(ce_get_markers(ce)), "not consistent")
  expect_equal(length(ce_get_markers(ce, pn)), 1)
  
  expect_error(length(ce_get_transformations(ce)), "not consistent")
  expect_equal(length(ce_get_transformations(ce, pn)), 7)
  
  gs <- cytobank_to_gatingset(ce, panel_id = 2)
  
  expect_is(gs, "GatingSet")
})

test_that("transform ungated channel",{
  acsfile <- "~/rglab/workspace/CytoML/wsTestSuite/gatingML/experiment_34218_Feb-16-2020_08-33-PM.acs"
  skip_if_not(file.exists(acsfile))
  ce <- open_cytobank_experiment(acsfile)
  gs <- cytobank_to_gatingset(ce)
  #check the uneven tags info in yaml
  dose <- pData(gs)[["Doses"]]
  expect_equal(dose[1], "NA")
  expect_equal(dose[4], "0_10")
  
  #check the ungated channel is transformed properly by scales specified in yaml
  #input as fcs name

  expect_equivalent(unlist(range(gs_cyto_data(gs)[[1, "Alexa 647-A"]])), c(-0.685, 8.159), tol = 2e-3)
  #gated channel is also at right scale
  expect_equivalent(unlist(range(gs_cyto_data(gs)[[1, "Alexa Fluor 700-A"]])), c(-0.111, 6.262), tol = 2e-3)
  #check gating result
  expect_equal(gh_pop_get_stats(gs[[2]], "Live")[, count], 3203)
  })

#this test case is somehow very slow which could be due to the big xml with complex gating schemes
#profvis shows the bottleneck is xml parsing, which calls for the efficient xml operations at c++
test_that("tailored gate -- lookup by file_id",{
  path <- "~/rglab/workspace/CytoML/wsTestSuite/gatingML/tailor_gate"
  skip_if_not(dir.exists(path))
  xmlfile <- file.path(path, "cytobank_gate_ml2_v10.xml")
  
  #input as file id
  fcsFiles <- file.path(path, "652774")
  gs <- cytobank_to_gatingset(xmlfile, fcsFiles)
  stats1 <- gh_pop_get_proportion(gs[[1]], "Singlets (Cells)")
  expect_equal(stats1, 0.830202)
  
  #input as fcs name
  fcsFiles <- file.path(path, "c01_ObserveHBP0002_20190704_HELIOS1_RUN1 _02_0_BE904259 0010.fcs")
  gs <- cytobank_to_gatingset(xmlfile, fcsFiles)
  stats2 <- gh_pop_get_proportion(gs[[1]], "Singlets (Cells)")
  expect_equal(stats2, 0.830202)
})

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
  expect_equal(markernames(gs)[c(1,4)], c("B710-A"="CD4 PcpCy55", "V450-A"="CD3 V450"))
  
  #parse from ce
  gs <- cytobank_to_gatingset(ce)
  dt_merged <- gs_compare_cytobank_counts(gs, statsfile, id.vars = "population", skip = "FCS Filename")
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)
  expect_setequal(names(pData(gs)), c("name", "Conditions", "Individuals"))
  expect_equal(markernames(gs)[c(1,4)], c("B710-A"="CD4", "V450-A"="CD3"))
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
  suppressWarnings(gatingset_to_cytobank(gs, outFile, cytobank.default.scale =F))

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
  fs <- load_cytoset_from_fcs(file.path(dataDir,"CytoTrol_CytoTrol_1.fcs"))
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
  gt_toggle_helpergates(gt, gs) #hide the helper gates
  stats.orig <- gh_pop_compare_stats(gs[[1]])[order(node), list(openCyto.count, node)]
  #output to cytobank

  suppressWarnings(gatingset_to_cytobank(gs, outFile, cytobank.default.scale = F))
  #parse it back in
  suppressWarnings(gs1 <- cytobank_to_gatingset(outFile, file.path(dataDir, "CytoTrol_CytoTrol_1.fcs")))
  stats.new <- gh_pop_compare_stats(gs1[[1]])[order(node), list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 6e-3)

})
