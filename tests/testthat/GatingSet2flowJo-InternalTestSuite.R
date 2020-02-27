context("Exporting GatingSet to flowJo workspace")

path <- "~/rglab/workspace/CytoML//wsTestSuite"

test_that("gatingset_to_flowjo: forward slash ",{
  thisPath <- file.path(path, "slash_issue_vX")
  gs <- load_gs(file.path(thisPath, "gs"))

  stats.orig <- gh_pop_compare_stats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath, additional.keys = NULL)
  stats.new <- gh_pop_compare_stats(gs1[[1]])
  expect_equal(stats.orig[,openCyto.freq], stats.new[,openCyto.freq], tol = 6e-5)
})

test_that("OrNode ",{
  thisPath <- file.path(path, "combineNode/OrNode")
  wsFile <- file.path(thisPath, "Test_EW.wsp")
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile, sampNloc = "sampleNode")
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})


test_that("Time gate ",{
  thisPath <- file.path(path, "flin")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1)
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
 #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile, sampNloc = "sampleNode")
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})

test_that("Time gate2--when computed timestep is very different from $TIMESTEP ",{
  thisPath <- file.path(path, "timegate")
  wsFile <- file.path(thisPath, "MX1 Analysis VISC.xml")
  ws <- open_flowjo_xml(wsFile)
  capture.output(gs <- flowjo_to_gatingset(ws,name=1))
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile, sampNloc = "sampleNode")
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new, tol = 4e-5)
})
test_that("EllipsoidGate defined on log-transformed channels ",{
  thisPath <- file.path(path, "ellipsoid_log")
  wsFile <- file.path(thisPath, "xml_spillover2.xml")
  ws <- open_flowjo_xml(wsFile, sampNloc = "sampleNode")
  capture.output(gs <- flowjo_to_gatingset(ws, name=1, execute = T, subset = "spillover_B2.fcs"))

  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile, sampNloc = "sampleNode")
  capture.output(gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath))
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(stats.orig, stats.new)
})
test_that("GatingSet2flowJo: rectangleGate + boolgate",{
  dataDir <- file.path(path, "curlyQuad/example1")
  ws <- open_flowjo_xml(file.path(dataDir, "20151208_TBNK_DS.xml"))
  gs <- flowjo_to_gatingset(ws, subset = 1, name = 2)

  stats.orig <- gh_pop_compare_stats(gs[[1]])
  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #load it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = dataDir)
  stats.new <- gh_pop_compare_stats(gs1[[1]])
  expect_equal(stats.orig, stats.new, tol = 3e-3)
})


test_that("gatingset_to_flowjo: no comp + fasinh ",{
  thisPath <- file.path(path, "PROVIDE")
  wsFile <- file.path(thisPath, "count_corrected.wsp")

  ws <- open_flowjo_xml(wsFile, sampNloc = "sampleNode")
  dd <- capture.output(gs <- flowjo_to_gatingset(ws, name = 1, subset = 3, additional.keys = NULL))
  stats.orig <- gh_pop_compare_stats(gs[[1]])[order(node), list(node, openCyto.count)]

  #output to flowJo
  outFile <- tempfile(fileext = ".wsp")
  gatingset_to_flowjo(gs, outFile)
  # cross_validate(gs, outFile)#can't cross validate due to the order change from ggcyto:::merge.quad.gates call and it is not worth to implement the same ordering logic as c++
  
  #parse it back in
  ws <- open_flowjo_xml(outFile, sampNloc = "sampleNode")
  capture.output(gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath, additional.keys = NULL))
  stats.new <- gh_pop_compare_stats(gs1[[1]])[order(node), list(node, openCyto.count)]
  expect_equal(stats.orig, stats.new)
})


test_that("gatingset_to_flowjo: automated gates+hidden gate + Infinity + boolean gate",{
  thisPath <- file.path(path, "gatingML/ics")
  #load the original automated gating set
  gs <- load_gs(file.path(thisPath, "autogating"))
  gt <- openCyto::gatingTemplate(file.path(thisPath, "template/gt_080.csv"))
  gt_toggle_helpergates(gt, gs) #hide the helper gates

  #add AND gate
  bf <- booleanFilter(cd4/GzB&cd4/Prf)
  gs_pop_add(gs, bf, name = "bool1", parent = "cd4")
  #Or gate
  bf <- booleanFilter(cd4/GzB|cd4/Prf)
  gs_pop_add(gs, bf, name = "bool2", parent = "cd4")
  #Not gate
  bf <- booleanFilter(!cd4/GzB)
  gs_pop_add(gs, bf, name = "bool3", parent = "cd4")
  #combine & and !
  bf <- booleanFilter(cd4/GzB&!cd4/Prf)
  gs_pop_add(gs, bf, name = "bool4", parent = "cd4")
  #combine & and !
  bf <- booleanFilter(cd4/GzB|!cd4/Prf)
  gs_pop_add(gs, bf, name = "bool5", parent = "cd4")
  recompute(gs)
  #mixture of & , |
  bf <- booleanFilter(cd4/GzB|cd4/Prf&cd4/IFNg)
  gs_pop_add(gs, bf, name = "bool6", parent = "cd4")
  recompute(gs)
  outFile <- tempfile(fileext = ".wsp")
  expect_error(gatingset_to_flowjo(gs, outFile), "And gate and Or gate can't not be used together!")
  gs_pop_remove(gs, "bool6")

  # autoplot(gs[[1]], getChildren(gs[[1]], "cd4"))
  # getTotal(gs[[1]], "bool5")
  # plotGate(gs, "bool4", bool = T)
  stats.orig <- gh_pop_compare_stats(gs[[1]])[, list(openCyto.count, node)]
  #output to flowJo
  gatingset_to_flowjo(gs, outFile)
  cross_validate(gs, outFile)
  
  #parse it back in
  ws <- open_flowjo_xml(outFile)
  gs1 <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  stats.new <- gh_pop_compare_stats(gs1[[1]])[, list(openCyto.count, node)]
  expect_equal(gh_pop_get_stats(gs1[[1]], "Prf-")[[2]], 90423)
  expect_equal(stats.orig, stats.new[-26,], tol = 1.6e-4)
})

