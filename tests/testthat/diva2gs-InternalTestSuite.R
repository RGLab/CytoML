library(data.table)
context("diva parser ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/diva"

test_that("diva--escape forward slash ",{
  ws <- open_diva_xml(file.path(path, "Leggat_VRC/181019_AC03_DL/181019_AC03_DL.xml"))
  gs <- diva_to_gatingset(ws, name = 2, worksheet = "global", subset = c("G00100001_V05_01_001.fcs")
                       # , which.lines = 7e3
                       )
  stats <- gh_pop_compare_stats(gs[[1]])[, openCyto.freq]
  # paste(round(stats, 2), collapse = ",")
  expect_equal(stats, c(1,0.45,1,0.63,0.34,0.16,0.17,1,0.16,0.33,0.79,0.02,0), tol = 5.3e-3)
})


test_that("diva--global sheet ",{
  ws <- open_diva_xml(file.path(path, "181030_AD01_RN/181030_AD01_RN.xml"))
  set.seed(1)
  gs <- diva_to_gatingset(ws, name = 3, subset = "G00159008_V05_01_003.fcs", worksheet = "global"
                        , which.lines = 1e3)#speed up this test case by subsetting data  due to the large data
  stats <- gh_pop_compare_stats(gs[[1]])[, openCyto.freq]
  expect_equal(stats, c(1,0.67,0.99,0.27,0.86,0.18,0.53,1,0.18,0.18,0.72,0.7,0.04,0.99), tol = 6e-2)
})
test_that("diva--swap ",{
  #This experiment exported by diva has the FCS swapped column for -W and -H
  ws <- open_diva_xml(file.path(path, "exampleExp/exampleExp.xml"))
  gs <- diva_to_gatingset(ws, name = 1, worksheet = "global")
  stats <- gh_pop_compare_stats(gs[[1]])[, openCyto.freq]
  expect_equal(stats, c(1,0.69,0.94,0.97,0.92,0), tol = 5e-3)
  expect_equal(gs_get_pop_paths(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4', '/P1/P2/P3/P4/P5'))
  fr <- gh_pop_get_data(gs[[1]])
  #ensure swap col properly for both range and data
  expect_equal(range(fr, "data")[,c("SSC-H", "SSC-W")] , structure(list("SSC-H" = c(0.2675822, 1.0369415)
                                                                        , "SSC-W" = c(0, 262143))
                                                                   , class = "data.frame"
                                                                   , row.names = c("min", "max")
  ), tol = 4e-3)
  expect_equal(range(fr)[,c("SSC-H", "SSC-W")] , structure(list("SSC-H" = c(0, 1.20412)
                                                                        , "SSC-W" = c(0, 262143))
                                                                   , class = "data.frame"
                                                                   , row.names = c("min", "max")
  ), tol = 4e-7)
  #if the FCS was exported separately, then we don't need to swap the data thus disable swapping 
  gs <- diva_to_gatingset(ws, name = 1, worksheet = "global"
                       , path = file.path(path, "exampleExp_export_as_fcs")
                       , swap_cols = FALSE)
  stats <- gh_pop_compare_stats(gs[[1]])[, openCyto.freq]
  expect_equal(stats, c(1,0.69,0.94,0.97,0.92,0), tol = 5e-3)
  expect_equal(gs_get_pop_paths(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4', '/P1/P2/P3/P4/P5'))
  
})

test_that("diva--tcell ",{
  ws <- open_diva_xml(file.path(path, "tcell/tcell.xml"))
  gs <- diva_to_gatingset(ws, name = 1)
  parsedStats <- gh_pop_compare_stats(gs[[1]])
  expect_equal(parsedStats[,openCyto.freq], parsedStats[,xml.freq], tol = 5e-3)
  expect_equal(gs_get_pop_paths(gs, path = "auto"), c('root','L','P2','cd3','cd8','cd4', 'P1', "cd4 & P1", "cd8 | cd4", "not cd4"))
  
  #parse global worksheet
  gs <- diva_to_gatingset(ws, name = 1, worksheet = "global")
  expect_equal(gs_get_pop_paths(gs), c('root','/s','/s/s2','/s/s2/P1','/s/s2/P2'))
  parsedStats <- gh_pop_compare_stats(gs[[1]])
  expect_equal(parsedStats[,openCyto.freq], c(1,.73,.757,.305,.569), tol = 2e-2)
  
  })

test_that("diva--2002-D-g001 ",{
  ws <- open_diva_xml(file.path(path, "2002-D-g001/2002-D-g001.xml"))
  gs <- diva_to_gatingset(ws, name = 2, subset = "SO1_Naive Sort_003.fcs",which.lines = 1e4)
  parsedStats <- gh_pop_compare_stats(gs[[1]])
  expectRes <- fread(file.path(path, "2002-D-g001/2002-D-g001.csv"), skip = "Population")
  setnames(expectRes, c("Population", "%Parent"), c("node", "openCyto.freq"))
  expectRes[, openCyto.freq := openCyto.freq / 100]
  parsedStats[, openCyto.freq := round(openCyto.freq, digits = 3)]

  expect_equal(parsedStats[-1,list(node, openCyto.freq)], expectRes[-1,list(node, openCyto.freq)], tol = 6e-3)

})
