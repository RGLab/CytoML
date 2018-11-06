library(data.table)
context("diva parser ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/diva"

test_that("diva--escape forward slash ",{
  ws <- openDiva(file.path(path, "Leggat_VRC/181019_AC03_DL/181019_AC03_DL.xml"))
  gs <- parseWorkspace(ws, name = 2, worksheet = "global", subset = c("G00100001_V05_01_001.fcs"))
  stats <- getPopStats(gs[[1]])[, openCyto.freq]
  # paste(round(stats, 2), collapse = ",")
  expect_equal(stats, c(1,0.45,1,0.63,0.34,0.16,0.17,1,0.16,0.33,0.79,0.02,0), tol = 5.3e-3)
})

test_that("diva--global sheet ",{
  ws <- openDiva(file.path(path, "181030_AD01_RN/181030_AD01_RN.xml"))
  # gs1 <- parseWorkspace(ws, name = 3, subset = "G00159008_V05_01_003.fcs")
  
  gs2 <- parseWorkspace(ws, name = 3, subset = "G00159008_V05_01_003.fcs", worksheet = "global")
  expect_equal(getPopStats(gs2[[1]])[, openCyto.freq], c(1,0.67,0.99,0.27,0.86,0.18,0.53,1,0.18,0.18,0.72,0.7,0.04,0.99), tol = 5e-3)
})
test_that("diva--swap ",{
  #This experiment exported by diva has the FCS swapped column for -W and -H
  ws <- openDiva(file.path(path, "exampleExp/exampleExp.xml"))
  gs <- parseWorkspace(ws, name = 1, worksheet = "global")
  parsedStats <- getPopStats(gs[[1]])
  expect_equal(parsedStats[,openCyto.freq], parsedStats[,xml.freq], tol = 4e-3)
  expect_equal(getNodes(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4', '/P1/P2/P3/P4/P5'))
  
  #if the FCS was exported separately, then we don't need to swap the data thus disable swapping 
  gs <- parseWorkspace(ws, name = 1, worksheet = "global"
                       , path = file.path(path, "exampleExp_export_as_fcs")
                       , swap_cols = FALSE)
  parsedStats <- getPopStats(gs[[1]])
  expect_equal(parsedStats[,openCyto.freq], parsedStats[,xml.freq], tol = 4e-3)
  expect_equal(getNodes(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4', '/P1/P2/P3/P4/P5'))
  
})

test_that("diva--tcell ",{
  ws <- openDiva(file.path(path, "tcell/tcell.xml"))
  gs <- parseWorkspace(ws, name = 1)
  parsedStats <- getPopStats(gs[[1]])
  expect_equal(parsedStats[,openCyto.freq], parsedStats[,xml.freq], tol = 5e-3)
  expect_equal(getNodes(gs), c('root','/L','/L/P2','/L/P2/cd3','/L/P2/cd3/cd8','/L/P2/cd3/cd4'))
  
  #parse global worksheet
  gs <- parseWorkspace(ws, name = 1, worksheet = "global")
  expect_equal(getNodes(gs), c('root','/s','/s/s2'))
  })

test_that("diva--2002-D-g001 ",{
  ws <- openDiva(file.path(path, "2002-D-g001/2002-D-g001.xml"))
  gs <- parseWorkspace(ws, name = 2, subset = "SO1_Naive Sort_003.fcs")
  parsedStats <- getPopStats(gs[[1]])
  expectRes <- fread(file.path(path, "2002-D-g001/2002-D-g001.csv"), skip = "Population")
  setnames(expectRes, c("Population", "%Parent"), c("node", "openCyto.freq"))
  expectRes[, openCyto.freq := openCyto.freq / 100]
  parsedStats[, openCyto.freq := round(openCyto.freq, digits = 3)]

  expect_equal(parsedStats[-1,list(node, openCyto.freq)], expectRes[-1,list(node, openCyto.freq)], tol = 1.09e-3)

})
