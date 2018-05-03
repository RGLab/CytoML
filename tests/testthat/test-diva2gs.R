context("parse diva workspace ..")

test_that("PE_2", {
  ws <- openDiva(system.file('extdata/diva/PE_2.xml', package = "flowWorkspaceData"))
  gs <- parseWorkspace(ws, name = 2, subset = 1)
  stats <- getPopStats(gs[[1]])

  expect_equal(stats[, xml.count], stats[, openCyto.count], tolerance = 0.0018)

  sg <- getSampleGroups(ws)
  expect_is(sg, "data.frame")
  expect_equal(sg[["tube"]], c('Unstained Control','FITC Stained Control','PE Stained Control','PerCP-Cy5-5 Stained Control','PE-Cy7 Stained Control','APC Stained Control','APC-Cy7 Stained Control','Bd Horizon V450 Stained Control','Pacific Orange Stained Control','_001','_002','_003','_004'))
  expect_equal(sg[["name"]], c('124480.fcs','124483.fcs','124485.fcs','124487.fcs','124489.fcs','124491.fcs','124493.fcs','124495.fcs','124497.fcs','124500.fcs','124502.fcs','124504.fcs','124506.fcs'))
  expect_equal(unique(sg[["specimen"]]), c("Compensation Controls", "PE"))
  paste(sg[["sampleName"]], collapse = "','")
  # getSamples(ws)

  expect_equal(getNodes(gs), c('root','/P1','/P1/P2','/P1/P2/P3','/P1/P2/P3/P4','/P1/P2/P3/P4/P5'))

})

