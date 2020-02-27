context("gatingset_to_cytobank and GatingSet2Cytobank ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/gatingML"

test_that("read.gatingML.cytobank: gates drawn on both compensated and uncompensated axes",{
  gm <- CytoML:::read.gatingML.cytobank(file.path(path, "CytExp_29783_Gates_v5.xml"))
  
  
  expect_equal(length(getNodes(gm)), 10)
})

test_that("cytobank2gs--cytof: special character in channels and no comp ",{

  thisPath <- file.path(path, "43281")
  gating_ml_cytobank_file = list.files(path = thisPath, pattern="gate.*xml", recursive=TRUE,full=TRUE)
  fcs_files = list.files(path = file.path(thisPath,"fcs_files"), recursive=TRUE,full=TRUE)
  gs <- cytobank_to_gatingset(gating_ml_cytobank_file, FCS = fcs_files[1])
  expect_is(gs, "GatingSet")
  expect_equal(gs_get_pop_paths(gs, path = "auto")[-1], c('H+R+','MEFSK4+','CD54+','Nanog+','Lin28+'))
})

test_that("GatingSet2Cytobank--cytof ",{

  thisPath <- file.path(path, "cytof")
  #load the original automated gating set
  gs_orig <- load_gs(file.path(thisPath, "gs"))
  stats <- gs_pop_get_count_fast(gs_orig)[order(Population),]

  #export the gs_orig to xml
  outFile <- tempfile(fileext = ".xml")
  expect_warning(gatingset_to_cytobank(gs_orig, outFile))

  #parse the exported xml
  fcs <- file.path(path, "cytof/186089_Env_1-concat_normalized.fcs")
  #mute the error message printed to stderr() by flowUtils
  con <- file("/dev/null", "r")
  sink(con, type = "message")
  capture.output(gs_parsed <- cytobank_to_gatingset(outFile, fcs))
  sink(NULL, type = "message")
  close(con)

  parsedStats <- gs_pop_get_count_fast(gs_parsed)[order(Population),]
  expect_equal(stats, parsedStats, tol = 1e-3)

  #upload xml to cytobank and download the cytobank version of xml

  # #parse the cytobank xml
  # xmlfile <- file.path(path, "ics/CytExp_52926_Gates_v5.xml")
  # gs_parsed <- cytobank_to_gatingset(xmlfile, fcs)
  #
  # parsedStats <- gs_pop_get_count_fast(gs_parsed)[order(Population),]
  # expect_equal(stats, parsedStats, tol = 2e-4)

})


test_that("gatingML-cytobank parsing: custom comp and gates with prefixed channels ",{
      thisPath <- file.path(path, "ics")
      #load the original automated gating set
      gs_orig <- load_gs(file.path(thisPath, "autogating"))
      gt <- openCyto::gatingTemplate(file.path(thisPath, "template/gt_080.csv"))
	  gt_toggle_helpergates(gt, gs_orig) #hide the helper gates
      stats <- gs_pop_get_count_fast(gs_orig)[order(Population),]

      #export the gs_orig to xml
      tmp <- tempfile()
      gatingset_to_cytobank(gs_orig, tmp)

      #parse the exported xml
      fcs <- file.path(path, "ics/769121.fcs")
      #mute the error message printed to stderr() by flowUtils
      con <- file("/dev/null", "r")
      sink(con, type = "message")
      gs_parsed <- cytobank_to_gatingset(tmp, fcs)
      sink(NULL, type = "message")
      close(con)

      parsedStats <- gs_pop_get_count_fast(gs_parsed)[order(Population),]
      expect_equal(stats, parsedStats, tol = 2e-4)

      #upload xml to cytobank and download the cytobank version of xml

      #parse the cytobank xml
      xmlfile <- file.path(path, "ics/CytExp_52926_Gates_v5.xml")
      gs_parsed <- cytobank_to_gatingset(xmlfile, fcs)

      parsedStats <- gs_pop_get_count_fast(gs_parsed)[order(Population),]
      expect_equal(stats, parsedStats, tol = 2e-4)

    })


test_that("gatingML-cytobank parsing: no transformations",{
  xmlfile <- file.path(path, "no_trans.xml")
  g <- CytoML:::read.gatingML.cytobank(xmlfile)
  expect_is(g, "graphGML")
  expect_null(CytoML:::getTransformations.graphGML(g))
})


test_that("gatingML-cytobank parsing: Merck FirstExample",{
  thisPath <- file.path(path, "Merck/firstExample")
  xmlfile <- file.path(thisPath, "CytExp_10623_Gates_v5.xml")
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)
  gs <- cytobank_to_gatingset(xmlfile, fcsFiles[c(1,3,6)])

  ### Verify the stats are correct
  statsfile <- file.path(thisPath,"population_counts.csv")
  dt_merged <- gs_compare_cytobank_counts(gs, statsfile, skip = "FCS Filename")

  expect_equal(dt_merged[,count.y], dt_merged[,count.x], tol = 5e-5)
})

test_that("gatingML-cytobank parsing: Merck SecondExample",{
  thisPath <- file.path(path, "/Merck/SecondExample")
  xmlfile <- file.path(thisPath, "CytExp_10624_Gates_v3.xml")
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)

  gs <- cytobank_to_gatingset(xmlfile, fcsFiles[c(1,4,6,12)])

  ### Verify the stats are correct
  statsfile <- file.path(thisPath,"secondExample.csv")
  dt_merged <- gs_compare_cytobank_counts(gs, statsfile, id.vars = "population", skip = "FCS Filename") #subset the files to speed up testing


  expect_equal(dt_merged[,count.y], dt_merged[,count.x], tol = 5e-5)
})
