context("parse workspaces of various flowJo versions ")
library(data.table)
path <- "~/rglab/workspace/CytoML/wsTestSuite"

test_that("Attribute redefined",{
			skip("only run interactively due to its lengthy output that can't be suppressed")
			wsFile <- file.path(path, "attr_redefined_err.xml")
			expect_error(ws <- open_flowjo_xml(wsFile), "not parsed", class = "error")
			suppressMessages(ws <- open_flowjo_xml(wsFile, options = 1))
			expect_is(ws, "flowjo_workspace")
			
		})

test_that("no gate",{
  
  wsFile <- file.path(path, "no-gate.wsp")
  
  ws <- open_flowjo_xml(wsFile, sampNloc = 'sampleNode')
  expect_equal(nrow(fj_ws_get_samples(ws)), 1)
  gs <- flowjo_to_gatingset(ws, name = 1, path = file.path(path,"Cytotrol/NHLBI/Tcell/"), include_empty_tree = TRUE)
  expect_equal(range(gh_pop_get_data(gs[[1]]))[,5], c(50.70029, 256.91464), tol = 1e-6)
  
  expect_equal(length(gs_get_pop_paths(gs)), 1)
  
  
})
test_that("flog-- offset and decades that expose the previous logGml2-based flog was wrong",{
  
  wsFile <- file.path(path, "flog/log.wsp")
  
  ws <- open_flowjo_xml(wsFile, sampNloc = 'sampleNode')
  gs <- flowjo_to_gatingset(ws, name = 1, path = file.path(path,"Cytotrol/NHLBI/Tcell/"))
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 0.01)
  
  
})
test_that("which.lines",{
  
  wsFile <- file.path(path, "flog_PnE/Liver.wsp")
  
  ws <- open_flowjo_xml(wsFile, sampNloc = 'sampleNode')
  set.seed(1)
  gs <- flowjo_to_gatingset(ws, name = 2, which.lines = 5e4)
  expect_equal(nrow(gh_pop_get_data(gs[[1]])), 5e4)
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 0.04)
  
  
})


test_that("derived parameters",{
  
  wsFile <- file.path(path, "derivedparam.wsp")
  
  ws <- open_flowjo_xml(wsFile)
  # flowWorkspace::set_log_level("GatingSet")
  gs <- flowjo_to_gatingset(ws, name = 1, execute = F)
  # set_log_level("none")
  
  expect_equal(gs_get_pop_paths(gs, path = "auto")[-1], c("Lymphocytes", "CD3"))
  
  
})

test_that("set T value properly through PnE instead of PnR for flog transform when FCS data is log scale",{
  
  wsFile <- file.path(path, "flog_PnE/Liver.wsp")
  
  ws <- open_flowjo_xml(wsFile, sampNloc = 'sampleNode')
  gs <- flowjo_to_gatingset(ws, name = 2)
 
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 0.015)
  
  
})


test_that("handle the linear transform with maxRange = 0",{
  
  wsFile <- file.path(path, "faultylinearTransform/FlowJo Test.wsp")
  
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1)
  
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq])
  
  
})

test_that("skip ManuallyIncludedSamples",{
  
  wsFile <- file.path(path, "logicle.wsp")
  
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1, path = system.file("extdata", package = "flowCore"), fcs_file_extension = ".B08")
 
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 0.009)
  
  
})

test_that("skip ManuallyIncludedSamples",{
      
      wsFile <- file.path(path, "ManuallyIncludedSamples.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      expect_true(setequal(subset(fj_ws_get_sample_groups(ws), groupName == "Sample")[["sampleID"]], c(1:5, 21:25)))
      gs <- flowjo_to_gatingset(ws, name = 3, path = file.path(path), execute = FALSE)
      
      expect_is(gs, "GatingSet")
      
    })

test_that("Handle duplicate sample names",{
  wsFile <- file.path(path, "duplicatedSampleID", "Ustekin_G26_sas_IMMPORT2.495809.xml")
  ws <- open_flowjo_xml(wsFile)
  sink(tempfile())
  expect_error(flowjo_to_gatingset(ws, name = 1), "Duplicated", class = "error")
  gs <- suppressWarnings(flowjo_to_gatingset(ws, name = 1, additional.sampleID = TRUE, which.lines = 1))
  sink()
  expect_is(gs, "GatingSet")
})

test_that("search reference node for boolean gate ",{
  thisPath <- file.path(path, "searchRefNode")
  wsFile <- file.path(thisPath, "2583-Y-MAL067-FJ.xml")
  ws <- open_flowjo_xml(wsFile)
  gs <- suppressWarnings(flowjo_to_gatingset(ws, name="Samples", subset = "1379326.fcs"))
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(nrow(res), 235)
  expect_equal(res[-10, xml.freq], res[-10, openCyto.freq], tol = 0.006)
  expect_equal(res[10, openCyto.count], 164)
  
  #skip leaf bool
  gs <- suppressWarnings(flowjo_to_gatingset(ws, name="Samples", subset = "1379326.fcs", leaf.bool = F))
  gh <- gs[[1]]
  leaf.bool <- which(sapply(gs_get_pop_paths(gs), function(node)length(gs_pop_get_children(gh, node))==0&&gh_pop_is_bool_gate(gh,node)))
  res <- gh_pop_compare_stats(gh)
  expect_true(all(is.na(res[leaf.bool,  openCyto.count])))
  expect_equal(res[-leaf.bool, xml.freq], res[-leaf.bool, openCyto.freq], tol = 0.006)
  })

test_that("vertical ellipsoidGate for vX ",{
  thisPath <- file.path(path, "ellipsoid_vertical")
  wsFile <- file.path(thisPath, "20171103.circle.flow.ctl.wsp")
  ws <- open_flowjo_xml(wsFile)
  gs <- suppressWarnings(flowjo_to_gatingset(ws, name=1))
  
  res <- gh_pop_compare_stats(gs[[1]])
  
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 0.02)
})

test_that("skip gains from FCS for vX ",{
  thisPath <- file.path(path, "no_gains_vX")
  wsFile <- file.path(thisPath, "10-Apr-2017.wsp")
  ws <- open_flowjo_xml(wsFile)
 capture.output( gs <- flowjo_to_gatingset(ws, name=2))
 
  res <- gh_pop_compare_stats(gs[[1]])
  
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 1e-3)
})

#somehow the latest Rstudio crashes on this particular test case (but not in R console command)
test_that("gate extension ",{
      thisPath <- file.path(path, "gate_extension")
      wsFile <- file.path(thisPath, "02-15-2013 ICS.xml")
      ws <- open_flowjo_xml(wsFile)
      set.seed(1)
      capture.output(gs <- flowjo_to_gatingset(ws, name=3, which.lines = 5e4))
      
      res <- gh_pop_compare_stats(gs[[1]])[xml.count != -1, ]
      
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 3e-2)
    })


test_that("curlyQuad gate1 ",{
      thisPath <- file.path(path, "gate_extension")
      wsFile <- file.path(thisPath, "VSVG OGH 14OCT15.wsp")
      ws <- open_flowjo_xml(wsFile)
      capture.output(gs <- flowjo_to_gatingset(ws, name=3))
      
      res <- gh_pop_compare_stats(gs[[1]])
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 2e-3)
    })

test_that("curlyQuad gate1 ",{
      thisPath <- file.path(path, "curlyQuad/example1")
      wsFile <- file.path(thisPath, "20151208_TBNK_DS.xml")
      ws <- open_flowjo_xml(wsFile)
      capture.output(gs <- flowjo_to_gatingset(ws, name=2))
     
      res <- gh_pop_compare_stats(gs[[1]])
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 7e-3)
    })

test_that("curlyQuad gate ",{
      thisPath <- file.path(path, "curlyQuad/example2")
      wsFile <- file.path(thisPath, "20-Apr-2016.wsp")
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name=1)
      
      res <- gh_pop_compare_stats(gs[[1]])
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 1.6e-2)
    })

test_that("EllipsoidGate defined on log-transformed channels ",{
      thisPath <- file.path(path, "ellipsoid_log")
      wsFile <- file.path(thisPath, "xml_spillover2.xml")
      ws <- open_flowjo_xml(wsFile, sampNloc = "sampleNode")
      capture.output(gs <- flowjo_to_gatingset(ws, name=1, execute = T, subset = "spillover_B2.fcs"))
       
      res <- gh_pop_compare_stats(gs[[1]])
      expect_equal(res[, xml.count], res[, openCyto.count], tol = 4e-3)
})

test_that("No gate extension ",{
      thisPath <- file.path(path, "negCoordinates")
      wsFile <- file.path(thisPath, "08-Mar-2016.wsp")
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1) #default extend_val = 0 will extend the gate  
      res <- gh_pop_compare_stats(gs[[1]])
      expect_gt(res[11, abs(xml.freq - openCyto.freq)], 0.1)
      
      gs <- flowjo_to_gatingset(ws, name = 1, extend_val = -2e3)#relax the threshold to disable extension
      res <- gh_pop_compare_stats(gs[[1]])
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 4e-3)
    })

test_that("FCS searching path with symlink ",{
  thisPath <- file.path(path, "flin_symlink")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1, subset = 1)
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 9e-4)
})

test_that("Time gate ",{
  thisPath <- file.path(path, "flin")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- open_flowjo_xml(wsFile)
  gs <- flowjo_to_gatingset(ws, name = 1, subset = 1)
  res <- gh_pop_compare_stats(gs[[1]])
  expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 9e-4)
})
test_that("Time gate2--when computed timestep is very different from $TIMESTEP ",{
      thisPath <- file.path(path, "timegate")
      wsFile <- file.path(thisPath, "MX1 Analysis VISC.xml")
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws,name="Group 1",subset=1)
      res <- gh_pop_compare_stats(gs[[1]])[xml.count!=-1,]
      expect_equal(res[, xml.freq], res[, openCyto.freq], tol = 8e-3)
    })
test_that("Inverse function of flog ",{
      thisPath <- file.path(path, "inverse")
      wsFile <- file.path(thisPath, "Small.xml")
      ws <- open_flowjo_xml(wsFile)
      
      capture.output(gs <- flowjo_to_gatingset(ws, name=1, emptyValue=FALSE))
      
      gh <- gs[[1]]
      thisCounts <- gs_pop_get_count_fast(gs, path = "auto")
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      expect_equal(thisCounts, expectCounts)
    
      
      trans <- gh_get_transformations(gh)
      inverse <- gh_get_transformations(gh, inverse = T)
      raw <- c(1e2, 1e3,1e5)
      log <- trans[[1]](raw)
      expect_equal(inverse[[1]](log), raw)
      
    })

test_that("v 10.0.6 - vX 1.8",{
      
      thisPath <- file.path(path, "mssm")
      wsFile <- file.path(thisPath, "CFSP_Analysis14.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = "Bcell", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = "Bcell", subset = 1)
      
      gh <- gs[[1]]
            
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[-1, xml.count], thisCounts[-1, openCyto.count], tol = 3.7e-3)
      
      #test double delimiter issue
      fcsname <- pData(gs)[["name"]]
      fr <- read.FCS(file.path(thisPath, fcsname))
      tmp <- tempdir()
      suppressWarnings(write.FCS(fr, filename = file.path(tmp, fcsname), delimiter = "/"))
      
      expect_error(gs <- flowjo_to_gatingset(ws, name = "Bcell", subset = 1, path = tmp, emptyValue = T)
          , "Empty keyword name", class = "error")#ncdfFlowSet
              
      gs <- flowjo_to_gatingset(ws, name = "Bcell", subset = 1, path = tmp, emptyValue = F)#ncdf
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)            
      expect_equal(thisCounts[-1, xml.count], thisCounts[-1, openCyto.count], tol = 3.7e-3)
      
      # gs <- flowjo_to_gatingset(ws, name = "Bcell", subset = 1, isNcdf = F, path = tmp, emptyValue = F)#flowSet
      # gh <- gs[[1]]
      # thisCounts <- gh_pop_compare_stats(gh)            
      # expect_equal(thisCounts[-1, xml.count], thisCounts[-1, openCyto.count], tol = 3.7e-3)
    })


test_that("v 10.0.7 - vX 20.0 (ellipsoidGate)",{
      
      thisPath <- file.path(path, "bioaster_ellipsoidGate")
      wsFile <- file.path(thisPath, "Matrice 1.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = "Matrice", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = "Matrice", subset = 1)
     
      gh <- gs[[1]]
     
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 2e-2)
    })



test_that("v 10.0.7 - vX 20.0 (missing_namespace and flin)",{
      
      thisPath <- file.path(path, "missing_namespace")
      wsFile <- file.path(thisPath, "BM_data.xml")
      ws <- open_flowjo_xml(wsFile, options = 32)#set option to suppress xml error
      expect_error(gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, execute = FALSE)
                    , "*: unknown tranformation type!transforms:linear", class = "error")
      
      
      wsFile <- file.path(thisPath, "BM_data_corrected.xml")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
      trans <- gh_get_transformations(gh, only = F, channel = "all")
      expect_equal(trans[[2]][["name"]], "flowJo_log")
    })

# invalid xml with Namespace prefix defintion missing #TODO: try to be robust on this kind of xml error
#test_that("v 10.0.7 - vX 20.0 (McGill/BMDCs) linear transformation",{
#      
#      thisPath <- file.path(path, "McGill/BMDCs")
#      wsFile <- file.path(thisPath, "20140124 BMDCs.1.wsp")
#      
#      ws <- open_flowjo_xml(wsFile)
#      gs <- flowjo_to_gatingset(ws, name = 3, subset = 1, execute = FALSE)
#      expect_is(gs, "GatingSet")
#      gs <- flowjo_to_gatingset(ws, name = 3, subset = 1)
#      
#      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#      thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
#      expect_equal(thisCounts, expectCounts)
#    })

test_that("v 10.0.7 - vX 20.0 (McGill/treg) ellipseidGate (biexponential)",{
      
      thisPath <- file.path(path, "McGill/Treg")
      wsFile <- file.path(thisPath, "20131206_Treg.1.ellipseidGate.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 3, subset = 4, execute = FALSE)
      expect_is(gs, "GatingSet")
      g <- gh_pop_get_gate(gs[[1]], "CD4Ellipse")
      #transformed ellipse Gate
      expect_is(g, "polygonGate")
      expect_equal(range(g@boundaries[, "Comp-APC-A"]), c(143.918, 207.082), tol = 1e-6)
      expect_equal(range(g@boundaries[, "SSC-A"]), c(10168.56, 58439.45), tol = 1e-6)
      
      #skip gate transform
      gs <- flowjo_to_gatingset(ws, name = 3, subset = 4, execute = FALSE, transform = FALSE)
      g <- gh_pop_get_gate(gs[[1]], "CD4Ellipse")
      expect_is(g, "polygonGate")
      #ellipsoidGate should be in 256 * 256 scale
      expect_equal(range(g@boundaries[, "Comp-APC-A"]), c(143.918, 207.082), tol = 1e-6)
      expect_equal(range(g@boundaries[, "SSC-A"]), c(9.930231, 57.069771), tol = 1e-6)
      
      gs <- flowjo_to_gatingset(ws, name = 3, subset = 4)
      
      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[,xml.freq], thisCounts[,openCyto.freq], tol = 7e-4)
    })

test_that("v 10.0.7 - vX 20.0 (PROVIDE/CyTOF) ellipseidGate (fasinh)",{
      
      thisPath <- file.path(path, "PROVIDE")
      wsFile <- file.path(thisPath, "batch1 local and week 53.wsp")
      
      ws <- open_flowjo_xml(wsFile, sampNloc = "sampleNode")
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 3, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      #won't find the file if $TOT is taken into account(most likely the data provided was wrong)
      expect_output(expect_error(gs <- flowjo_to_gatingset(ws, name = 1, subset = 3)
                                  , "No samples", class = "error")
               , "FCS")
      
      #relax the rules 
      # UPDATE: After changes to search_for_fcs, this kind of evasion of $TOT check is not possible
      expect_error(capture.output(gs <- flowjo_to_gatingset(ws, name = 1, subset = 3, additional.keys = NULL)), "No samples", class = "error")
      #switch to the corrected wsp
      wsFile <- file.path(thisPath, "count_corrected.wsp")
      
      ws <- open_flowjo_xml(wsFile, sampNloc = "sampleNode")
      capture.output(gs <- flowjo_to_gatingset(ws, name = 1, subset = 3))
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
      expect_equal(thisCounts[, openCyto.count], thisCounts[, xml.count], tol = 0.04)
      
    })

test_that("v 10.0.7 - vX 20.0 (cytof no compensation)",{
      
      thisPath <- file.path(path, "CyTOF")
      wsFile <- file.path(thisPath, "cytof.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      
      expect_warning(gs <- flowjo_to_gatingset(ws, name = 1, path = file.path(path), execute = FALSE), "different gating tree structures")
      
      expect_is(gs, "GatingSet")
#      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#      thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
#      expect_equal(thisCounts, expectCounts)
    })

test_that("v 10.0.7r2 - vX 20.0 (NotNode)",{
      
      thisPath <- file.path(path, "combineNode/NotNode")
      wsFile <- file.path(thisPath, "WSwithNotNodePopulation.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      
      gs <- flowjo_to_gatingset(ws, name = 1, path = file.path(path), execute = FALSE)
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
      g <- gh_pop_get_gate(gh, "CD20+⁻")
      expect_is(g, "booleanFilter")
      expect_equal(g@deparse, "!LIVE/Single Cells/CD45+/CD20+")
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
      expect_equivalent(thisCounts[,c("xml.count", "node"), with = F], expectCounts[,c("flowJo.count", "node"), with = F])
    })

test_that("v 10.2 - vX 20.0 (AndNode)",{
      
      thisPath <- file.path(path, "combineNode/AndNode")
      wsFile <- file.path(thisPath, "test_gates.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      
      gs <- flowjo_to_gatingset(ws, name = 1, path = file.path(path), execute = FALSE)
      
      expect_is(gs, "GatingSet")
      
      gh <- gs[[1]]
      g <- gh_pop_get_gate(gh, "CD4/CD107+IFNg+IL2+Mip1b+TNF+")
      expect_is(g, "booleanFilter")
      expect_equal(g@deparse, "Lymphocytes/Single Cells/CD3/CD4/CD107&Lymphocytes/Single Cells/CD3/CD4/IFNg&Lymphocytes/Single Cells/CD3/CD4/IL2&Lymphocytes/Single Cells/CD3/CD4/Mip1b&Lymphocytes/Single Cells/CD3/CD4/TNF")
    })

test_that("v 10.0.8r1 - vX 20.0 (OrNode)",{
  
  thisPath <- file.path(path, "combineNode/OrNode")
  wsFile <- file.path(thisPath, "Test_EW.wsp")
  
  ws <- open_flowjo_xml(wsFile)
  
  gs <- flowjo_to_gatingset(ws, name = 1, path = thisPath)
  
  expect_is(gs, "GatingSet")
  gh <- gs[[1]]
  g <- gh_pop_get_gate(gh, "CD44+")
  expect_is(g, "booleanFilter")
  expect_equal(g@deparse, "FCS singlets/SSC singlets/Lymphocytes/CD8/F5/Live/Q6: CD44+ , CD62L+|FCS singlets/SSC singlets/Lymphocytes/CD8/F5/Live/Q7: CD44+ , CD62L-")
  
  expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
  thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
  expect_equivalent(thisCounts, expectCounts)
})



test_that("v 10.0.8 - vX 20.0 (slash_issue_vX)",{
      thisPath <- file.path(path, "slash_issue_vX")
      wsFile <- file.path(thisPath, "IFEP004.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = 5, path = file.path(thisPath), which.lines = 1e5)
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 0.025)
      
      
    })

test_that("v 10.2 - vX 20.0 (EllipsoidGate)",{
      thisPath <- file.path(path, "EllipsoidGate_10.2")
      wsFile <- file.path(thisPath, "mA J21 for HT.wsp")
      
      ws <- open_flowjo_xml(wsFile)
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = 2, path = file.path(thisPath), which.lines = 1e5)
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 0.03)
      
      
    })

test_that("v 7.6.1- win 1.6 (use default biexp trans when channel-specific trans not found within its respective trans group )",{
      
      thisPath <- file.path(path, "GYO")
      wsFile <- file.path(thisPath, "whole blood GYO-0109 050214.wsp")
      ws <- open_flowjo_xml(wsFile)
      
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1, path = thisPath,  execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      expect_output(gs <- flowjo_to_gatingset(ws, name = 2, path = thisPath, which.lines = 1e5), "FCS")
      gh <- gs[[1]]
      
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 0.015)
    })

test_that("v 7.6.5 - win 1.61 (PBMC)",{
      
      thisPath <- file.path(path, "PBMC/Blomberg")
      wsFile <- file.path(thisPath, "Exp2_Tcell.wsp")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, sampNloc = "sampleNode", execute = FALSE)
      expect_is(gs, "GatingSet")
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, sampNloc = "sampleNode")
      gh <- gs[[1]]
            
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 9e-3)
            
    })

test_that("v 7.6.5 - win 1.61 (sampNloc = 'sampleNode')",{
      
      thisPath <- file.path(path, "Cytotrol/Miami")
      wsFile <- file.path(thisPath, "flowJo/Cytotrol_061112_Tcell.wsp")

      ws <- open_flowjo_xml(wsFile)
      
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, path = file.path(thisPath,"Tcell"), sampNloc = "sampleNode", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = 1, subset = 1, path = file.path(thisPath,"Tcell"), sampNloc = "sampleNode")
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 2e-4)
      
    })

#this test case failed on the libxml parsing
# test_that("v 9.0.1 - mac 2.0 (HVTN RV144 -- options = 1)",{
#       
#       thisPath <- file.path(path, "HVTN/RV144")
#       wsFile <- file.path(thisPath, "Batch 1264 RV144.xml")
# 
#       dd <- capture_output(ws <- open_flowjo_xml(wsFile, options = 1))
#       #not sure how to suppress the long stacks of C messages (XML package)      
#       gs <- flowjo_to_gatingset(ws, name = 4, subset = 1, execute = FALSE)
#       expect_is(gs, "GatingSet")
#       
#       gs <- flowjo_to_gatingset(ws, name = 4, subset = 1)
#       gh <- gs[[1]]
#       expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#       thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
#       expect_equal(thisCounts, expectCounts, tolerance = 1e-5, check.attributes = FALSE)
#       
#     })
# 
test_that("v 9.0.1 - mac 2.0 (HVTN 080-0880)",{
      
      thisPath <- file.path(path, "HVTN/080")
      wsFile <- file.path(thisPath, "080 batch 0880.xml")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 4, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = 4, subset = "431321.fcs", which.lines = 1e5)
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 4e-3)
      
    })


test_that("v 9.2 - mac 2.0 (ITN029)",{
      
      thisPath <- file.path(path, "ITN029ST")
      wsFile <- file.path(thisPath, "QA_template.xml")

      ws <- open_flowjo_xml(wsFile)
      expect_output(gs <- flowjo_to_gatingset(ws, name = 2, subset = 1,execute = FALSE), "no calibration")
      expect_is(gs, "GatingSet")
      
      expect_output(gs <- flowjo_to_gatingset(ws, name = 2, subset = 1), "no calibration")
      gh <- gs[[1]]
      # expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      # thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
      # expect_equal(thisCounts, expectCounts, check.attributes = FALSE)
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 7e-3)
      
    })
test_that("v 9.4.2 - mac 2.0",{
      
      thisPath <- file.path(path, "PBMC/HIPC_trial")
      wsFile <- file.path(thisPath, "data/HIPC_trial.xml")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1)
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 5e-3)
      
    })

#1. boolean specification contains double spaces and non-spaces, which leads to the fix that uses boolean operator as delimiter instead of space)
#2. boolean gate refers to the node (AM) that appears both at sibling and children level, which leads to further checking in getRefNodes routines
#3. boolean gate has quotedString which leads to more generic xpath searching for gatePath and trailing space removal.
test_that("v 9.4.4 - mac 2.0 ",{
      
      thisPath <- file.path(path, "JJ")
      wsFile <- file.path(thisPath, "JJ_FlowJo_.xml")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = "Test", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = "Test", subset = 1, which.lines = 1e5)
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 2e-2)
      
    })


test_that("v 9.5.2 - mac 2.0",{
      
      thisPath <- file.path(path, "Cytotrol/NHLBI")
      wsFile <- file.path(thisPath, "flowJo/NHLBI.xml")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1, path = file.path(thisPath,"Bcell"), execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1, path = file.path(thisPath,"Bcell"))
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 5e-3)
      
      #create a temp folder and symlink to original files to test the feature of searching sample by keyword $FIL
      # in the use case where the fcs has been modified
      fcs <- list.files(pattern = "fcs", file.path(thisPath, "Bcell"), full = T)[[1]]
      tmp <- tempfile()
      dir.create(tmp)
      newFCS <- file.path(tmp, "test.fcs")
      file.symlink(fcs, newFCS)
      
      gs <- flowjo_to_gatingset(ws, name = 2, subset = 1, path = tmp)
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 5e-3)
      unlink(tmp,recursive = T)
    })

test_that("v 9.6.3 - mac 2.0 (ignore highValue for FSC/SSC)",{
      
      thisPath <- file.path(path, "roch")
      wsFile <- file.path(thisPath, "PROP_20120118_TPHE.xml")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 1, subset = "Specimen_001_Tube_024.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = 1, subset = "Specimen_001_Tube_024.fcs")
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 7e-3)
      
    })

test_that("v 9.7.4 - mac 3.0",{
      thisPath <- file.path(path, "v9.7.4")
      wsFile <- file.path(thisPath, "T1 CHI-002v974.xml")

      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = "CHI-002 PBMC control", subset = "CHI-002 PBMC control_101211.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = "CHI-002 PBMC control", subset = "CHI-002 PBMC control_101211.fcs")
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh, path = "full")[, list(xml.count,openCyto.count, node)]
      expectCounts[flowJo.count ==0, flowJo.count := -1]#fix the legacy counts
      expect_equal(thisCounts, expectCounts, check.attributes = FALSE, tol = 3.3e-3)
    })

test_that("v 9.7.5 - mac 3.0 (no compensation and using calibrationIndex)",{
      thisPath <- file.path(path, "Ragon")
      wsFile <- file.path(thisPath, "neut v non neut v9.xml")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 5, subset = "477889_env_cct_norm_concatenated.txt", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- flowjo_to_gatingset(ws, name = 5, subset = "477889_env_cct_norm_concatenated.txt", fcs_file_extension = ".txt")
	  gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- gh_pop_compare_stats(gh)[, list(xml.count,openCyto.count, node)]
      expectCounts[flowJo.count ==0, flowJo.count := -1] #fix the legacy counts
      expect_equal(thisCounts, expectCounts, check.attributes = FALSE, tol = 2e-4)
    })

test_that("v 9.7.5 - mac 3.0 (boolGate that refers to the non-sibling nodes)",{
      thisPath <- file.path(path, "094")
      wsFile <- file.path(thisPath, "1851-M-094.xml")
      
      ws <- open_flowjo_xml(wsFile)
      gs <- flowjo_to_gatingset(ws, name = 2, subset = "434713.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      set.seed(1)
      gs <- flowjo_to_gatingset(ws, name = 2, subset = "434713.fcs", which.lines = 1e5)
      gh <- gs[[1]]
      thisCounts <- gh_pop_compare_stats(gh)
      expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 5e-3)
    })

test_that("search_for_fcs logic", {
  thisPath <- file.path(path, "file_search_tests")
  wsFile <- file.path(thisPath, "manual.xml")
  subset <- c("CytoTrol_CytoTrol_1.fcs", "CytoTrol_CytoTrol_2.fcs")
  ws <- open_flowjo_xml(wsFile)
  
  # Simple case, no subdirs and filenames match sampleNames
  fcs_path <- file.path(thisPath, "no_subdirs")
  gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path)
  expect_equal(length(gs), 2)
  
  # This one should reject CytoTrol_CytoTrol_1.fcs because $TOT doesn't match
  fcs_path <- file.path(thisPath, "no_subdirs_wrong_TOT")
  msgs <- capture.output(gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path))
  expect_match(msgs[[1]], "incorrect total number of events",)
  expect_equal(length(gs), 1)
  
  # One of the filenames doesn't match, go to $FIL
  fcs_path <- file.path(thisPath, "needs_FIL")
  gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path)
  expect_equal(length(gs), 2)
  
  # Toplevel directory file has wrong $TOT, so it should grab the one
  # from the subdirectory
  fcs_path <- file.path(thisPath, "needs_TOT")
  gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path)
  expect_equal(length(gs), 2)
  expect_match(keyword(gs[[1]], "FILENAME"), "subdir_with_correct_TOT/CytoTrol_CytoTrol_1.fcs")
  
  # In toplevel $TUBE NAME on CytoTrol_CytoTrol_1.fcs changed CytoTrol_2 -> Cytotrol_2
  # If no subdirs, accept lone match (no need to check keys)
  fcs_path <- file.path(thisPath, "needs_keys", "no_subdirs")
  gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path)
  expect_equal(length(gs), 2)
  expect_match(keyword(gs[[2]], "FILENAME"), "no_subdirs/CytoTrol_CytoTrol_2.fcs")
  # Now, with multiple $TOT matches, need to check keys and take the one from the subdir
  fcs_path <- file.path(thisPath, "needs_keys", "w_subdirs")
  # It should fail without specifying additional keys
  expect_error(gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path), "Multiple FCS files", class = "error")
  gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path, additional.keys = c("TUBE NAME"))
  expect_equal(length(gs), 2)
  expect_match(keyword(gs[[2]], "FILENAME"), "subdir_with_correct_keys/CytoTrol_CytoTrol_2.fcs")
  
  # Just make sure it appropriately errors out for full duplicates
  fcs_path <- file.path(thisPath, "full_duplicates")
  expect_error(gs <- flowjo_to_gatingset(ws, name=4, subset=subset, path=fcs_path, additional.keys = c("TUBE NAME")), "Multiple FCS files", class = "error")
  
})

test_that("magnetic gates", {
  thisPath <- file.path(path, "magnetic")
  ws <- open_flowjo_xml(file.path(thisPath, "2020-03-11 CyTOF gating strategy_dummy.wsp"))
  gs <- flowjo_to_gatingset(ws, name = 1)
  thisCounts <- gh_pop_compare_stats(gs[[1]])
  # Give a little cushion to frequency tolerance due to small subsample
  expect_equal(thisCounts[, xml.freq], thisCounts[, openCyto.freq], tol = 2e-2)
})

