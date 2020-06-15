context("flowjo_to_gatingset")
dataDir <- system.file("extdata",package="flowWorkspaceData")
wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)

ws <- open_flowjo_xml(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowjo_workspace"))
})

source("flowJoWorkspace-testSuite.R", local = TRUE)


gs <- NULL

test_that("parse workspace by loading data from cytoset",{
  #default search fcs 
  tmp <- tempfile()
  dir.create(tmp)
  invisible(capture_output(expect_error(gs <- flowjo_to_gatingset(ws, name = 4, path = tmp), "No samples", class = "error")))
  cs <- load_cytoset_from_fcs(list.files(dataDir, ".fcs", full.names = TRUE)[1])
  #supply in-memory cs
  expect_equal(cs_get_h5_file_path(cs), "")
  expect_error(gs <- flowjo_to_gatingset(ws, name = 4, path = tmp, cytoset = cs), "not supported", class = "error")
  #supply non-matched cs
  cs <- load_cytoset_from_fcs(list.files(dataDir, "a2004", full.names = TRUE)[1], is_h5 = TRUE)
  invisible(capture_output(expect_error(gs <- flowjo_to_gatingset(ws, name = 4, path = tmp, cytoset = cs), "No samples", class = "error")))
  #supply correct cs
  cs <- load_cytoset_from_fcs(list.files(dataDir, "CytoTrol_CytoTrol_", full.names = TRUE), is_h5 = TRUE)
  #create view
  cf <- get_cytoframe_from_cs(cs, 1)
  # colnames(cf)[5] <- "B710"
  newcol <- matrix(0, nrow = nrow(cf), ncol = 1)
  colnames(newcol) <- "redundant"
  cf <- cf_append_cols(cf, newcol)
  cf <- cf[, -13]
  cflist <- list(cf, cs[[2, return = "cytoframe"]])
  names(cflist) <- c("a","b")
  cs <- cytoset(cflist)
  invisible(capture_output(gs <- flowjo_to_gatingset(ws, name = 4, path = tempdir(), cytoset = cs)))
  expect_that(gs, is_a("GatingSet"))
  stats <- gh_pop_compare_stats(gs[[1]])  
  expect_equal(stats[, openCyto.freq], stats[, xml.freq], tol = 3e-3)
  #cs is shared with gs
  expect_equal(cs_get_h5_file_path(cs), cs_get_h5_file_path(gs_cyto_data(gs)))
  #cs meta is also modified in place
  expect_equal(colnames(cs), colnames(gs_cyto_data(gs)))
  
  })

test_that("Can parse workspace in current working dir without path",{
  wd <- getwd()
  setwd(dataDir)
  ws_wd <- open_flowjo_xml("manual.xml")
  dd <- capture.output(suppressMessages(gs <<- try(flowjo_to_gatingset(ws_wd, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = NULL))))
  expect_that(gs, is_a("GatingSet"))
  setwd(wd)
})

test_that("Can parse workspace",{
    dd <- capture.output(suppressMessages(gs <<- try(flowjo_to_gatingset(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = NULL))))
	  expect_that(gs, is_a("GatingSet"));
        
    expect_output(expect_error(suppressMessages(flowjo_to_gatingset(ws
                                                                , path = file.path(dataDir, "gs_manual")
                                                                , name = 4
                                                                , subset = "CytoTrol_CytoTrol_1.fcs"
                                                                , additional.keys = NULL
                                                                )
                                                )
                                  , "No sample", class = "error")
                       , "FCS not found")
                
	
})


gh <- gs[[1]]


test_that("parse without gating",{
      
      dd <- capture.output(suppressMessages(gs1 <- try(flowjo_to_gatingset(ws, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", execute = FALSE))))
      expect_that(gs1, is_a("GatingSet"));
      gh1 <- gs1[[1]]
      
      thisStats <- gh_pop_compare_stats(gh1)[, list(xml.freq,xml.count, node)]
      expectStats <- gh_pop_compare_stats(gh)[, list(xml.freq,xml.count, node)]
      expect_equal(thisStats, expectStats)
      
      #exclude the gates that require extension since the extend_to are different 
      # based on whether data is loaded
      nodes <- gs_get_pop_paths(gh)[ -c(6:13, 15:22)]
      thisGates <- sapply(nodes[-1], gh_pop_get_gate, obj = gh1)
      expectGates <- sapply(nodes[-1], gh_pop_get_gate, obj = gh)
      expect_equal(thisGates, expectGates)
      
      
    })

test_that("external comp", {
      comp <- gh_get_compensations(gh)
      #single comp
      dd <- capture.output(suppressWarnings(suppressMessages(
                          gs1 <- try(flowjo_to_gatingset(ws, name = 4
                                      , compensation = comp
                                      , execute = TRUE)))))
      expect_that(gs1, is_a("GatingSet"));

      gh1 <- gs1[[1]]
      expect_equal(comp, gh_get_compensations(gh1))
      
      thisStats <- gh_pop_compare_stats(gh1)
      expectStats <- gh_pop_compare_stats(gh)
      expect_equal(thisStats, expectStats)
      
      #a list of comp
      comp <- gs_get_compensations(gs1)
      dd <- capture.output(suppressWarnings(suppressMessages(
              gs1 <- try(flowjo_to_gatingset(ws, name = 4
                      , compensation = comp
                      , execute = TRUE)))))
      expect_that(gs1, is_a("GatingSet"));
      expect_equal(comp,  gs_get_compensations(gs1))
      gh1 <- gs1[[1]]
      thisStats <- gh_pop_compare_stats(gh1)
      expect_equal(thisStats, expectStats)
      
      
      #extra elements
      comp[3] <- comp[1]
      names(comp)[3] <- "dd"
      dd <- capture.output(suppressWarnings(suppressMessages(gs1 <- flowjo_to_gatingset(ws, name = 4, compensation = comp))))
      expect_that(gs1, is_a("GatingSet"));
      expect_equal(comp[1:2],  gs_get_compensations(gs1))
      
    })

test_that("use additional keywords for guid",{
      dd <- capture.output(suppressMessages(gs2 <- try(flowjo_to_gatingset(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = "$TOT"))))
      expect_equal(sampleNames(gs2[[1]]), paste(sampleNames(gh), trimws(keyword(gh)[["$TOT"]]), sep = "_"))
      expect_equal(gh_pop_compare_stats(gs2[[1]]), gh_pop_compare_stats(gh))
        
    })
#TODO:fix failure
# test_that("supply sampleID--file mapping through 'path'",{
#       mapping <- data.frame(sampleID1 = '1', file = file.path(dataDir, "CytoTrol_CytoTrol_11.fcs"))
#       expect_error(dd <- capture.output(suppressMessages(gs3 <- flowjo_to_gatingset(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
#                   , "When 'path' is a data.frame, it must contain columns")
#       colnames(mapping)[1] <- "sampleID"
#       expect_error(dd <- capture.output(suppressMessages(gs3 <- flowjo_to_gatingset(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
#           , "must be numeric")
#       mapping[["sampleID"]] <- 1
#       expect_error(dd <- capture.output(suppressMessages(gs3 <- flowjo_to_gatingset(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
#           , "No sample")
#       mapping[["sampleID"]] <- 19
#       expect_error(dd <- capture.output(suppressMessages(gs3 <- flowjo_to_gatingset(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
#           , "not a valid file")
#       mapping[["file"]] <- file.path(dataDir, "CytoTrol_CytoTrol_1.fcs")
#       dd <- capture.output(suppressMessages(gs3 <- flowjo_to_gatingset(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
#       expect_equal(getPopStats(gs3[[1]]), getPopStats(gh))
#       
#     })

test_that("parse pData from keyword", {
    keys <- c("PATIENT ID", "SAMPLE ID", "$TOT", "EXPERIMENT NAME")
    #parse pData from xml
    expect_error(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4, keywords = keys, execute = F, keywords.source="FCS")
                 , "Can't parse phenodata", class = "error")
    dd <- capture.output(suppressMessages(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4, keywords = keys, execute = F)))
    pd1 <- pData(gs1)
    expect_equal(nrow(pd1), 4)
    
    #parse pData from FCS
    dd <- capture.output(suppressWarnings(suppressMessages(gs2 <- flowjo_to_gatingset(ws, path = dataDir, name = 4, keywords = keys, keywords.source = "FCS"))))
    pd2 <- pData(gs2)
    expect_equal(nrow(pd2), 2)
        
    expect_equivalent(pd1[1:2, names(pd2)], pd2)
    
    #case insensitive
    keys <- tolower(keys)
    expect_error(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4, keywords = keys, execute = F)
                   , regexp = "not found", class = "error")
    # pd2 <- pData(gs1)
    # expect_true(all(is.na(pd2[[2]])))
  
    #TODO:ignore case for keyword
    # dd <- capture.output(suppressMessages(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4, keywords = keys, execute = F, keyword.ignore.case = T)))
    # pd2 <- pData(gs1)
    # colnames(pd1)[-1] <- keys
    # expect_equal(pd1, pd2)
    # 
    })


test_that("subset", {
  
    dd <- capture.output(suppressMessages(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                          , subset = `TUBE NAME` %in% c("CytoTrol_1", "CytoTrol_2")
                                          , keywords = "TUBE NAME", execute = F)))
    #subset by sample names
    dd <- capture.output(suppressMessages(gs2 <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                                                , subset = c("CytoTrol_CytoTrol_1.fcs", "CytoTrol_CytoTrol_2.fcs")
                                                                , keywords = "TUBE NAME"
                                                                , execute = F)))
    # expect_equivalent(pData(gs1), pData(gs2))
    
    #subset by numeric index
    dd <- capture.output(suppressMessages((gs3 <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                                                 , subset = 1:2
                                                                 , keywords = "TUBE NAME"
                                                                 , execute = F))))
    expect_equivalent(pData(gs1), pData(gs2))
    expect_equivalent(pData(gs2), pData(gs3))
    
    expect_error(gs4 <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                       , subset = 1:2
                                       , keywords = "TUBE NAME", execute = F
                                       , keywords.source = "FCS"
                                       )
                , "Can't parse phenodata from FCS", class = "error")

            
    })


