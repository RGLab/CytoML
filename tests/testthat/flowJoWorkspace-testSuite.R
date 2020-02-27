context("workspace")
# resultDir <- "tests/testthat/expect_result/"
fjRes <- readRDS(file.path(resultDir, "flowJoWorkspace_expect.rds"))

test_that("show workspace",
    {
      thisRes <- capture.output(show(ws))[-(1:2)]
      expectRes <- fjRes[["ws_show"]][-(1:5)]
      # expectRes[3] <- sub("45", "35", expectRes[3])#now we getSampleGroups also include the samples with 0 populations
      expect_equal(thisRes, expectRes)
      
    })


test_that("getKeywordsBySampleID workspace",
    {
      thisExpectRes <- fjRes[["getkwByID_ws"]]
      thisExpectRes <- lapply(fjRes[["getkwByID_ws"]], function(kw)trimws(kw[["value"]]))
      names(thisExpectRes) <- lapply(fjRes[["getkwByID_ws"]], "[[", "name")
      
      expect_equal(fj_ws_get_keywords(ws, 1), thisExpectRes)
      
    })

test_that("fj_ws_get_keywords workspace",
    {
      expect_error(fj_ws_get_keywords(ws, "CytoTrol_CytoTrol_1.fcs"), "Multiple sample nodes found", class = "error")
      thisExpectRes <- lapply(fjRes[["getkw_ws"]], trimws)
      expect_equal(fj_ws_get_keywords(ws, 1), thisExpectRes)
    })



test_that("getSamples&getSampleGroups workspace",
    {
      thisRes <- fj_ws_get_samples(ws)
      thisExpect <- fjRes[[".getSamples"]]
      #record the rows to be removed
      excludeIds <- as.integer(rownames(subset(thisExpect, pop.counts <=0)))
      thisExpect <- thisExpect[-excludeIds,-4]
      expect_equivalent(subset(thisRes, count > 0), thisExpect)
      
      
      thisRes <- fj_ws_get_sample_groups(ws)
      thisExpect <- fjRes[[".getSampleGroups"]]
      # thisExpect <- thisExpect[-excludeIds, ]#now we getSampleGroups also include the samples with 0 populations
      expect_equivalent(thisRes, thisExpect)
    })

