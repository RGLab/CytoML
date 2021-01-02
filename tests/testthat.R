library(testthat)
library(CytoML)

win32_flag = .Platform$OS.type == "windows" && .Machine$sizeof.pointer != 8
if(!win32_flag)
  test_check("CytoML")

#devtools::test()

# test_file("~/rglab/workspace/CytoML/tests/testthat/flowjo2gs_internalTestSuite.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/Cytobank2GatingSet-InternalTestSuite.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/GatingSet2flowJo-InternalTestSuite.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/diva2gs-InternalTestSuite.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/test-cytobank.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/test-diva2gs.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/test-extend.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/test-GatingSet2flowJo.R")
# test_file("~/rglab/workspace/CytoML/tests/testthat/test-flowjo2gs.R")
