
# CytoML: A tool designed to work with openCyto to exchange gated cytometry data with third-party platforms

This package is designed to import/export the hierarchical gated flow data to/from the `openCyto` framework using the standard cytometry format: `gatingML2.0` and `FCS3.0`. This package makes use of our `GatingSet` R object and data model such that imported data can easily be manipulated and visualized in R using tools like `OpenCyto` and `ggcyto`.


### INSTALLATION

```r
# First, install it from bionconductor so that it will pull all the dependent packages automatically
library(BiocInstalller)
bicLite(openCyto) # may be older
# Then, install the latest version from github using devtools package 
install.packages("devtools") 
library(devtools) #load it
install_github("RGLab/flowWorkspace", ref="trunk")
install_github("RGLab/openCyto", ref="trunk")
```

### Import `gatingML` and `FCS` data from other platforms into `openCyto`

```r
library(CytoML)
xmlfile <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "CytoML")
fcsFiles <- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
gs <- cytobank2GatingSet(xmlfile, fcsFiles)
```

### Then you can interact with the gated data (`GatingSet`)

```r
#get the first sample
gh <- gs[[1]]
#plot the hierarchy tree
plot(gh)
#show all the cell populations(/nodes)
getNodes(gh)
#show the population statistics
getPopStats(gh)
#plot the gates
plotGate(gh) 
```

### Export the existing `GatingSet` from `openCyto` to `Cytobank` or `flowJo`

```r
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))

#Cytobank
outFile <- tempfile(fileext = ".xml")
GatingSet2cytobank(gs, outFile)

#flowJo
outFile <- tempfile(fileext = ".wsp")
GatingSet2flowJo(gs, outFile)
```
