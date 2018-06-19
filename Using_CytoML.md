---
title: "Using CytoML to Import and Export Gated Cytometry Data"
author: "Greg Finak"
date: "6/4/2018"
output: 
  html_document:
    fig_caption: yes
    keep_md: yes
    theme: paper
    toc: yes
bibliography: bibliography.bib
---



## Background.

We demonstrate how to import and export gated cytometry data into R/Bioconductor using the CytoML  R/Bioconductor package. 
CytoML fits into a important niche in the data analysis pipeline by enabling reproducible analysis and data sharing of manually and computationally gated cytometry data across several different vendor platforms. 

### BD's DIVA software 

Beckton Dickinson's (BD's) DIVA software is used as the acquisition software for all BD instruments.
It is often used to define gates for sorting cell poplations and also for data analysis.
DIVA's analyses are stored as XML files. No special conversion needs to be done to read these using CytoML. 

DIVA XML organizes the different gating groups into different 'specimen' nodes under an 'experiment' node. Each 'specimen' group contains multiple 'tube' nodes, which represent FCS samples. All gate definitions are directly stored under 'tube' node. The gating hierarchy can be derived from the 'parent' attribute of each gate and each gate is uniquely labeled with the full gating path (i.e. 'A\B\C').

Gate definitions are not GatingML-complied. Currently CytoML supports RECTANGLE_REGION, POLYGON_REGION, INTERVAL_REGION gates.
DIVA supports log or biexp transformations. The transformation is determined by the attributes under the 'tube/instrument_settings' node. The global transformation parameters can be overridden by gate-level attributes.

### FlowJo workspaces.

FlowJo (Ashland, OR) is a graphical data analysis platform used primarily for manual gating of cytometry data. 
Older versions of FlowJo (< v. 10) stored workspaces in binary format (so-called ".jo" files) that could be exported to XML from within FlowJo. 
Newer versionsof FlowJo (>= v. 10) store workspaces in XML format natively (".wsp" files). 
The ".wsp" files can work natively with the CytoML package. 

FlowJo XML (latest v 10) stores the gates, transformation and compensation informatoin at the sample level ('Sample' node). The gating hierarchy is naturally captured in the nested 'subPopulation' xml nodes. The gate definitions are partially GatingML2-compliant. Currently the gates supported by CytoML are NotNode, OrNode, AndNode, PolygonGate, RectangleGate (a QuadGate is typically stored as four RectangleGate objects), EllipsoidGate, and CurlyQuad.

### Cytobank workspace files.

Cytobank encodes analyses in XML format as well. No special conversion needs to be done to read Cytobank XML files. 

Cytobank XML is the most Gating-ML 2.0 standard-conforming format (http://www.isac-net.org/std/Gating-ML/v2.0/gating).
All the gate objects as well as the transformation and compenstation nodes are stored in Gating-ML 2.0 format and thus can be directly parsed into R through the *flowUtils* package. 
However, these objects are organized in a flat structure. That means every gate is stored at the root level of the XML document. 
Gates are also replicated across samples *if* the gate coordinates are not identical for some samples. 
In these cases, the sample/FCS information is attached to the gate node through the 'custom_info' property. 
Consequently, the gating hierarchy needs to be reconstructed from the what Cytobank calls a 'GateSet' node. 
These 'GateSet' nodes are composed of 'BooleanGate' nodes that use the 'AND' operator to organize the sequence of gates that need to be applied to identify a specific cell population. 
The 'QuadrantGate' is treated differently since each 'quadrant' essentially represents a separate gate and thus is not parsed through a 'GateSet'.
Besides the standard Gating-ML contents, each Gating-ML node also carries the extra 'custom_info' which stores some critical information (e.g. gate name, gate definition in json format) that's required for the cytobank parser work with its own XML.
Both the transformation and compensation properties are global (e.g., apply to all samples).
The counts of cells in each cell population are not stored in the Cytobank XML and need to be imported as a separate csv, or they can be recomputed from the raw data once imported into R.


### A unifying data representation.

Despite the differences between how each platform represents a gated data analysis, once imported, the data are represented using a common set of underlying data structures, implemented in the *flowWorkspace* package. 
Individual samples are mapped to *GatingHierarchy* objects, and collections of them with a common gating scheme are combined into *GatingSet* objects. 
A *GatingSet/GatingHierarchy* stores along with it the gates, transformations, compensation matrices, and sample meta-data necessary to reproduce an analysis from raw FCS files. 
All the same information that is stored in the platform-specific XML files.
Once these data are in a common representation it CytoML allows us to transform data between platforms.


#### Export of computational gating.

Because of the common data representation, data that are gated using computational tools can be represented in the same manner and can be exported to the different platforms for visualization. 
This enables computational analyses to be shared with, and the data to be inspected by, users of these other platforms. 
Additionally, it allows the flexibility of mixing manual with computational analysis. 
Users can, for example, import manually gated data and perform computational gating or visualization of a subset of cells within the data set very easily *flowWorskpace* package (@flowWorkspace, @Lin2015-zg).

## Examples

These examples require the *flowWorkspaceData* package as well as the *CytoML* package and its dependencies. All figures (except the plots from FlowJo and Cytobank) are generated using the *ggcyto* package (@Van2018-zj). Automated gating is performed using the openCyto package (@F).

These can be installed using Bioconductor as follows:


```r
# Set a default mirror
utils::chooseCRANmirror(ind = 64)
#Install Bioconductor if it's not installed
if(!"BiocInstaller"%in%rownames(installed.packages())){
  source("http://www.bioconductor.org/biocLite.R")
}else{
  #Otherwise:
  library(BiocInstaller)
}
#install flowWorkspaceData if it's not installed
if(!"flowWorkspaceData"%in%rownames(installed.packages())){
  biocLite("flowWorkspaceData", ask = FALSE)
}
#install CytoML if it's not installed
if(!"CytoML"%in%rownames(installed.packages())){
  biocLite("CytoML", ask = FALSE)
}
if(!"png"%in%rownames(installed.packages())){
  biocLite("png", ask = FALSE)
}
#install ggcyto
if(!"ggcyto"%in%rownames(installed.packages())){
  biocLite("ggcyto", ask = FALSE)
}
```


### Importing and Visualizing FlowJo workspaces.


```r
library(flowWorkspace)
library(png)
library(grid)
library(gridExtra)
library(gtable)
library(cowplot)
library(ggcyto)
library(CytoML)
```

We find the path to the manual gating XML FlowJo file that's distributed with the flowWorkspaceData package. 


```r
manual_gating_xml = system.file("extdata",package="flowWorkspaceData","manual.xml")
```

We need to do two things to import the data. 
We need to open the workspace, then we need to choose a group of files to import. 
We see that this workspace has six sample groups, five of which correspond to different staining panels (for B cells, DCs, T cells, T helper cells, and T regulatory cells). 
The sixth is a group of "All Samples" that is almost always present in FlowJo workspaces, and should *never* be imported into R, since it would construct a *GatingSet* of samples with inconsistent (i.e. different) gating trees. 



```r
ws = openWorkspace(manual_gating_xml)
print(ws)
```

```
## FlowJo Workspace Version  2.0 
## File location:  /Library/Frameworks/R.framework/Versions/3.5/Resources/library/flowWorkspaceData/extdata 
## File name:  manual.xml 
## Workspace is open. 
## 
## Groups in Workspace
##          Name Num.Samples
## 1 All Samples          45
## 2      B-cell           4
## 3          DC           4
## 4      T-cell           4
## 5     Thelper           4
## 6        Treg           4
```

We will import the `T-cell` group. We need to point the parser at the right location where the FCS files are located.


```r
path = system.file("extdata",package="flowWorkspaceData")
gs_tcell = parseWorkspace(ws, name = "T-cell", path = path)
```

```
## mac version of flowJo workspace recognized.
```

```r
gs_tcell
```

```
## A GatingSet with 2 samples
```

Two of the files are available in the data package. We can plot the gating tree from this analysis, as well as produce dotplots of individual gated populations using Bioconductor's tools.


```r
plot(gs_tcell)
```

![Figure 1: Gating tree of the Cytotrol T-cell data imported from a FlowJo workspace using CytoML and flowWorkspace.](Using_CytoML_files/figure-html/plot_tree-1.png)

The gating tree shows gating of different memory and activated CD4 and CD8 T cell populations. 


```r
p = autoplot(gs_tcell[[1]], bin = 128)
```

![Figure 2:  The gating hierarchy for a Cytotrol T cell sample imported from a FlowJo gating hierarchy.](Using_CytoML_files/figure-html/cd4_memory-1.png)

### Adding a lymphocyte gate using openCyto.

We'll use openCyto (@Finak2014-mr) to add a lymphocyte gate on the FSC-A, SSC-A channels beneath the singlet gate.

![Figure 3: The ungated lymphocyte population shown in FSC-A and SSC-A.](Using_CytoML_files/figure-html/opencyto_gating_fj-1.png)


```r
# Add a gate using opencyto
add_pop(gs=gs_tcell,alias = "lymphocytes",pop = "+",parent = "singlets",dims = "FSC-A,SSC-A", gating_method = "flowClust",gating_args = "K=3")

#move the CD3+ subtree beneath the lymphocyte gate
for(i in sampleNames(gs_tcell))
  flowWorkspace::moveNode(gh = gs_tcell[[i]], node = "CD3+", to = "lymphocytes")
recompute(gs_tcell)
```

### Exporting to FlowJo workspace format. 

Finally we'll export the new modified gating set to a FlowJo XML workspace.


```r
CytoML::GatingSet2flowJo(gs_tcell,outFile = "new_flowjo_xml.xml")
```

```
## [1] "new_flowjo_xml.xml"
```




```r
system("open -a /Applications/FlowJo.app new_flowjo_xml.xml")
```


```r
img = png::readPNG("new_flowjo_xml-Layout.png")
g <- rasterGrob(img, interpolate=TRUE)
p1 = ggcyto(gs_tcell,
            subset = "singlets",
            mapping = aes(x = "FSC-A", y = "SSC-A")) + geom_hex(bins = 128) + geom_gate("lymphocytes") + geom_stats() + theme(axis.text.x =
            element_text(angle = 45, hjust = 1)) + theme_classic(base_size = 10)
            p1 = as.ggplot(p1)
p1 = ggplotGrob(p1)
g = rasterGrob(img,interpolate = TRUE)

grid.newpage()
## main viewports, I divide the scene in 10 rows ans 5 columns(5 pictures)
pushViewport(plotViewport(margins = c(1,1,1,1),
             layout=grid.layout(nrow=10, ncol=100),xscale =c(1,5)))
## I put in the 1:7 rows the plot without axis
## I define my nested viewport then I plot it as a grob.
pushViewport(plotViewport(layout.pos.col=5:95, layout.pos.row=1:4,
             margins = c(1,1,1,1)))
grid.draw(p1)
upViewport()
grid.text("A",0,1)
pushViewport(plotViewport(layout.pos.col=1:100, layout.pos.row=5:10,
             margins = c(1,1,1,1)))
grid.draw(g)
upViewport()
grid.text("B",0 ,0.55)
```

![**Figure 4**: Lymphocyte gates visualized in openCyto (A) and in FlowJo (B). A workspace of manually gated data was imported into R, modified using openCyto to add the lymphocyte gate, and exported to a new FlowJo workspace using the CytoML package.](Using_CytoML_files/figure-html/combined_plot-1.png)

The plots from openCyto (Figure 4A) and the exported FlowJo workpsace (Figure 4B) show the same information (and have the same population statistics), with some minor style differences.

## Exporting to Cytobank format.

We can take the same gating set we just worked on and export it to Cytobank format. 


```r
GatingSet2cytobank(gs_tcell,outFile = "new_cytobank_xml_cytobank_scale.xml",cytobank.default.scale = TRUE)
```

```
## [1] "new_cytobank_xml_cytobank_scale.xml"
```


This gating-ml file is imported into a cytobank experiment along with the FCS files. The experiment is public and can be accessed at   the [cytobank community site](https://community.cytobank.org/cytobank/experiments/72951/illustrations/138779). The cytobank imported gates can be seen below:



![**Figure 5**: Cytobank display of the gating hierarchy for a manually gated Cytotrol T cell sample, with an added openCyto lymphoctye gate. ](GatingHierarchyPlotsForCytoMLExample.png)


### Importing from Cytobank.

Next we will import a cytobank workspace. For this purpose we will work with an existing public data set on cytobank. 
We use the public experiment ["Kinase Inhibitor-Treated Reprogramming MEFs"](https://community.cytobank.org/cytobank/experiments/43281) (@Zunder2015-ne). 

We download the ACS container and unzip it to access the contents.


```r
gating_ml_cytobank_file = list.files(
  path = "experiment_43281_Jun-05-2018_11-56-AM",
  pattern = "gate.*xml",
  recursive = TRUE,
  full.names = TRUE
  )
  fcs_files = list.files(
  path = file.path(
  "experiment_43281_Jun-05-2018_11-56-AM",
  "experiments",
  "43281",
  "fcs_files"
  ),
  recursive = TRUE,
  full.names = TRUE
  )
  #Import the cytobank xml workspace to create a GatingSet.
  gs = cytobank2GatingSet(xml = gating_ml_cytobank_file, FCS = fcs_files)
  
  #Arrange the plots of the 6 samples using the ggcyto package.
  plots = list()
  for (j in 1:length(gs)) {
    for (i in names(p)) {
    p = autoplot(gs[[1]], bins = 128)
      p[[i]] = p[[i]] + theme_classic() + theme_cowplot()
    }
    plots[[j]] = ggcyto_arrange(p, nrow = 1)
  }
  plot(do.call(gridExtra::gtable_rbind,plots))
```

![**Figure 5**: Sample 1 of 6 from Cytobank experiment 43281](Using_CytoML_files/figure-html/acs_load-1.png)

```r
cap = c("**Figure 5**: Sample 1 of 6 from Cytobank experiment 43281")
```



# References

