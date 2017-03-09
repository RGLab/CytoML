#' Convert a GatingSet to winlist protocol
#'
#'
#' @param gs a GatingSet object
#' @param outFile the workspace file path to write
#' @param ... other arguments
#'        showHidden whether to include the hidden population nodes in the output
#' @export
#' @importFrom flowWorkspace clone updateChannels
#' @return nothing
#' @examples
#' library(flowWorkspace)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' #output to winlist
#' outFile <- tempfile(fileext = ".wlx")
#' GatingSet2winlist(gs, outFile)
#'
#'
GatingSet2winlist <- function(gs, outFile, ...){

  chnls <- colnames(gs)
  slash_loc <- sapply(chnls, function(thisCol)as.integer(gregexpr("/", thisCol)[[1]]), simplify = FALSE)
  new_cnd <- flowWorkspace:::.fix_channel_slash(chnls, slash_loc)
  if(!all(new_cnd == chnls)){
    gs <- clone(gs, isNew = FALSE, isEmpty = FALSE) # ensure everything else is cloned except hdf5
    gs <- updateChannels(gs, map = data.frame(old = chnls, new = new_cnd))
  }

  pData(gs)[["name"]] <- as.character(pData(gs)[["name"]]) #coerce factor to character
  # load template
  #avoid internal XML doc since its manipulation is not stable and leads to segfault
  template <- openWinlist("/loc/no-backup/mike/shared/tcell/template.wlx", useInternalNodes = FALSE)
  ws <- wlxNode(template, gs)
  locale <- localeToCharset()[1]
  if(locale == "ISO8859-1")
    locale <- "ISO-8859-1"
  ## Write out to an XML file (suppress the warning due to the usage of deprecated structure call in saveXML)
  suppressWarnings(saveXML(ws, file=outFile, prefix=sprintf("<?xml version=\"1.0\" encoding=\"%s\"?>", locale)))
}

wlxNode <- function(ws, ...){
  xmlValue(ws[["ObjectVersion"]]) <- 1
  xmlValue(ws[["Date"]]) <- format(Sys.Date(), "%d%b%Y")
  xmlValue(ws[["Time"]]) <- format(Sys.time(), "%H:%M:%S")
  ws[["DataSources"]] <- datasourceNode(ws[["DataSources"]], ...)
  ws
}

datasourceNode <- function(ds, gs, ...){
  guid <- sampleNames(gs)
  nSample <- length(guid)
  # pData(gs)[["name"]]
  xmlValue(ds[["NumDataSources"]]) <- nSample

  dsNodes <- lapply(seq_along(guid), function(i){
    id <- guid[i]
    dsNode(ds[["DataSource"]], gs[[id]], ds_id = i)
  })
  #empty the old ones before adding new
  ds <- removeChildren(ds, kids = list("DataSource"))
  ds <- addChildren(node = ds, kids = dsNodes)
  ds
}

dsNode <- function(ds, gh, ...){

  sn <- sampleNames(gh)
  xmlValue(ds[["File"]][[1]]) <- sn
  #update ds property node
  dsPropNode(ds[["Properties"]], gh, ...)
  #histogram node
  histEnv =  new.env(parent = emptyenv())
  dsHistNode(ds[["Histograms"]], gh, histEnv, ...)
  #Regions node
  dsRegionNode(ds[["Regions"]], gh, histEnv, ...)
  ds
}

dsRegionNode <- function(dsRegion, gh, histEnv, ...){
  nodes <- getNodes(gh)[-1]
  prop <- dsRegion[["Properties"]]
  xmlValue(prop[["NumRegions"]]) <- paste0("2,", length(nodes))
  tmpNode <- dsRegion[["RegionDefinition"]]
  regionList <- lapply(nodes, function(node){

    #update Properties node
    histID <- histEnv[[node]]
    xmlValue(tmpNode[["Properties"]][["HistogramID"]]) <- paste0("2,", histID)
    gateID <- flowWorkspace:::.getNodeInd(gh, node) - 1
    xmlValue(tmpNode[["Properties"]][["RegionID"]]) <- paste0("2,", gateID)
    tmpNode[["Properties"]][["Name"]] <- xmlNode("Name", xmlCDataNode(paste0("1,R", gateID+1)))
    tmpNode[["Properties"]][["Path"]] <- xmlNode("Path", xmlCDataNode(paste0("1,/R", gateID+1)))
    #update DragRegion node
    tmpNode[["RegionDefinition"]] <- dragRegionNode(tmpNode[["DragRegion"]], gh, node)
    tmpNode
  })
  xmlNode("Regions"
          , .children = c(list(prop)
                          , regionList)
  )

}

dragRegionNode<- function(dRegion, gh, node, ...){
  gateID <- flowWorkspace:::.getNodeInd(gh, node) - 1
  dProp <- dRegion[["Properties"]]
  xmlValue(dProp[["RegionID"]]) <- paste0("2,", gateID)
  dProp[["Name"]] <- xmlNode("Name", xmlCDataNode(paste0("1,R", gateID+1)))
  dProp[["Path"]] <- xmlNode("Path", xmlCDataNode(paste0("1,//R", gateID+1)))
  dProp[["RegionAlias"]] <- xmlNode("RegionAlias", xmlCDataNode(paste0("1,", basename(node))))
  xmlValue(dProp[["GatedEvents"]]) <- paste0("3,", getTotal(gh, node))
  dProp <- removeChildren(dProp, kids = lapply(0:7, function(i)paste0("LinVertex", i)))
  dProp <- removeChildren(dProp, kids = lapply(0:7, function(i)paste0("Vertex", i)))
  gate <- getGate(gh, node)
  #winlist does not support negated gate
  #we have to create inverse gate on our end
  if(flowWorkspace:::isNegated(gh, node)){
    rng <- range(getData(gh, use.exprs = FALSE))
    gate <- inverse(gate, rng)
  }
  #transform gate to raw
  orig.trans <- getTransformations(gh, inverse = TRUE)
  gate <- .scaleGate(gate, orig.trans)
  linVertexList <- gate2winlist(gate, type = "LinVertex")
  #trans gate to hyperlog
  chnls <- names(orig.trans)
  trans <- sapply(chnls, function(pn){
    maxValue <- 262144
    pos <- 4.5
    r <- 10
    w <- log10(r) #(pos - log10(maxValue/r))/2
    neg <- -20
    a <- log10(-neg)

    function(x){
      flowCore:::hyperlog_transform(x, T = maxValue, W = w, M = pos, A = a, FALSE)
    }

  }, simplify = FALSE)


  gate <- .scaleGate(gate, trans)
  vertexList <- gate2winlist(gate, type = "Vertex")

  dProp <- addChildren(dProp, kids = linVertexList)
  dProp <- addChildren(dProp, kids = vertexList)
  dRegion[["Properties"]] <- dProp
  dRegion
}

.scaleGate <- function(gate, trans)
{
  chnls <- names(trans)
  params <- as.vector(parameters(gate))

  for(i in seq_along(params)){
    param <- params[i]
    ind <- match(param, chnls)
    # ind <- sapply(chnls, function(chnl)grepl(chnl, param), USE.NAMES = FALSE)
    nMatched <- length(ind)
    if(nMatched == 1){
      if(is.na(ind))
        next

      trans.fun <- trans[[ind]]
      gate <- transform(gate, trans.fun, param)
    }else if(nMatched > 1)
    stop("multiple trans matched to :", param)
  }
  gate
}

gate2winlist <- function(gate, type, ...)UseMethod("gate2winlist")
gate2winlist.polygonGate <- function(gate, type = "LinVertex", ...){
  lapply(seq_len(nrow(gate@boundaries)), function(i){
    coord <- gate@boundaries[i,]
    xmlNode(paste0(type, i), paste0("8,", paste(coord, collapse = ",")))
  })
}

dsHistNode <- function(dsHist, gh, histEnv, ...){
  nodes <- getNodes(gh)[-1]
  fr <- getData(gh, use.exprs = FALSE)
  pd <- pData(parameters(fr))
  plots <- flowWorkspace:::.mergeGates(gh, nodes, bool = FALSE ,merge = TRUE)

  #generate hist node for each plot
  tmpNode <- dsHist[["Histogram"]]
  histList <- lapply(seq_along(plots), function(i){
    xmlValue(tmpNode[["HistID"]]) <- paste0("2,", i)
    obj <- plots[[i]]

    if(is.list(obj))
      children <- obj[["popIds"]]
    else
      children <- obj
    #store the mapping : node path vs histid
    for(child in children)
      histEnv[[child]] <- i
    parent <- getParent(gh, child)
    pid <- flowWorkspace:::.getNodeInd(gh, parent) - 1
    xmlValue(tmpNode[["GateID"]]) <- paste0("2,", pid)
    params <- as.vector(parameters(getGate(gh,child)))
    paramId <- sapply(params, function(param){
      sub("\\$P", "", rownames(subset(pd, name == param)))
    })
    if(length(params==1))
    {

      title <- paste0("1,", pid)
    }else
    {
      title <- paste0("2,", paste(params, collapse = " vs "))
      xmlValue(tmpNode[["YParam"]]) <- paste0("2,", paramId[2])
    }
    xmlValue(tmpNode[["XParam"]]) <- paste0("2,", paramId[1])
    tmpNode[["HistogramName"]] <- xmlNode("HistogramName", xmlCDataNode(title))
    tmpNode
  })

  xmlNode("Histograms"
          , .children = c(list(xmlNode("NumHistograms", length(plots)))
                          , histList)
          )



}

dsPropNode <- function(dsProp, gh, ds_id, ...){

  xmlValue(dsProp[["DataSourceID"]]) <- paste0("2,", ds_id)

  fr <- getData(gh, use.exprs = FALSE)
  pd <- pData(parameters(fr))
  nParam <- nrow(pd)

  xmlValue(dsProp[["NumNativeParameters"]]) <- paste0("2,", nParam)
  ids <- seq_len(nParam) - 1
  #ParamBaseName
  dsProp <- removeChildren(dsProp, kids = lapply(0:11, function(i)paste0("ParamBaseName", i)))
  dsProp <- addChildren(dsProp, kids = lapply(ids, function(id){
                                        marker <- as.vector(pd[id+1, "desc"])
                                        if(is.na(marker))
                                          marker <- as.vector(pd[id+1, "name"])

                                        xmlNode(name = paste0("ParamBaseName", id), xmlCDataNode(paste0("1,", marker)))
                                      })
                  )
  trans <- getTransformations(gh)
  logChnls <- names(trans)
  #MaxNegativeValue

  dsProp <- removeChildren(dsProp, kids = lapply(0:11, function(i)paste0("MaxNegativeValue", i)))
  dsProp <- addChildren(dsProp, kids = lapply(ids, function(id){
    chnl <- as.vector(pd[id+1,"name"])
    val <- ifelse(grepl("time", chnl, ignore.case = TRUE), 0, -20)
    xmlNode(name = paste0("MaxNegativeValue", id), xmlCDataNode(paste0("4,", val)))
  })
  )

  #BCoefficient

  dsProp <- removeChildren(dsProp, kids = lapply(0:11, function(i)paste0("BCoefficient", i)))
  dsProp <- addChildren(dsProp, kids = lapply(ids, function(id){

    xmlNode(name = paste0("BCoefficient", id), xmlCDataNode(paste0("4,", 10)))
  })
  )

  #ConvertParam0ToLog
  dsProp <- removeChildren(dsProp, kids = lapply(0:11, function(i)paste0("ConvertParam", i, "ToLog")))
  dsProp <- addChildren(dsProp, kids = lapply(ids, function(id){
    chnl <- as.vector(pd[id+1,"name"])
    val <- ifelse(grepl(chnl, logChnls), 2, 0)
    xmlNode(name = paste0("ConvertParam", id, "ToLog"), xmlCDataNode(paste0("2,", val)))
  })
  )


  dsProp
}