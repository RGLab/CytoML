openWinlist <- function(file,options = 0,...){
  #message("We do not fully support all features found in a flowJo workspace, nor do we fully support all flowJo workspaces at this time.")
  tmp<-tempfile(fileext=".xml")
  if(!file.exists(file))
    stop(file, " not found!")
  if(!file.copy(file,tmp))
    stop("Can't copy ", file, " to ", tmp)

  if(inherits(file,"character")){
    x<-xmlTreeParse(tmp,useInternalNodes=TRUE,options = options, ...);
  }else{
    stop("Require a filename of a workspace, but received ",class(x)[1]);
  }
  #    browser()
  rootNode <- names(xmlChildren(x))[2]

  if(rootNode == "WinListXMLProtocol"){
    return(xmlRoot(x));
  }else
    stop("Unrecognized xml root node: ", rootNode)


}
winlist2GatingSet <- function(xmlFileName, path, ...){
  root <- openWinlist(xmlFileName, ...)
  files <- xpathSApply(root, "DataSources/DataSource/File/text()", xmlValue)

  dataPaths <- vector("character")
  excludefiles <- vector("logical")

  for(file in files){

    #########################################################
    #get full path for each fcs
    #########################################################
    ##escape "illegal" characters

    file<-gsub("\\?","\\\\?",gsub("\\]","\\\\]",gsub("\\[","\\\\[",gsub("\\-","\\\\-",gsub("\\+","\\\\+",gsub("\\)","\\\\)",gsub("\\(","\\\\(",file)))))))
    absPath <- list.files(pattern=paste("^",file,"",sep=""),path=path,recursive=TRUE,full.names=TRUE)
    nFound <- length(absPath)
    if(nFound == 0){
      warning("Can't find ",file," in directory: ",path,"\n");
      excludefiles<-c(excludefiles,TRUE);

    }else if(nFound > 1){
      stop('Multiple files found for:', file)
    }else{

      dataPaths<-c(dataPaths,absPath)
      excludefiles<-c(excludefiles,FALSE);
    }
  }
  #Remove samples where files don't exist.
  if(length(which(excludefiles))>0){
    message("Removing ",length(which(excludefiles))," samples from the analysis since we can't find their FCS files.");
    dataPaths<-dataPaths[!excludefiles];
  }




  if(length(dataPaths)==0)
    stop("not sample to be added to GatingSet!")

  #load the raw data from FCS
  fs <- read.ncdfFlowSet(dataPaths,isWriteSlice=FALSE,...)

  # biexp_list <- new.env(parent = emptyenv())
  for(file in dataPaths)
  {

    cnd <- colnames(fs)

    message("loading data: ",file);
    data <- read.FCS(file)[, cnd]

    sampleName <- basename(file)

    #get tube node
    xpathSample <- paste0("DataSources/DataSource/File[text()='", sampleName, "']/..")
    sampleNode <- xpathApply(root, xpathSample)[[1]]

    #parse parameterID vs paramName from DataSource/Properties
    paramEnv <- new.env(parent = emptyenv())
    paramNodes <- xpathApply(sampleNode, "Properties/*[starts-with(name(), 'ParamBaseName')]")
    for(pn in paramNodes)
    {
      paramID <- xmlName(pn)
      paramName <- getContent(pn)
      paramEnv[[paramID]] <- paramName
    }
    # #TODO: parse comp
    # biexp_para <- new.env(parent = emptyenv())
    # comp <- xpathApply(sampleNode, "instrument_settings/parameter", function(paramNode, biexp_para){
    #
    #   paramName <- xmlGetAttr(paramNode, "name")
    #
    #   isComp <- as.logical(xmlValue(xmlElementsByTagName(paramNode, "is_log")[[1]]))
    #   if(isComp){
    #
    #     #get biexp para
    #
    #     biexp_para[[paramName]] <- c(min = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "min")[[1]]))
    #                                  , max = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "max")[[1]]))
    #                                  , biexp_scale = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "comp_biexp_scale")[[1]]))
    #     )
    #     #get comp
    #     coef <- as.numeric(xpathSApply(paramNode, "compensation/compensation_coefficient", xmlValue))
    #     # browser()
    #     res <- list(coef)
    #     names(res) <- paramName
    #
    #   }else
    #     res <- NULL
    #   return(res)
    # }, biexp_para = biexp_para)


    ##################################
    #Compensating the data
    ##################################


    message("Compensating");
    #we use the spillover from FCS keyword
    comp <- spillover(data)
    comp <- compact(comp)
    if(length(comp) > 1)
      stop("More than one spillover found in FCS!")
    else if(length(comp) == 0)
      stop("No spillover found in FCS!")
    else
      comp <- comp[[1]]
    data <- compensate(data,comp)



    message(paste("transforming ..."))

    # params <- names(biexp_para)
    params <- colnames(comp)

    # browser()
    #transform data in default flowCore logicle scale
    # trans <- sapply(params, function(pn){
    #   this_para <- biexp_para[[pn]]
    #   maxValue <- 262144
    #   pos <- 4.5
    #   r <- abs(this_para[["biexp_scale"]])
    #   w = (pos - log10(maxValue/r))/2
    #   lgclObj  <- logicleTransform(w=w, t = maxValue, m = pos) #
    # }
    # , simplify = FALSE)

    #transform with default hyperlog
    trans <- sapply(params, function(pn){
        function(x){
          #TODO: change hyperlogGml2 eval method to avoid directly call this
          flowCore:::hyperlog_transform(x, T = 262144, W = 0.5, M = 4.5, A=0, FALSE)
        }
      }, simplify = FALSE)

    translist <- transformList(params, trans)
    data <- transform(data, translist)

    # browser()
    fs[[sampleName]] <- data

    # biexp_list[[sampleName]] <- trans

    #parse population objects from Gate nodes
    popEnv <- new.env(parent = emptyenv());
    popNodes <- xpathApply(sampleNode, "Gates/GateDefinition")
    for(popNode in popNodes)
    {
      if(getContent(popNode, "Active") == "1")
      {
        popId <- getContent(popNode, "GateID")
        parentId <- getContent(popNode, "ParentGateID")
        if(parentId != popId)#skip root
        {
          popEnv[[popId]] <- list(parentId = parentId
                                  , NumEvents = getContent(popNode, "NumEvents")
                                  , Equation = getContent(popNode, "Equation")
                                  )
        }
      }

    }

    #parse gates (with abstract dims) from region node
    gateEnv <- new.env(parent = emptyenv());
    gateNodes <- xpathApply(sampleNode, "Regions/RegionDefinition/DragRegion/Properties")
    for(gateNode in gateNodes)
    {
        gateID <- getContent(gateNode, "RegionID")

        #special logic for parsing quadgate
        NStatColumns <- gateNode[["NStatColumns"]]
        NStatRows <- gateNode[["NStatRows"]]
        if(!is.null(NStatRows)&&!is.null(NStatColumns))
        {
          #get name from SubRegionAlias
          sub_name <- paste0("SubRegionAlias", gateID)
          gateName <- getContent(gateNode, sub_name)
        }else
          gateName <- getContent(gateNode, "RegionAlias")

        vertNodes <- xpathApply(gateNode, "*[starts-with(name(), 'LinVertex')]")

        mat <- sapply(vertNodes, function(vn){
                    as.numeric(getContent(vn))
                 })

        gType = getContent(gateNode, "Description")
        if(gType == "2 parameter polygon region"){
          rownames(mat) <- c("x", "y")
          gate <- polygonGate(.gate = t(mat))
        }else if(gType == "1 parameter region"){
          # browser()
          coord <- list(unique(mat[1,]))
          names(coord) <- "x"
          gate <- rectangleGate(coord)
        }else
          stop("unsupported gate type: ", gType)

        gateEnv[[gateID]] <- list(gateName = gateName, gate = gate)

    }


    #parse histograms

    #parse equations/bool expresions from pop to link to the actual gate object

    #link pop to histogram using GateID to get dimID





  }


  gs <- GatingSet(fs)


  #   add(gh, gate, parent = parent, name = nodeName)
  #   if(parent == "root")
  #     parent <- ""
  #   unique.path <- file.path(parent, nodeName)
  #   recompute(gh, unique.path)
  #   #save the xml counts
  #   set.count.xml(gh, unique.path, count)
  # }else{
  #   rootNode.xml <- nodeName
  #   if(rootNode.xml!="All Events")
  #     stop("unrecognized root node: ", rootNode.xml)
  #   set.count.xml(gh, "root", count)
  #   next
  # }








  message("done!")



  #we don't want to return the splitted gs since they share the same cdf and externalptr
  #thus should be handled differently(more efficiently) from the regular gslist

  #    # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed
  gslist <- suppressMessages(flowWorkspace:::.groupByTree(gs))
  if(length(gslist) > 1)
    warning("GatingSet contains different gating tree structures and must be cleaned before using it! ")
  #    if(length(gslist) == 1){
  #      return (gslist[[1]])
  #    }else
  {
    #      warning("Due to the different gating tree structures, a list of GatingSets is returned instead!")
    #      return (gslist)
    }
  gs

}

#' parse the actual content from json text
getContent <- function(xmlnode, name = NULL){
  if(!is.null(name))
    xmlnode <- xmlnode[[name]]
  x <- xmlValue(xmlnode)
  strsplit(x, ",")[[1]][-1]
}