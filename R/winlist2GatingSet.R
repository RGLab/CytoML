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

  sampleEnv <- new.env(parent = emptyenv())
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
      paramEnv[[paramID]] <- getChannelMarker(data, paramName)[["name"]]
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
        # if(parentId != popId)#skip root
        # {
          popEnv[[popId]] <- list(parentId = parentId
                                  , NumEvents = getContent(popNode, "NumEvents")
                                  , Equation = getContent(popNode, "Equation")
                                  )
        # }
      }

    }

    #parse histograms
    histEnv <- new.env(parent = emptyenv());
    histNodes <- xpathApply(sampleNode, "Histograms/Histogram")
    for(histNode in histNodes)
    {
      HistID <- getContent(histNode, "HistID")
      xParamID <- getContent(histNode, "XParam")
      xParam <- paramEnv[[paste0("ParamBaseName", xParamID)]]
      yParamID <- getContent(histNode, "YParam")
      yParam <- paramEnv[[paste0("ParamBaseName", yParamID)]]

      histEnv[[HistID]] <- list(x = xParam, y = yParam)

    }

    #parse gates (with abstract dims) from region node
    gateEnv <- new.env(parent = emptyenv());
    gateNodes <- xpathApply(sampleNode, "Regions/RegionDefinition")
    for(gateNode in gateNodes)
    {

        HistogramID <- getContent(gateNode[["Properties"]], "HistogramID")
        gateID <- getContent(gateNode[["Properties"]], "RegionID")
        gateNode <- gateNode[["DragRegion"]][["Properties"]]
        if(is.null(gateNode))
          next

        #special logic for parsing quadgate
        NStatColumns <- gateNode[["NStatColumns"]]
        NStatRows <- gateNode[["NStatRows"]]
        isQadGate <- FALSE
        if(!is.null(NStatRows)&&!is.null(NStatColumns))
        {
          #get name from SubRegionAlias
          sub_name <- paste0("SubRegionAlias", gateID)
          gateName <- getContent(gateNode, sub_name)
          isQadGate <- TRUE
        }else
          gateName <- getContent(gateNode, "RegionAlias")

        vertNodes <- xpathApply(gateNode, "*[starts-with(name(), 'LinVertex')]")

        mat <- sapply(vertNodes, function(vn)as.numeric(getContent(vn)))
        vertID <- sapply(vertNodes, function(vn)as.numeric(sub("LinVertex", "", xmlName(vn))))
        mat <- mat[, order(vertID)]#sort by ID order


        gType = getContent(gateNode, "Description")

        #link gate to histogram using fill dims
        params <- histEnv[[HistogramID]]
        if(is.null(params))
          stop("unable to find the histogram related to this gate:", gateName)

        if(gType == "2 parameter polygon region" || (isQadGate && gType == "Collection of regions")){

          rownames(mat) <- params
          gate <- polygonGate(.gate = t(mat))
        }else if(gType == "1 parameter region"){
          # browser()
          coord <- list(unique(mat[1,]))
          names(coord) <- params[["x"]]
          gate <- rectangleGate(coord)
        }else{
          stop("unsupported gate type: ", gType)
        }

        gateID <- as.character(as.integer(gateID)+1)#increment id for it is referenced from pop differently
        gateEnv[[gateID]] <- list(gateName = gateName, gate = gate)

    }




    #parse equations/bool expresions from pop to link to the actual gate object
    dt <- rbindlist(as.list(popEnv), idcol = "id")

    res <- new.env(parent = emptyenv())
    traverseTree(dt[id>0,], pid= 0, pEquation = "", gateEnv, res)#start from root
    # merge back to dt
    dt <- merge(dt, rbindlist(as.list(res), idcol = "id"), all.x = TRUE)

    sampleEnv[[sampleName]] <- list(pops = dt, gateEnv = gateEnv, trans = trans)
  }

  #add gates to gs
  gs <- GatingSet(fs)

  for(sn in sampleNames(gs))
  {

    .addPop(gs[[sn]], pid = 0, parentPath = "root", sampleEnv[[sn]])
  }




  message("done!")

  #we don't want to return the splitted gs since they share the same cdf and externalptr
  #thus should be handled differently(more efficiently) from the regular gslist

  #    # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed
  gslist <- suppressMessages(flowWorkspace:::.groupByTree(gs))
  if(length(gslist) > 1)
    warning("GatingSet contains different gating tree structures and must be cleaned before using it! ")
  gs

}

.addPop <- function(gh, pid, parentPath, popInfo){
  trans <- popInfo[["trans"]]
  dt <- popInfo[["pops"]]
  gateEnv <- popInfo[["gateEnv"]]
  children <- dt[parentId == pid, ]
  for(i in seq_len(nrow(children)))
  {
    gateID <- children[i, gateID]
    cid <-  children[i, id]
    NumEvents <- children[i, NumEvents]
    if(cid > 0)#skip root node
    {
      gate <- gateEnv[[gateID]]
      gateName <- gate[["gateName"]]
      gate <- gate[["gate"]]
      gate <- .transformGate(gate, trans)
      negated <- children[i, negated]
      add(gh, gate, parent = parentPath, name = gateName, negated = negated)
      if(parentPath == "root")
        parentPath <- ""

      unique.path <- file.path(parentPath, gateName)
      recompute(gh, unique.path)
    }else
      unique.path <- "root"

    #save the xml counts
    set.count.xml(gh, unique.path, as.integer(NumEvents))
    if(cid > 0)
      .addPop(gh, cid, unique.path, popInfo)#recursively gate descendants

  }
}

.transformGate <- function(gate, trans){

  params <- as.vector(parameters(gate))
  chnls <- names(trans)

  for(i in seq_along(params)){
    param <- params[i]
    ind <- sapply(chnls, function(chnl)grepl(chnl, param), USE.NAMES = FALSE)
    nMatched <- sum(ind)
    if(nMatched == 1){
      chnl <- chnls[ind]
      trans.fun <- trans[[chnl]]
      gate <- transform(gate, trans.fun, param)

    }else if(nMatched > 1)
      stop("multiple trans matched to :", param)
  }

  # round
  gate

}

#' resursively parse gateID from equations
#' @param dt a data.table that stores hierarchical structure of gate defintions
#' @param pid the parent gate ID that restrains the subsets to process
#' @param pEquation the gate definiton of the parent gate
#' @param gateEnv the environment that stores the actual gate objects to be used to validate the parsing
#' @param res the working environment to hold the parsing results
traverseTree <- function(dt, pid, pEquation, gateEnv, res){
  children <- dt[parentId == pid, ]#extract the children gates to be processed
  nChildren <- nrow(children)
  for(i in seq_len(nChildren))
  {
    cid <- children[i, id]
    Equation <- children[i, Equation]
    #strip the leading string that represents the parent gate defintion
    def <- sub(paste0("^", pEquation,"&"), "", Equation)
    if(grepl("\\&", def))
      stop("The gate should not have '&' operators!", def)
    #deal with ! symbol
    negated <- FALSE
    if(grepl("\\!", def))
    {
      negated <- TRUE
      def <- sub("\\!", "", def)
    }
    #extract id
    gateID <- sub("R", "", def)

    if(!exists(gateID, gateEnv))
      stop("Can't find gate for R",gateID, " parsed from ", Equation)
    res[[cid]] <- list(gateID = gateID, negated = negated)
    #recursively to process its children
    traverseTree(dt, pid = cid, Equation, gateEnv, res)

  }

}
#' parse the actual content from json text
getContent <- function(xmlnode, name = NULL){
  if(!is.null(name))
    xmlnode <- xmlnode[[name]]
  x <- xmlValue(xmlnode)
  strsplit(x, ",")[[1]][-1]
}