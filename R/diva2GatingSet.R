#' divaWorkspace class
#' Inherited from \link{flowJoWorkspace-class}
#' @importClassesFrom flowWorkspace flowJoWorkspace
#' @exportClass divaWorkspace
setClass("divaWorkspace", contains = "flowJoWorkspace")
# copied from "openWorkspace" method (flowWorkspace/diva branch)
#' open Diva xml workspace
#'
#' @param file xml file
#' @param options argument passed to \link{xmlTreeParse}
#' @param ... arguments passed to \link{xmlTreeParse}
#' @return a \code{divaWorkspace} object
#' @examples
#' \dontrun{
#' library(flowWorkspace)
#' library(CytoML)
#' ws <- openDiva(system.file('extdata/diva/PE_2.xml', package = "flowWorkspaceData"))
#' ws
#' getSampleGroups(ws)
#' getSamples(ws)
#' gs <- parseWorkspace(ws, name = 2, subset = 1)
#' sampleNames(gs)
#' getNodes(gs)
#' plotGate(gs[[1]])
#' }
#' @export
#' @importFrom XML xmlTreeParse xpathApply xmlGetAttr
openDiva <- function(file,options = 0,...){
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
  rootNode <- names(xmlChildren(x))

  ver <- xpathApply(x, paste0("/", rootNode),function(x)xmlGetAttr(x,"version"))[[1]]
  if(rootNode == "Workspace"){
    x<-new("flowJoWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x, options = as.integer(options))
    x@.cache$flag <- TRUE
  }else if(rootNode == "bdfacs"){
    x <- new("divaWorkspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x, options = as.integer(options))
    x@.cache$flag <- TRUE
  }else
    stop("Unrecognized xml root node: ", rootNode)

  return(x);
}

#' @rdname divaWorkspace-class
#' @param x divaWorkspace
#' @importFrom flowWorkspace getSamples
#' @export
setMethod("getSamples","divaWorkspace",function(x){
      selectMethod("getSampleGroups","divaWorkspace")(x)
    })

#' @rdname divaWorkspace-class
#' @importFrom flowWorkspace getSampleGroups
#' @export
setMethod("getSampleGroups","divaWorkspace",function(x){
      .getSampleGroupsDiva(x)
    })

#' @importFrom plyr ldply
.getSampleGroupsDiva<-function(x){
    ldply(
        xpathApply(x@doc, "/bdfacs/experiment/specimen",function(specimen){
              samples <- xpathApply(specimen, "tube",function(tube){
                                            c(tube = xmlGetAttr(tube,"name")
                                                , name = xmlValue(xmlElementsByTagName(tube,"data_filename")[[1]])
                                            )
                                  })

              samples <- ldply(samples)
              samples[["specimen"]] <- xmlGetAttr(specimen, "name")
              samples
            })
      )

}

#' @rdname divaWorkspace-class
#' @param object divaWorkspace
#' @importFrom flowWorkspace show
#' @export
setMethod("show",c("divaWorkspace"),function(object){
      cat("Diva Workspace Version ",object@version,"\n");
      cat("File location: ",object@path,"\n");
      cat("File name: ",object@file,"\n");
      if(object@.cache$flag){
        cat("Workspace is open.","\n");
        cat("\nGroups in Workspace\n");

        sg <- getSampleGroups(object)

        tbl <- data.frame(table(sg$specimen))
        colnames(tbl) <- c("specimen", "samples")
        print(tbl)
      }else{
        cat("Workspace is closed.","\n")
      }
    })

#' @rdname divaWorkspace-class
#' @param obj divaWorkspace
#' @param ... other arguments
#' @importFrom flowWorkspace parseWorkspace
#' @export
setMethod("parseWorkspace",signature("divaWorkspace"),function(obj, ...){
      .preprocessorDiva(obj, ...)
    })

.preprocessorDiva<- function(obj, name = NULL
                                    , subset = NULL
                                    , path = obj@path
                                    , ...)
{

  #sample info
  sg <- getSamples(obj)

  # filter by group name
  sg[["specimen"]] <- factor(sg[["specimen"]])
  groups <- levels(sg[["specimen"]])

  if(is.null(name)){
    message("Choose which group of samples to import:\n");
    groupInd <- menu(groups,graphics=FALSE);
  }else if(is.numeric(name)){
    if(length(groups)<name)
      stop("Invalid sample group index.")
    groupInd <- name
  }else if(is.character(name)){
    if(is.na(match(name,groups)))
      stop("Invalid sample group name.")
    groupInd <- match(name,groups)
  }
  group.name <- groups[groupInd]

  sg <- subset(sg, specimen == group.name)
#    browser()
  #filter by subset (sample name or numeric index)
  if(is.factor(subset)){
    subset<-as.character(subset)
  }
  if(is.character(subset)){
    sg <- subset(sg, name %in% subset)
  }else if(is.numeric(subset))
    sg <- sg[subset, ]

  #check if there are samples to parse
  sn <- sg[["name"]]
  nSample <- length(sn)
  if(nSample == 0)
    stop("No samples in this workspace to parse!")


  #check duplicated sample names

  isDup <- duplicated(sn)
  if(any(isDup))
    stop("Duplicated sample names detected within group: ", paste(sampleSelected[isDup], collapse = " "), "\n Please check if the appropriate group is selected.")


  message("Parsing ", nSample," samples");
  .parseDivaWorkspace(xmlFileName=file.path(obj@path,obj@file)
                      ,samples = sn
                      , groupName = group.name
                      ,path=path
                      ,xmlParserOption = obj@options
                      ,ws = obj
                      ,...)

}

#' @importFrom XML xpathSApply
#' @importFrom flowCore read.FCS transformList spillover logicleTransform
#' @importFrom flowWorkspace set.count.xml
#' @param scale_level indicates whether the gate is scaled by tube-level or gate-level biexp_scale_value (for debug purpose, May not be needed.)
.parseDivaWorkspace <- function(xmlFileName,samples,path,xmlParserOption, ws, groupName, scale_level = "gate", verbose = FALSE, ...){

  scale_level <- match.arg(scale_level, c("gate", "tube"))
  if(!file.exists(xmlFileName))
    stop(xmlFileName," not found!")
#  gs <- new("GatingSet", guid = .uuid_gen(), flag = FALSE)


  dataPaths <- vector("character")
  excludefiles<-vector("logical")
  for(file in samples){

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

      dataPaths<-c(dataPaths,dirname(absPath))
      excludefiles<-c(excludefiles,FALSE);
    }
  }
  #Remove samples where files don't exist.
  if(length(which(excludefiles))>0){
    message("Removing ",length(which(excludefiles))," samples from the analysis since we can't find their FCS files.");
    samples<-samples[!excludefiles];
  }


  files<-file.path(dataPaths,samples)

  if(length(files)==0)
    stop("not sample to be added to GatingSet!")

  #load the raw data from FCS
  fs <- read.ncdfFlowSet(files,isWriteSlice=TRUE,...)
  cnd <- colnames(fs)

  rootDoc <- ws@doc

  xpathGroup <- paste0("/bdfacs/experiment/specimen[@name='", groupName, "']")
  #determine the magic number to replace neg value for log10 trans
  log_decade <- xmlValue(rootDoc[["/bdfacs/experiment/log_decades"]])
  if(log_decade == "4")
    min_val <- 26
  else if(log_decade == "5")
    min_val <- 2.6
  else
    stop("unsupported decade: ", log_decade)
   # parse compp & trans
  translist <- complist <- list()
  for(file in files)
  {


        sampleName <- basename(file)

        #get tube node
        xpathSample <- paste0(xpathGroup, "/tube[data_filename='", sampleName, "']")
        sampleNode <- xpathApply(rootDoc, xpathSample)[[1]]

        # get comp & param for biexp
        biexp_para <- new.env(parent = emptyenv())
        use_auto_biexp_scale <- as.logical(xmlValue(sampleNode[["instrument_settings"]][["use_auto_biexp_scale"]]))
        biexp_scale_node <- ifelse(use_auto_biexp_scale, "comp_biexp_scale", "manual_biexp_scale")

        comp <- xpathApply(sampleNode, "instrument_settings/parameter", function(paramNode, biexp_para){

          paramName <- xmlGetAttr(paramNode, "name")

          isComp <- as.logical(xmlValue(xmlElementsByTagName(paramNode, "is_log")[[1]]))
          if(isComp){

            #get biexp para

            biexp_para[[paramName]] <- c(min = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "min")[[1]]))
                                          , max = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "max")[[1]]))
                                          , biexp_scale = as.numeric(xmlValue(xmlElementsByTagName(paramNode, biexp_scale_node)[[1]]))
                                          )
            #get comp
            coef <- as.numeric(xpathSApply(paramNode, "compensation/compensation_coefficient", xmlValue))
            # browser()
            res <- list(coef)
            names(res) <- paramName

          }else
            res <- NULL
            return(res)
        }, biexp_para = biexp_para)
        #comp stored in xml seems to be incorrect

        ##################################
        #parse comp
        ##################################

        data <- fs[[sampleName]]
        # message("loading data: ",file);
        # data <- read.FCS(file)[, cnd]
        #
        #we use the spillover from FCS keyword
        comp <- spillover(data)
        comp <- compact(comp)
        if(length(comp) > 1)
          stop("More than one spillover found in FCS!")
        else if(length(comp) == 0)
          stop("No spillover found in FCS!")
        else
          complist[[sampleName]] <- comp[[1]]

        #parse trans
        params <- names(biexp_para)
        # browser()
        #transform data in default flowCore logicle or log10 scale
        trans <- sapply(params, function(pn){
          this_para <- biexp_para[[pn]]
          maxValue <- 262144#TODO:this_para[["max"]]^10
          pos <- 4.5
          r <- abs(this_para[["biexp_scale"]])
          trans <- generate_trans(maxValue, pos, r)
                    }
          , simplify = FALSE)

        translist[[sampleName]] <- transformerList(params, trans)

    }


  gs <- GatingSet(fs)
  message("Compensating")
  gs <- compensate(gs, complist)

  message("computing data range")
  data.ranges <- sapply(sampleNames(gs), function(sn)range(getData(gs[[sn]]), "data"), simplify = FALSE)

  message(paste("transforming ..."))
  gs <- transform(gs, translist)

    message("parsing gates ...")
  for(sn in sampleNames(gs)){
    gh <- gs[[sn]]

    this_biexp <- translist[[sn]]
    xpathSample <- paste0(xpathGroup, "/tube[data_filename='", sampleName, "']")
    sampleNode <- xpathApply(rootDoc, xpathSample)[[1]]
    #assume the gates listed in xml follows the topological order
    rootNode.xml <- NULL
    gateNodes <- xpathApply(sampleNode, "gates/gate")
    for(gateNode in gateNodes)
    {
      nodeName <- xmlGetAttr(gateNode, "fullname")
      nodeName <- gsub("\\\\", "/", nodeName)
      nodeName <- basename(nodeName)
      if(verbose)
        message(nodeName)
      count <- as.integer(xmlValue(xmlElementsByTagName(gateNode, "num_events")[[1]]))
      parent <- xmlElementsByTagName(gateNode, "parent")
      if(length(parent) > 0){
        parent <- xmlValue(parent[[1]])
        parent <- gsub("\\\\", "/", parent)
        parent <- gsub(rootNode.xml, "root", parent)


        regionNode <- xmlElementsByTagName(gateNode, "region")[[1]]
        xParam <- xmlGetAttr(regionNode, "xparm")
        yParam <- xmlGetAttr(regionNode, "yparm")
        gType <- xmlGetAttr(regionNode, "type")

        #parse the coodinates
        mat <- xpathSApply(regionNode, "points/point", function(pointNode)as.numeric(xmlAttrs(pointNode)))
        #rescale the gate if it is stored as unscaled value
        is.x.scaled <- as.logical(xmlValue(xmlElementsByTagName(gateNode, "is_x_parameter_scaled")[[1]]))
        is.y.scaled <- as.logical(xmlValue(xmlElementsByTagName(gateNode, "is_y_parameter_scaled")[[1]]))

        x_parameter_scale_value <- as.integer(xmlValue(xmlElementsByTagName(gateNode, "x_parameter_scale_value")[[1]]))
        y_parameter_scale_value <- as.integer(xmlValue(xmlElementsByTagName(gateNode, "y_parameter_scale_value")[[1]]))


        x_biexp <- this_biexp[[xParam]][["transform"]]
        y_biexp <- if(is.null(yParam)) NULL else this_biexp[[yParam]][["transform"]]
        #the gate may be either stored as simple log or 4096 scale
        #we need to rescale them to the data scale (i.e. 4.5 )
        if(is.null(yParam))
          bound <- matrix(c(-Inf,Inf), byrow = TRUE, nrow = 1, dimnames = list(c(xParam), c("min", "max")))
        else
          bound <- matrix(c(-Inf,Inf,-Inf,Inf), byrow = TRUE, nrow = 2, dimnames = list(c(xParam, yParam), c("min", "max")))
        x.extend <- y.extend <- FALSE
        if(is.x.scaled)#if the gate is scaled to 4096
        {
          mat[1, ] <- mat[1, ]/4096
          if(!is.null(x_biexp))
          {
            #when channel is logicle scale
            mat[1, ] <- mat[1, ] * 4.5 # restore it to te logicle scale
            #rescale gate to data scale
            if(scale_level=="gate")
            {
              trans.gate <- generate_trans(r = x_parameter_scale_value)
              mat[1, ] <- trans.gate$inverse(mat[1, ])
              mat[1, ] <- x_biexp(mat[1, ])
            }
          }
          else
            mat[1, ] <- mat[1, ] * 262144
        }else
        {
          if(!is.null(x_biexp))#it was in log scale
          {
            #restore to raw scale
            mat[1, ] <- 10 ^ mat[1, ]
            #set flag to trigger gate extension later
            x.extend <- TRUE
          }
        }

        if(is.y.scaled)#if the gate is scaled to 4096
        {
          mat[2, ] <- mat[2, ]/4096
          if(!is.null(y_biexp))
          {
            #when channel is logicle scale
            mat[2, ] <- mat[2, ] * 4.5
            #rescale gate to data scale
            if(scale_level=="gate")
            {
              trans.gate <- generate_trans(r = y_parameter_scale_value)
              mat[2, ] <- trans.gate$inverse(mat[2, ])
              mat[2, ] <- y_biexp(mat[2, ])
            }
          }
          else
            mat[2, ] <- mat[2, ] * 262144
        }else
        {
          if(!is.null(y_biexp))#it was in log scale
          {
            #restore to raw scale
            mat[2, ] <- 10 ^ mat[2, ]
            #set flag to trigger gate extension later
            y.extend <- TRUE
          }
        }


        if(gType == "RECTANGLE_REGION"){
          x <- unique(mat[1,])
          y <- unique(mat[2,])
          if(length(x)!=2||length(y)!=2)
            stop("invalid RECTANGLE_REGION from ", nodeName)
          coord <- list(x,y)
          names(coord) <- c(xParam, yParam)
          gate <- rectangleGate(.gate = coord)
        }else if(gType == "POLYGON_REGION"){
          rownames(mat) <- c(xParam, yParam)
          gate <- polygonGate(.gate = t(mat))
        }else if(gType == "INTERVAL_REGION"){
          # browser()
          coord <- list(mat[1,])
          names(coord) <- xParam
          gate <- rectangleGate(coord)
        }else
          stop("unsupported gate type: ", gType)

        #deal with the off threshold data truncation in log10 scale scenario
        #we do gate extention instead to keep transformation consistent across gates(i.e always use biexp)
        if(x.extend||y.extend)
        {
          if(x.extend)
            bound[xParam, ] <- c(min_val, 262143)
          if(y.extend)
            bound[yParam, ] <- c(min_val, 262143)
          gate <- extend(gate, bound, t(data.ranges[[sn]]))
          #need transform since extention was performed on the raw-scale gate
          if(x.extend)
            gate <- transform(gate, x_biexp@.Data, xParam)
          if(y.extend)
            gate <- transform(gate, y_biexp@.Data, yParam)
        }




        add(gh, gate, parent = parent, name = nodeName)
        if(parent == "root")
          parent <- ""
        unique.path <- file.path(parent, nodeName)
        suppressMessages(recompute(gh, unique.path))
        #save the xml counts
        set.count.xml(gh, unique.path, count)
      }else{
        rootNode.xml <- nodeName
        if(rootNode.xml!="All Events")
          stop("unrecognized root node: ", rootNode.xml)
        set.count.xml(gh, "root", count)
        next
      }


    }


  }





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

#use the equation suggested by BD engineer last year
generate_trans <- function(maxValue = 262144, pos = 4.5, r)
{
  if(r == 0)
    r <- maxValue/10^pos
  w <- (pos - log10(maxValue/r))/2
  if(w < 0)
    w <- 0
  logicle_trans(w=w, t = maxValue, m = pos) #
}