#' @include flowJoWorkspace_Methods.R 
NULL
#' An R representation of a BD FACSDiva workspace
#' 
#' Inherited from \link{flowjo_workspace-class}
#' 
#' @name diva_workspace-class
#' @aliases show,diva_workspace-method
#' 
#' @section Slots: 
#' \describe{
#'   \item{\code{version}:}{Object of class \code{"character"}. The version of the XML workspace. }
#'   \item{\code{file}:}{Object of class \code{"character"}. The file name. }
#'   \item{\code{.cache}:}{Object of class \code{"environment"}. An environment for internal use.  }
#' 	\item{\code{path}:}{Object of class \code{"character"}. The path to the file. }
#'   \item{\code{doc}:}{Object of class \code{"XMLInternalDocument"}. The XML document object. }
#'   \item{\code{options}:}{Object of class \code{"integer"}. The XML parsing options passed to \code{\link{xmlTreeParse}}. }
#'   }
#' 
#' @seealso 
#'   \code{\linkS4class{GatingSet}} 
#'   \code{\linkS4class{GatingHierarchy}}
#' 
#' @exportClass diva_workspace
setClass("diva_workspace" ,representation(version="character"
                                           , file="character"
                                           , .cache="environment"
                                           , path="character"
                                           , doc="XMLInternalDocument"
                                           , options="integer")
)

#' @templateVar old openDiva
#' @templateVar new open_diva_xml
#' @template template-depr_pkg
NULL

#' @export
openDiva <- function(...){
  .Deprecated("open_diva_xml")
  open_diva_xml(...)
}

#' open Diva xml workspace
#'
#' @name open_diva_xml
#' @aliases openDiva
#' @param file xml file
#' @param options argument passed to \link{xmlTreeParse}
#' @param ... arguments passed to \link{xmlTreeParse}
#' @return a \code{diva_workspace} object
#' @examples
#' \dontrun{
#' library(flowWorkspace)
#' library(CytoML)
#' ws <- open_diva_xml(system.file('extdata/diva/PE_2.xml', package = "flowWorkspaceData"))
#' ws
#' diva_get_sample_groups(ws)
#' gs <- diva_to_gatingset(ws, name = 2, subset = 1)
#' sampleNames(gs)
#' gs_get_pop_paths(gs)
#' plotGate(gs[[1]])
#' }
#' @importFrom XML xmlTreeParse xpathApply xmlGetAttr
#' @importFrom methods new
#' @export
open_diva_xml <- function(file,options = 0,...){
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
  unlink(tmp)
  #    browser()
  rootNode <- names(xmlChildren(x))

  ver <- xpathApply(x, paste0("/", rootNode),function(x)xmlGetAttr(x,"version"))[[1]]
  if(rootNode == "bdfacs"){
    x <- methods::new("diva_workspace",version=ver,.cache=new.env(parent=emptyenv()),file=basename(file),path=dirname(file),doc=x, options = as.integer(options))
    x@.cache$flag <- TRUE
  }else
    stop("Unrecognized xml root node: ", rootNode)

  return(x);
}


#' @export
diva_get_sample_groups <- function(x){
  do.call(rbind, 
        xpathApply(x@doc, "/bdfacs/experiment/specimen",function(specimen){
              samples <- xpathApply(specimen, "tube",function(tube){
                                            c(tube = xmlGetAttr(tube,"name")
                                                , name = xmlValue(xmlElementsByTagName(tube,"data_filename")[[1]])
                                            )
                                  })

              samples <- do.call(rbind, lapply(samples, function(i)data.frame(t(i))))
              samples[["specimen"]] <- xmlGetAttr(specimen, "name")
              samples
            })
      )

}

#' Get a table of samples from a FACSDiva workspace
#'
#' Return a data.frame of sample group information from a FACSDiva workspace
#' @name diva_get_samples
#' @aliases diva_get_sample_groups
#' @usage 
#' diva_get_samples(x)
#' diva_get_sample_groups(x)
#' @param x A \code{diva_workspace}
#' @return
#' A \code{data.frame} with columns \code{tub}, \code{name}, and \code{specimen}
#' @importFrom methods selectMethod
#' @export
diva_get_samples <- diva_get_sample_groups

get_global_sheets<-function(x){
  
    res <- xpathApply(x@doc, "/bdfacs/experiment/acquisition_worksheets/worksheet_template",function(t){
      
        xmlGetAttr(t,"name")
        
      })
    # res <- data.table(res)
    # setnames(res, "res", "name")
    unlist(res)
}

#' @importFrom flowWorkspace show
#' @export
setMethod("show",c("diva_workspace"),function(object){
      cat("Diva Workspace Version ",object@version,"\n");
      cat("File location: ",object@path,"\n");
      cat("File name: ",object@file,"\n");
      if(object@.cache$flag){
        cat("Workspace is open.","\n");
        cat("\nGroups in Workspace\n");

        sg <- diva_get_sample_groups(object)

        tbl <- data.frame(table(sg$specimen))
        colnames(tbl) <- c("specimen", "samples")
        tbl <- tbl[match(unique(sg$specimen), tbl$specimen),]
        print(tbl)
      }else{
        cat("Workspace is closed.","\n")
      }
    })

#' @templateVar old parseWorkspace
#' @templateVar new diva_to_gatingset
#' @template template-depr_pkg
NULL

#' @export
setMethod("parseWorkspace",signature("diva_workspace"),function(obj, ...){
    .Deprecated("diva_to_gatingset")
    diva_to_gatingset(obj, ...)
    })

#' Parse a FACSDiva Workspace
#' 
#' Function to parse a FACSDiva Workspace, generate a \code{GatingHierarchy} or \code{GatingSet} object, and associated flowCore gates.
#' @name diva_to_gatingset
#' @aliases parseWorkspace,diva_workspace-method 
#' @param obj diva_workspace
#' @param name sample group to be parsed, either numeric index or the group name
#' @param subset samples to be imported. either numeric index or the sample name. Default is NULL, which imports all samples.
#' @param worksheet select worksheet to import. either "normal" or "global"
#' @param path the FCS data path
#' @param swap_cols diva seems to swap some data cols during importing fcs to experiments
#' 					this argument provide a list to tell the parser which cols to be swapped
#' 					default is list(`FSC-H` = 'FSC-W',`SSC-H` = 'SSC-W')
#' @param verbose whether print more messages during the parsing
#' @param ... other arguments
#' @importFrom utils menu
#' @importFrom flowCore colnames<-
#' @importFrom flowWorkspace pop_add
#' @export
diva_to_gatingset<- function(obj, name = NULL
                                    , subset = NULL
                                    , path = obj@path
                                    , worksheet = c("normal", "global")
                                    , swap_cols = list(`FSC-H` = 'FSC-W'
                                                , `SSC-H` = 'SSC-W') #diva seems to swap these data cols during importing fcs to experiments
                                    , verbose = FALSE
                                    , ...)
{

  worksheet <- match.arg(worksheet)
  #sample info
  sg <- diva_get_samples(obj)

  # filter by group name
  sg[["specimen"]] <- factor(sg[["specimen"]])
  groups <- levels(sg[["specimen"]])
 template_sheet <- get_global_sheets(obj)[1]

  if(is.null(name)){
    message("Choose which group of samples to import:\n");
    groupInd <- utils::menu(groups,graphics=FALSE);
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
  # NOTE: This seems like a bug. Where is the specimen variable defined?
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
  #NOTE This seems like a bug: where is sampleSelected defined?
  if(any(isDup))
    stop("Duplicated sample names detected within group: ", paste(sampleSelected[isDup], collapse = " "), "\n Please check if the appropriate group is selected.")


    if(worksheet == "global")
    {
      message("parse the global template ...")
      samples <- sn[1]
    }
    else
    {
      message("Parsing ", nSample," samples");
      samples <- sn
    }
      
    
    thisCall <- quote(.parseDivaWorkspace(xmlFileName=file.path(obj@path,obj@file)
                                          ,samples = samples
                                          ,worksheet = worksheet
                                          , swap_cols = swap_cols
                                          , template_sheet = template_sheet
                                          , groupName = group.name
                                          ,path=path
                                          ,xmlParserOption = obj@options
                                          ,ws = obj
                                          , verbose = verbose
                                          ,...)
                      )
    if(worksheet == "global")
    {
      if(verbose)
        gs <- eval(thisCall)
      else
        suppressMessages(gs <- eval(thisCall))
      #cp gates to the all files
      gs <- gh_apply_to_new_fcs(gs[[1]], sn, path = path , swap_cols = swap_cols, compensation_source = "template", ...)
    }else
      gs <- eval(thisCall)
    
    gs
    
 
}
#' @importFrom XML xpathSApply
#' @importFrom flowCore read.FCS transformList spillover logicleTransform
#' @importFrom flowWorkspace gh_pop_set_xml_count save_gs load_gs gs_split_by_tree gh_pop_is_hidden gh_pop_is_negated swap_data_cols cs_swap_colnames get_cytoframe_from_cs load_cytoframe cf_write_disk merge_list_to_gs cs_set_cytoframe
#' @importFrom ggcyto rescale_gate
#' @param scale_level indicates whether the gate is scaled by tube-level or gate-level biexp_scale_value (for debug purpose, May not be needed.)
#' @noRd
.parseDivaWorkspace <- function(xmlFileName,samples
                                ,worksheet = worksheet
                                , template_sheet = "Global Sheet1"
                                , swap_cols
                                ,path,xmlParserOption, ws, groupName, scale_level = "tube", verbose = FALSE, num_threads = 1
                                , xml_compensation_enabled = NULL
                                ,  ...){

  scale_level <- match.arg(scale_level, c("gate", "tube"))
  if(!file.exists(xmlFileName))
    stop(xmlFileName," not found!")

  rootDoc <- ws@doc
  
  
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
  
  
  xpathGroup <- paste0("/bdfacs/experiment/specimen[@name='", groupName, "']")
  #determine the magic number to replace neg value for log10 trans
  log_decade <- xmlValue(rootDoc[["/bdfacs/experiment/log_decades"]])
  if(log_decade == "4")
    min_val <- 26
  else if(log_decade == "5")
    min_val <- 2.6
  else
    stop("unsupported decade: ", log_decade)

  num_threads <- min(num_threads, length(files))
  if(num_threads >1)
  {
    requireNamespace("parallel")
    file.group <- split(files, cut(seq_along(files),num_threads))
  }
  else
    file.group <- list(files)
  names(file.group) <- as.character(seq_along(file.group))
  tmp.dir <- tempfile()
  dir.create(tmp.dir)
  thisCall <- quote(lapply(names(file.group), function(grpid){

      files <- file.group[[grpid]]
      # parse compp & trans
      gslist <- data.ranges <- list()
      for(file in files)
      {


        sampleName <- basename(file)
        
          
        #get tube node
        xpathSample <- paste0(xpathGroup, "/tube[data_filename='", sampleName, "']")
        sampleNode.tube <- xpathApply(rootDoc, xpathSample)[[1]]
        if(worksheet == "normal")
        {
          gate_source <- sampleNode.tube
          execute <- TRUE
        }else
        {
          gate_source <- xpathApply(rootDoc, paste0("/bdfacs/experiment/acquisition_worksheets/worksheet_template[@name='"
                                                    , template_sheet, "']")
                                    )[[1]]
          execute <- FALSE
        }
        # get comp & param for biexp
        biexp_para <- new.env(parent = emptyenv())
        #global worksheet doesn't seem to have the valid default scale parameters sometime
        #so we parse it from the sample/tube-specific settings
        use_auto_biexp_scale <- as.logical(xmlValue(sampleNode.tube[["instrument_settings"]][["use_auto_biexp_scale"]]))
        if(is.null(xml_compensation_enabled))
          xml_compensation_enabled <- as.logical(xmlValue(sampleNode.tube[["instrument_settings"]][["compensation_enabled"]]))
        biexp_scale_node <- ifelse(use_auto_biexp_scale, "comp_biexp_scale", "manual_biexp_scale")
        comp <- xpathApply(sampleNode.tube, "instrument_settings/parameter", function(paramNode, biexp_para){

          paramName <- xmlGetAttr(paramNode, "name")

          # A param can have is_log == true without having a compensation node
          # so we need to check these separately
          isLog <- as.logical(xmlValue(xmlElementsByTagName(paramNode, "is_log")[[1]]))
          isComp <- length(xmlElementsByTagName(paramNode, "compensation")) > 0
          if(isLog){

            #get biexp para

            biexp_para[[paramName]] <- c(min = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "min")[[1]]))
                                          , max = as.numeric(xmlValue(xmlElementsByTagName(paramNode, "max")[[1]]))
                                          , biexp_scale = as.numeric(xmlValue(xmlElementsByTagName(paramNode, biexp_scale_node)[[1]]))
                                          )
          }
          if(isComp){
            #get comp
            coef <- as.numeric(xpathSApply(paramNode, "compensation/compensation_coefficient", xmlValue))
            res <- list(coef)
            names(res) <- paramName
          }else
            res <- NULL
          return(res)
        }, biexp_para = biexp_para)

        ##################################
        #parse comp
        ##################################


        message("loading data: ",file);
        #load single sample into cs so that gs can be constructed from it
        cs <- load_cytoset_from_fcs(file, backend = "mem", ...)#has to load data regardless of execute flag because data range is needed for gate extension
        cols <- swap_data_cols(colnames(cs), swap_cols)
        if(!all(cols==colnames(cs)))
          for(c1 in names(swap_cols))
          {
            c2 <- swap_cols[[c1]]
            cs_swap_colnames(cs, c1, c2)					
          }
        
        gs <- GatingSet(cs)
        cf <- get_cytoframe_from_cs(cs, sampleName)

        message("Compensating")
        if(xml_compensation_enabled){
          # Use the spillover matrix obtained from the xml for this tube
          comp <- unlist(comp, recur = F)
          comp <- data.frame(comp, check.names = F)
          # NOTE: The matrix of compensation coefficients in the xml
          # is already inverted (it's a compensation matrix, not a spillover
          # matrix), so we need to invert it before passing it to compensate
          comp <- solve(comp)
          colnames(comp) <- rownames(comp)
          comp <- compensation(comp)

         compensate(gs, comp)
        }else{
          #we use the spillover from FCS keyword
          comp <- spillover(cf)
          comp <- compact(comp)
          if(length(comp) > 1)
            stop("More than one spillover found in FCS!")
          else if(length(comp) == 0)
            stop("No spillover found in FCS!")

          compensate(gs, comp[[1]])
        }

        message("computing data range")
        data.ranges[[sampleName]] <- range(cf, "data")


        message(paste("transforming ..."))

        #parse trans
        params <- names(biexp_para)
        # browser()
        #transform data in default flowCore logicle or log10 scale
        trans <- sapply(params, function(pn){
          this_para <- biexp_para[[pn]]
          maxValue <- 262144#TODO:10^this_para[["max"]]
          pos <- 4.5
          r <- abs(this_para[["biexp_scale"]])
          trans <- generate_trans(maxValue, pos, r)
                    }
          , simplify = FALSE)
        this_biexp <- transformerList(params, trans)
        
        gs <- transform(gs, this_biexp)
        

        message("parsing gates ...")

        gh <- gs[[sampleName]]

        if(worksheet == "normal")
          sampleNode <- sampleNode.tube#xpathApply(rootDoc, xpathSample)[[1]]
        else
          sampleNode <- gate_source
        #assume the gates listed in xml follows the topological order
        rootNode.xml <- NULL
        gateNodes <- xpathApply(sampleNode, "gates/gate")
        for(gateNode in gateNodes)
        {
          nodeName <- xmlGetAttr(gateNode, "fullname")
          gtype <- xmlGetAttr(gateNode, "type")
          nodeName <- normalize_gate_path(nodeName)
          nodeName <- basename(nodeName)
          if(verbose)
            message(nodeName)
          if(worksheet == "normal")
            count <- as.integer(xmlValue(xmlElementsByTagName(gateNode, "num_events")[[1]]))
          else
            count <- 0#skip xml cnt for template gating
          parent <- xmlElementsByTagName(gateNode, "parent")
          if(length(parent) > 0){
            parent <- xmlValue(parent[[1]])
            parent <- normalize_gate_path(parent)
            parent <- gsub(rootNode.xml, "root", parent)
            
            if(gtype == "Region_Classifier")
            {
              
              regionNode <- xmlElementsByTagName(gateNode, "region")[[1]]
              
              xParam <- xmlGetAttr(regionNode, "xparm")
              yParam <- xmlGetAttr(regionNode, "yparm")
              region_type <- xmlGetAttr(regionNode, "type")
  
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
                  # rescale gate to data scale by:
                  # 1) inverting the gate logicle transform (based on the biexp_scale value from the gate node)
                  #    --This appears to always be a logicle transform: biexp_scale == 0 --> w == 0 in logicle
                  # 2) applying the tranformation applied to the data (based on the biexp_scale value from the tube node) 
                  #    --Here if biexp_scale == 0, the data gets scaled log10
                  trans.gate <- generate_trans(r = x_parameter_scale_value, force_logicle=TRUE)
                  mat[1, ] <- trans.gate$inverse(mat[1, ])
                  mat[1, ] <- x_biexp(mat[1, ])
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
                  #rescale gate to data scale -- see note in is.x.scaled block above
                  trans.gate <- generate_trans(r = y_parameter_scale_value, force_logicle=TRUE)
                  mat[2, ] <- trans.gate$inverse(mat[2, ])
                  mat[2, ] <- y_biexp(mat[2, ])
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
  
  
              if(region_type == "RECTANGLE_REGION"){
                x <- unique(mat[1,])
                y <- unique(mat[2,])
                if(length(x)!=2||length(y)!=2)
                  stop("invalid RECTANGLE_REGION from ", nodeName)
                coord <- list(x,y)
                names(coord) <- c(xParam, yParam)
                gate <- rectangleGate(.gate = coord)
              }else if(region_type == "POLYGON_REGION"){
                rownames(mat) <- c(xParam, yParam)
                gate <- polygonGate(.gate = t(mat))
              }else if(region_type == "INTERVAL_REGION"){
                # browser()
                coord <- list(mat[1,])
                names(coord) <- xParam
                gate <- rectangleGate(coord)
              }else
                stop("unsupported gate region type: ", region_type)
  
              #deal with the off threshold data truncation in log10 scale scenario
              #we do gate extention instead to keep transformation consistent across gates(i.e always use biexp)
              if(x.extend||y.extend)
              {
                if(x.extend)
                  bound[xParam, ] <- c(min_val, 262143)
                if(y.extend)
                  bound[yParam, ] <- c(min_val, 262143)
                gate <- extend(gate, bound, t(data.ranges[[sampleName]]))
                #need transform since extention was performed on the raw-scale gate
                if(x.extend)
                  gate <- rescale_gate(gate, x_biexp@.Data, xParam)
                if(y.extend)
                  gate <- rescale_gate(gate, y_biexp@.Data, yParam)
              }
            }else if(gtype %in% c("AND_Classifier", "OR_Classifier", "NOT_Classifier"))
            {
              op <- sub("_Classifier$", "", gtype)
              gate <- diva_get_bool_gate(gateNode, op)
                
            }else
              stop("Unsupported gate type: ", gtype)



          pop_add(gate, gh, parent = parent, name = nodeName)
            if(parent == "root")
              parent <- ""
            unique.path <- file.path(parent, nodeName)
          #save the xml counts
            gh_pop_set_xml_count(gh, unique.path, count)
          }else{
            rootNode.xml <- nodeName
            if(rootNode.xml!="All Events")
              stop("unrecognized root node: ", rootNode.xml)
            gh_pop_set_xml_count(gh, "root", count)
            next
          }
        }#end of gate adding
        if(execute)
        {
          #convert to on disk cs to prevent mem issue
          tmp <- tempfile()
		  backend <- list(...)[["backend"]]
		  if(is.null(backend))
			  backend <- get_default_backend()
		  cf_write_disk(cf, tmp, backend = backend)
          cs_set_cytoframe(cs, sampleName, load_cytoframe(tmp))
        }
        gslist[[sampleName]] <- gs
      }
      #merge samples into a single gs
      gs <- merge_list_to_gs(gslist)

      if(execute)
        suppressMessages(recompute(gs))
      if(num_threads>1)
        suppressMessages(save_gs(gs, cdf = "move", path = file.path(tmp.dir, grpid)))
      message("done!")
      gs
  })
  )

  if(num_threads >1)
  {
    require(parallel)
    thisCall[[1]] <- quote(mclapply)

    thisCall[["mc.cores"]] <- num_threads
    eval(thisCall)
    
    gsfiles <- list.files(tmp.dir, full.names = TRUE)
    
  
    res <- try(suppressMessages(lapply(gsfiles, load_gs)))
    if(is(res, "try-error"))
      stop("the parsed GatingSets are cached in: ", tmp.dir, "\n Please use load_gs to load and clean/merge them!")
    merge_list_to_gs(res)
  }else
  {

    gs <-  eval(thisCall)[[1]]

       # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed
    gslist <- suppressMessages(gs_split_by_tree(gs))
    if(length(gslist) > 1)
      warning("GatingSet contains different gating tree structures and must be cleaned before using it! ")
    gs
  }

}

diva_get_bool_gate <- function(gateNode, op){
  if(op == "NOT")
  {
    op <- "!"
  }else if(op == "AND")
    op <- "&"
  else 
    op <- "|"
  
  refs <- xmlElementsByTagName(gateNode, "input")
  refs <- sapply(refs, function(ref){
                        ref <- xmlValue(ref)
                        ref <- sub("^All Events", "", ref)
                        ref <- gsub("\\\\", "/", ref)
                    })
  
  nref <- length(refs)
  if(op == "!")
  {
    if(nref!=1)
      stop("invalid number of reference nodes for NOT gate")
    exprs <- paste0("!", refs)
  }else
  {
    if(nref < 2)
      stop("invalid number of reference nodes for AND, OR gate")
    exprs <- paste(refs, collapse = op)
  }
  call <- substitute(booleanFilter(v), list(v = as.symbol(exprs)))
  eval(call)
}
normalize_gate_path <- function(path){
  path <- gsub("/", "|", path)#escape /
  gsub("\\\\", "/", path)
}

#use the equation suggested by BD engineer last year
#' @importFrom flowWorkspace logicle_trans
generate_trans <- function(maxValue = 262144, pos = 4.5, r, force_logicle=FALSE)
{
  if(r == 0){
    if(!force_logicle)
      return (flowjo_log_trans())# r <- maxValue/10^pos
    else
      w <- 0
  }else{
    w <- (pos - log10(maxValue/r))/2
    if(w < 0)
      w <- 0
  }

  logicle_trans(w=w, t = maxValue, m = pos) #
}