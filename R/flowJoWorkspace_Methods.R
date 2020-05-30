#' @useDynLib CytoML,.registration = TRUE
NULL

#' An R representation of a flowJo workspace.
#' 
#' Objects can be created by calls of the form \code{new("flowjo_workspace.xml", ...)}.
#'
#' @section Slots: 
#' \describe{
#'   \item{\code{doc}:}{Object of class \code{"externalptr"}. }
#'   }
#' 
#' @seealso 
#'   \code{\linkS4class{GatingSet}} 
#'   \code{\linkS4class{GatingHierarchy}}
#' 
#' @examples
#'   require(flowWorkspaceData)
#'   d<-system.file("extdata",package="flowWorkspaceData")
#'   wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
#'   ws <- open_flowjo_xml(wsfile);
#'   ws
#'   fj_ws_get_samples(ws)
#' 
#' @name flowjo_workspace-class
#' @rdname flowjo_workspace-class
#' @exportClass flowjo_workspace
#' @aliases 
#' show,flowjo_workspace-method
setClass("flowjo_workspace",representation(doc="externalptr"))


#' Open/Close a flowJo workspace
#' 
#' Open a flowJo workspace and return a \code{flowjo_workspace} object.
#' Close a flowjo_workspace, destroying the internal representation of the XML document, and freeing the associated memory.
#' 
#' @param file Full path to the XML flowJo workspace file.
#' @param options xml parsing options passed to \code{\link{xmlTreeParse}}. See http://xmlsoft.org/html/libxml-parser.html#xmlParserOption for details.
#' @param sample_names_from character specifying where in the XML workspace file to obtain the sample names, either
#' "keyword" for the included $FIL keyword for each sample, or "sampleNode" for the name of the sample node
#' @param ... not used
#' @details
#' 	Open an XML flowJo workspace file and return a \code{flowjo_workspace} object. The workspace is represented using a \code{XMLInternalDocument} object.
#' 	Close a flowJoWorkpsace after finishing with it. This is necessary to explicitly clean up the C-based representation of the XML tree. (See the XML package).
#' @return  a \code{flowjo_workspace} object.
#' @examples
#' \dontrun{
#' 	file<-"myworkspace.xml"
#' 	ws<-open_flowjo_xml(file);
#' 	ws
#' }
#'
#' @importFrom XML xmlTreeParse xmlAttrs xmlGetAttr xmlTreeParse xmlRoot xmlValue xpathApply
#' @export 
open_flowjo_xml <- function(file,options = 0, sample_names_from = "keyword", ...){
  if("sampNloc" %in% names(list(...))){
    warning("'sampNloc' argument deprecated and renamed 'sample_names_from'")
    sample_names_from <- list(...)[["sampNloc"]]
  }
  valid_values <- c("keyword", "sampleNode")
  sample_names_from <- match.arg(sample_names_from, valid_values)
  file <- path.expand(file)
  new("flowjo_workspace", doc = open_workspace(file, sample_name_location = match(sample_names_from,valid_values), xmlParserOption = options))
  
}

setMethod("show",c("flowjo_workspace"),function(object){
#	cat("FlowJo Workspace Version ",get_version(object),"\n");
      cat("File location: ",get_xml_file_path(object@doc),"\n");
      cat("\nGroups in Workspace\n");
      
      sg <- fj_ws_get_sample_groups(object)
      tbl<-table(Name=sg$groupName,GroupID=sg$groupID)
      print(data.frame(Name=rownames(tbl),"Num.Samples"=diag(tbl)))
      
    })
#' @export
#' @param obj flowjo_workspace
#' @rdname flowjo_to_gatingset
setGeneric("parseWorkspace",function(obj,...)standardGeneric("parseWorkspace"))

#' @templateVar old parseWorkspace
#' @templateVar new flowjo_to_gatingset
#' @template template-depr_pkg
NULL

#' @importFrom flowWorkspace openWorkspace
#' @rdname flowjo_to_gatingset
setMethod("parseWorkspace",signature("flowjo_workspace"),function(obj, ...){
			.Deprecated("flowjo_to_gatingset")
			flowjo_to_gatingset(obj, ...)
			
		})

#' Parse a flowJo Workspace
#' 
#' Function to parse a flowJo Workspace, generate a \code{GatingHierarchy} or \code{GatingSet} object, and associated flowCore gates. The data are not loaded or acted upon until an explicit call to \code{recompute()} is made on the \code{GatingHierarchy} objects in the \code{GatingSet}.
#' @param ws A \code{flowjo_workspace} to be parsed.
#' @param name \code{numeric} or \code{character}. The name or index of the group of samples to be imported. If \code{NULL}, the groups are printed to the screen and one can be selected interactively. Usually, multiple groups are defined in the flowJo workspace file.
#' @param  execute \code{TRUE|FALSE} a logical specifying if the gates, transformations, and compensation should be immediately calculated after the flowJo workspace have been imported. TRUE by default. 
#' @param  subset \code{numeric} vector specifying the subset of samples in a group to import.
#'                        Or a \code{character} specifying the FCS filenames to be imported.
#'                        Or an \code{expression} to be passed to 'subset' function to filter samples by 'pData' (Note that the columns referred by the expression must also be explicitly specified in 'keywords' argument)  
#' @param  includeGates \code{logical} Should gates be imported, or just the data with compensation and transformation?
#' @param  path either a \code{character} scalar or \code{data.frame}. When \code{character}, it is a path to the fcs files that are to be imported. The code will search recursively, so you can point it to a location above the files. 
#'                                                          When it is a \code{data.frame}, it is expected to contain two columns:'sampleID' and 'file', which is used as the mapping between 'sampleID' and FCS file (absolute) path. When such mapping is provided, the file system searching is avoided.
#' @param cytoset a \code{cytoset} object that provides the alternative data source other than FCS files. It is useful sometime to preprocess the raw fcs files
#'                                   (e.g. standardize channels using \code{cytoqc} package) and then directly use them for flowJo parsing.
#'                                   when cytoset is provided, \code{path} argument is ignored.
#' @param h5_dir the path to write h5 data
#' @param  compensation a \code{compensation} object, matrix or data.frame or a list of these objects that allow the customized compensation () to be used instead of the one specified in flowJo workspace or FCS file.    
#'                                 When it is a list, its names is supposed to be matched to sample guids (Default is the fcs filename suffixed by $TOT. See "additional.keys" arguments for details of guids)
#'                                 When some of the samples don't have the external compensations matched, it will fall back to the flowJo xml or FCS looking for the compensation matrix.
#' @param  channel.ignore.case a \code{logical} flag indicates whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
#' @param  extend_val \code{numeric} the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
#' @param  extend_to \code{numeric} the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
#'                                  But when the gates needs to be extended without loading the raw data (i.e. \code{execute} is set to FALSE), then this hard-coded value is used.
#' @param  leaf.bool a \code{logical} whether to compute the leaf boolean gates. Default is TRUE. It helps to speed up parsing by turning it off when the statistics of these leaf boolean gates are not important for analysis. (e.g. COMPASS package will calculate them by itself.)
#'                                           If needed, they can be calculated by calling \code{recompute} method at later stage.
#' @param  additional.keys \code{character} vector:  The keywords (parsed from FCS header) to be combined(concatenated with "_") with FCS filename
#'                                                          to uniquely identify samples. Default is '$TOT' (total number of cells) and more keywords can be added to make this GUID.
#' @param  additional.sampleID \code{boolean}: A boolean specifying whether to include the flowJo sample ID in a GUID to uniquely identify samples. This can be helpful when the
#'                                                    filename or other keywords are not enough to differentiate between samples. Default is FALSE.                                                
#' @param  keywords \code{character} vector specifying the keywords to be extracted as pData of GatingSet
#' @param  keywords.source \code{character} the place where the keywords are extracted from, can be either "XML" or "FCS"
#' @param  keyword.ignore.case a \code{logical} flag indicates whether the keywords matching needs to be case sensitive.    
#' @param  include_empty_tree a \code{logical} whether to include samples that don't have gates.
#' @param  skip_faulty_gate a \code{logical} whether to skip the faulty gates so that the parser can still process the rest of gating tree.
#' 
#' @param  greedy_match \code{logical}: By default, if flowjo_to_gatingset finds multiple FCS files matching a sample by total event count as well as sampleID and/or keywords specified by additional.keys and additional.sampleID, it will return an error listing the duplicate files.
#'                                            If greedy_match is TRUE, the method will simply take the first file with either filename or $FIL keyword matching the sample name and having the correct number of events.
#' @param  mc.cores \code{numeric} the number of threads to pass to the C++ parser to run in parallel
#' @param  transform \code{logical} to enable/disable transformation of gates and data. Default is TRUE. It is mainly for debug purpose (when the raw gates need to be parsed.
#' @param  fcs_file_extension default is ".fcs"
#' @param  ... Additional arguments to be passed to FCS parser
#' @details
#' A flowjo_workspace is generated with a call to \code{open_flowjo_xml()}, passing the name of the xml workspace file. This returns a \code{flowjo_workspace}, which can be parsed using the \code{flowjo_to_gatingset()} method. The function can be called non-interactively by passing the index or name of the group of samples to be imported via \code{flowjo_to_gatingset(obj,name=x)}, where \code{x} is either the numeric index, or the name. 
#' The \code{subset} argument allows one to select a set of files from the chosen sample group. The routine will take the intersection of the files in the sample group, the files specified in \code{subset} and the files available on disk, and import them. 
#' @return 
#'  a \code{GatingSet}, which is a wrapper around a list of \code{GatingHierarchy} objects, each representing a single sample in the workspace. The \code{GatingHierarchy} objects contain \code{graphNEL} trees that  represent the gating hierarchy of each sample. Each node in the \code{GatingHierarchy} has associated data, including the population counts from flowJo, the parent population counts, the \code{flowCore} gates generated from the flowJo workspace gate definitions. Data are not yet loaded or acted upon at this stage. To execute the gating of each data file, a call to \code{execute()} must be made on each \code{GatingHierarchy} object in the \code{GatingSet}. This is done automatically by default, and there is no more reason to set this argument to FALSE. 
#' @seealso \code{\link{fj_ws_get_sample_groups}},\code{\link{GatingSet}}
#' @examples
#' \dontrun{
#' 	 #f is a xml file name of a flowJo workspace
#' 	ws <- open_flowjo_xml(f)
#'  #parse the second group
#' 	gs <- flowjo_to_gatingset(ws, name = 2); #assume that the fcs files are under the same folder as workspace
#' 
#'  
#'  gs <- flowjo_to_gatingset(ws, name = 4
#'                         , path = dataDir     #specify the FCS path 
#'                         , subset = "CytoTrol_CytoTrol_1.fcs")     #subset the parsing by FCS filename
#' 
#'  
#' 
#'  gs <- flowjo_to_gatingset(ws, path = dataDir, name = 4
#'                          , keywords = c("PATIENT ID", "SAMPLE ID", "$TOT", "EXPERIMENT NAME") #tell the parser to extract keywords as pData
#'                          , keywords.source = "XML" # keywords are extracted from xml workspace (alternatively can be set to "FCS")
#'                          , additional.keys = c("PATIENT ID") #use additional keywords together with FCS filename to uniquely identify samples
#'                          , execute = F) # parse workspace without the actual gating (can save time if just want to get the info from xml)
#' 
#' #subset by pData (extracted from keywords)
#' gs <- flowjo_to_gatingset(ws, path = dataDir, name = 4
#'                          , subset = `TUBE NAME` %in% c("CytoTrol_1", "CytoTrol_2")
#'                          , keywords = "TUBE NAME")
#' 
#' 
#' #overide the default compensation defined in xml with the customized compenstations
#' gs <- flowjo_to_gatingset(ws, name = 2, compensation = comps); #comp is either a compensation object or a list of compensation objects
#' }
#' @aliases flowjo_to_gatingset
#' @rdname flowjo_to_gatingset
#' @export 
#' @importFrom utils menu
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom dplyr enquo
flowjo_to_gatingset <- function(ws, name = NULL
    , subset = list()
    , execute = TRUE
    , path = ""
    , cytoset = NULL
    , h5_dir = tempdir()
    , includeGates = TRUE
    , additional.keys = "$TOT"
    , additional.sampleID = FALSE
    , keywords = character()
    , keywords.source = "XML"
    , keyword.ignore.case = FALSE
    , extend_val = 0
    , extend_to = -4000
    , channel.ignore.case = FALSE
    , leaf.bool = TRUE
    , include_empty_tree = FALSE
    , skip_faulty_gate = FALSE
    , compensation = NULL
    , transform = TRUE
	, fcs_file_extension = ".fcs"
	, greedy_match = FALSE
	, mc.cores = 1
    , ...)
{
  if(is.null(cytoset))
    cytoset <- cytoset()
  # determine the group
  g <- fj_ws_get_sample_groups(ws)
  groups <- g[!duplicated(g$groupName),]
  groups <- groups[order(groups$groupID),"groupName"]
  
  if(is.null(name)){
    groupInd <- menu(groups,graphics=FALSE, "Choose which group of samples to import:");
  }else if(is.numeric(name)){
    if(length(groups)<name)
      stop("Invalid sample group index.")
    groupInd <- name
  }else if(is.character(name)){
    if(is.na(match(name,groups)))
      stop("Invalid sample group name.")
    groupInd <- match(name,groups)
  }
  
  #parse the filter
  subset_parsed <- try(eval(substitute(subset)), silent = TRUE)
  if(class(subset_parsed) == "try-error"){
    # Try evaluating the subset arg as an expression for filtering
    # based on keywords
    samples_in_group <- fj_ws_get_samples(ws, groupInd)
    subset_parsed <- try({
      keys <- lapply(samples_in_group$sampleID, function(sid){
        unlist(fj_ws_get_keywords(ws, sid)[keywords])
      })
      keys <- data.frame(do.call(rbind, keys), check.names = FALSE)
      keys <- cbind(samples_in_group[,c("sampleID", "name")], keys)
      # Pull the sample names that pass the filter
      filter(keys, !!enquo(subset))$name
      })
  }
  if(class(subset_parsed) == "try-error")
    stop("invalid 'subset' argument!")
  
  subset <- subset_parsed

  if(is(subset, "numeric"))#convert numeric index to sample names
  {
    subset <- as.character(fj_ws_get_samples(ws, groupInd)[subset, "name"])
 }
  
  if(is(subset, "character"))
  {
    subset <- list(name = subset)
  }
  
  if(!is(subset, "list"))
  {
    stop("invalid 'subset' argument!")
  }
  if(is.null(additional.keys))
    additional.keys <- character(0)
  if(is.null(path))
    path <- ""
  args <- list(...)
  if(!is.null(args[["isNcdf"]]))
  {
    warning("'isNcdf' argument is deprecated!Data is always stored in h5 format by default!")
    args[["isNcdf"]] <- NULL
  }
  if(is.null(compensation))
  {
    compensation <- list()
  }else
  {
    if(is.list(compensation)&&!is.data.frame(compensation))
    {
      compensation <- sapply(compensation, check_comp, simplify = FALSE)
    }else
      compensation <- check_comp(compensation)
      
  }
  args <- list(ws = ws@doc
                 , group_id = groupInd - 1
                 , subset = subset
                 , execute = execute
                 , path = suppressWarnings(normalizePath(path))
                  , cytoset = cytoset@pointer
                 , h5_dir = suppressWarnings(normalizePath(h5_dir))
                 , includeGates = includeGates
                 , additional_keys = additional.keys
                 , additional_sampleID = additional.sampleID
                 , keywords = keywords
                 , is_pheno_data_from_FCS = keywords.source == "FCS"
                 , keyword_ignore_case = keyword.ignore.case
                 , extend_val = extend_val
                 , extend_to = extend_to
                 , channel_ignore_case = channel.ignore.case
                 , leaf_bool = leaf.bool
                , include_empty_tree = include_empty_tree
               , skip_faulty_gate = skip_faulty_gate
                 , comps = compensation
                , transform = transform
		 		 , fcs_file_extension = fcs_file_extension
		 		 , greedy_match = greedy_match
				 , fcs_parse_arg = args
		 		 , num_threads = mc.cores
              )
  p <- do.call(parse_workspace, args)
  gs <- new("GatingSet", pointer = p)
  #    # try to post process the GatingSet to split the GatingSets(based on different the gating trees) if needed                
  gslist <- suppressMessages(gs_split_by_tree(gs))
  if(length(gslist) > 1)
	  warning("GatingSet contains different gating tree structures and must be cleaned before using it! ")
  gs
}


check_comp <- function(compensation){
	if(is(compensation, "compensation")){
		compensation <- compensation@spillover
	}else if(is.data.frame(compensation)){
		compensation <- as.matrix(compensation)
	}
	if(!is.matrix(compensation))
		stop("'compensation' should be a compensation object, matrix or data.frame!")
	compensation
}


#' Get Keywords
#'
#' Retrieve keywords associated with a workspace
#' 
#' @name fj_ws_get_keywords
#' @aliases getKeywords
#' @param obj A \code{flowjo_workspace}
#' @param y c\code{character} or \code{numeric} specifying the sample name or sample ID
#' @param ... other arguments
#'      sampNloc a \code{character} the location where the sample name is specified. See \code{parseWorkspace} for more details.
#'
#' @details
#'   Retrieve a list of keywords from a \code{flowjo_workspace}
#' @return A list of keyword - value pairs.
#' @examples
#'
#' \dontrun{
#'   d<-system.file("extdata",package="flowWorkspaceData")
#'   wsfile<-list.files(d,pattern="manual.xml",full=TRUE)
#'   ws <- open_flowjo_xml(wsfile)
#'
#'   fj_ws_get_samples(ws)
#'   res <- try(fj_ws_get_keywords(ws,"CytoTrol_CytoTrol_1.fcs"), silent = TRUE)
#'   print(res[[1]])
#'   fj_ws_get_keywords(ws, 1)
#' }
#' @export
fj_ws_get_keywords <- function(obj,y, ...){
	if(length(y) > 1)
        stop("getKeywords can only work with one sample at a time!")
	if(is.character(y))
		func <- get_keywords_by_name
	else
	{
		y <- as.integer(y)
		func <- get_keywords_by_id
	}
      as.list(func(obj@doc, y))
    }

#' @templateVar old getKeywords
#' @templateVar new fj_ws_get_keywords
#' @template template-depr_pkg
NULL

#' @export 
getKeywords <- function(...){
  .Deprecated("fj_ws_get_keywords")
  fj_ws_get_keywords(...)
  }


#' Get a list of samples from a flowJo workspace
#'
#' Return  a data frame of samples contained in a flowJo workspace
#' @name fj_ws_get_samples
#' @aliases getSamples
#' @param x A \code{flowjo_workspace}
#' @param group_id \code{integer} specifies the group from which samples are returned
#' @details
#' The samples with 0 populations are excluded.
#' Returns a \code{data.frame} of samples in the \code{flowjo_workspace}, including their
#' \code{sampleID}, \code{name}
#'
#' @return
#' A \code{data.frame} with columns \code{sampleID}, \code{name}
#'
#' @examples
#'       \dontrun{
#'         #ws is a flowjo_workspace
#'         fj_ws_get_samples(ws);
#'       }
#' @export
fj_ws_get_samples <- function(x, group_id = NULL)
{
  res <- get_samples(x@doc)
  if(!is.null(group_id))
    res <- res[group_id]
  res <- unlist(res, recursive = FALSE)
  res <- lapply(res, as.data.frame, stringsAsFactors = FALSE)
  res <- do.call(rbind, res)
  unique(res)
}

#' @templateVar old getSamples
#' @templateVar new fj_ws_get_samples
#' @template template-depr_pkg
NULL

#' @export  
getSamples <- function(...){
  .Deprecated("fj_ws_get_samples")
  fj_ws_get_samples(...)
  }
#' Get a table of sample groups from a flowJo workspace
#'
#' Return a data frame of sample group information from a flowJo workspace
#' @name fj_ws_get_sample_groups
#' @aliases getSampleGroups
#' @param x A \code{flowjo_workspace} object.
#' @details 
#' Note that the samples with 0 populations are also included (since count populations requires traversing xml for all samples thus can be expensive)
#' Returns a table of samples and groups defined in the flowJo workspace
#' @return
#'   A \code{data.frame} containing the \code{groupName}, \code{groupID}, and \code{sampleID} for each sample in the workspace. Each sample may be associated with multiple groups.
#' @seealso \code{\link{flowjo_workspace-class}} \code{\link{flowjo_to_gatingset}}
#'
#' @examples
#'   \dontrun{
#'     #ws is a flowjo_workspace
#'     fj_ws_get_sample_groups(ws);
#'   }
#' @export
fj_ws_get_sample_groups <- function(x){
  res <- get_sample_groups(x@doc)
  df <- mapply(res[["groupName"]], res[["groupID"]], res[["sampleID"]]
               , FUN = function(x, y, z){
                  data.frame(x,y,z)                            
               }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  df <- do.call(rbind, df)
  colnames(df) <-  c("groupName", "groupID", "sampleID")
  df$groupName <- as.character(df$groupName)
  df
}

#' @templateVar old getSampleGroups
#' @templateVar new fj_ws_get_sample_groups
#' @template template-depr_pkg
NULL

#' @export 
getSampleGroups <- function(...){
  .Deprecated("fj_ws_get_sample_groups")
  fj_ws_get_sample_groups(...)
}




