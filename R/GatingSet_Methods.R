#' constructors for GatingSet 
#' 
#' construct object from xml workspace file and a list of sampleIDs (not intended to be called by user.)
#' 
#' @param x \code{character} or \code{flowSet} or \code{GatingHierarchy}
#' @param y \code{character} or\code{missing}
#' @param guids \code{character} vectors to uniquely identify each sample (Sometime FCS file names alone may not be unique)
#' @param includeGates \code{logical} whether to parse the gates or just simply extract the flowJo stats
#' @param sampNloc \code{character} scalar indicating where to get sampleName(or FCS filename) within xml workspace. It is either from "keyword" or "sampleNode".
#' @param xmlParserOption \code{integer} option passed to \code{\link{xmlTreeParse}} 
#' @param wsType \code{character} workspace type, can be value of "win", "macII", "vX", "macIII".
#'  
#' @rdname GatingSet-methods
#' @aliases GatingSet
#' @export 
setMethod("GatingSet",c("character","character"),function(x,y, guids, includeGates=FALSE, sampNloc="keyword",xmlParserOption, wsType){
			
			xmlFileName<-x
			sampleIDs<-y
#			browser()
			sampNloc<-match(sampNloc,c("keyword","sampleNode"))
			if(is.na(sampNloc))
				sampNloc<-0
			stopifnot(!missing(xmlFileName))
			
			wsType <- match(wsType, c("win", "macII", "vX", "macIII"))
			if(is.na(wsType))
				stop("unrecognized workspace type: ", wsType)
			
			if(!file.exists(xmlFileName))
				stop(xmlFileName," not found!")
			Object<-new("GatingSet")
			Object@pointer<-.cpp_parseWorkspace(xmlFileName,sampleIDs,guids,includeGates,as.integer(sampNloc),as.integer(xmlParserOption),as.integer(wsType))
			identifier(Object) <- .uuid_gen()
			Object@flag <- FALSE
			
			return(Object)
		})

