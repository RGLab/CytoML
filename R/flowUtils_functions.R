# functions copied from flowUtils since they are removed from the latest flowUtils
####################################

## Return default attributes for an XML node of type 'tag' potentially 
## replacing values by the content of 'attrs' (in form of a named list)
## or by additional named '...' arguments.
targs <- function(tag, attrs=NULL, system="win", ...)
{
  defs <- fjSettings(system)
  tnam <- gsub(".*:", "", tag)
  if(!tnam %in% names(defs))
    stop("'", tnam, "' is not a valid XML tag in this context.")
  res <- defs[[tnam]]
  args <- c(list(...), attrs)
  if(length(args) && length(names(args)))
  {
    args <- args[names(args)!=""]
    res[names(args)] <- args
  }
  if(!is.null(res))
  {
    res <- res[!sapply(res, is.null)]
    n <- names(res)
    res <- as.character(res)
    names(res) <- n
  }
  return(res)
} 

## The default attributes for all types of XML nodes needed for a FlowJo 
## workspace. NULL values are ignored. These are stored in inst/defaults.xml
## and new tags have to be added there.
fjSettings <- function(type=c("win", "mac")) switch(match.arg(type),
                                                    "win"=flowUtils:::.fuEnv$winDefaults,
                                                    "mac"=flowUtils:::.fuEnv$macDefaults, stop("Unknown system!"))




## Create XML node of type 'tag' taking the default attributes unless
## specifically altered via the 'attrs' argument. Further children of 
## the node can be passed in as a list using 'children' or as named
## '...' arguments
xmlTag <- function(tag, attrs=NULL, children=NULL, system="win", ...)
{    
  mf <- list(...)
  tn <- if("namespace" %in% names(mf)) 
    paste(mf$namespace, tag, sep=":") else tag
  if(!is.list(children) || is(children, "XMLNode"))
    children <- list(children)
  xmlNode(name=tag, attrs=targs(tn, attrs=attrs, system=system), 
          .children=children, ...) 
}

xmlVertexNode <- function (xy) 
{
  xmlTag("vertex", namespace = "gating", children = lapply(xy, 
                                                           function(x) xmlTag("coordinate", namespace = "gating", 
                                                                              attrs = list(`data-type:value` = x))))
}
