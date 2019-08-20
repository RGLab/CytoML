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
                                                    "win"=.fuEnv$winDefaults,
                                                    "mac"=.fuEnv$macDefaults, stop("Unknown system!"))




## Create XML node of type 'tag' taking the default attributes unless
## specifically altered via the 'attrs' argument. Further children of 
## the node can be passed in as a list using 'children' or as named
## '...' arguments
xmlTag <- function(tag, attrs=NULL, children=NULL, system="win", ...)
{    
  mf <- list(...)
  tn <- if("namespace" %in% names(mf)) 
    paste(mf$namespace, tag, sep=":") else tag
  if((!is.list(children) || is(children, "XMLNode"))&&!is.null(children))
    children <- list(children)
  xmlNode(name=tag, attrs=targs(tn, attrs=attrs, system=system), 
          .children=children, ...) 
}

xmlVertexNode <- function (xy) 
{
  xmlTag("vertex", namespace = "gating", children = lapply(xy, 
                                                           function(x) xmlTag("coordinate", namespace = "gating", 
                                                                              attrs = list(`data-type:value` = as.character(format_float(x))))))
}

# internals copied from flowUtils to avoid :::
#' @importFrom XML xmlNamespace
smartTreeParse <- function (file, ...) 
{
  handlers = list(comment = function(x, ...) {
    NULL
  }, startElement = function(x, ...) {
    class(x) = c(paste(make.names(c(xmlNamespace(x), xmlName(x))), 
                       collapse = "_"), make.names(xmlNamespace(x)), class(x))
    x
  })
  xmlTreeParse(file, handlers = handlers, asTree = TRUE, fullNamespaceInfo = TRUE, 
               ...)
}

# internals copied from flowUtils to avoid :::
#' @importFrom  XML xmlSApply
.fuEnv <-  new.env(parent=emptyenv())
.onLoad <- function(...)
{	
  
  mdef <- xmlSApply(xmlTreeParse(system.file("defaults.xml",
                                             package="CytoML"),
                                 addAttributeNamespaces=TRUE)[["doc"]][[1]][["macdefaults"]],
                    function(x)
                      if(!is(x, "XMLCommentNode")) as.list(xmlAttrs(x)))
  wdef <- xmlSApply(xmlTreeParse(system.file("defaults.xml",
                                             package="CytoML"),
                                 addAttributeNamespaces=TRUE)[["doc"]][[1]][["windefaults"]],
                    function(x)
                      if(!is(x, "XMLCommentNode")) as.list(xmlAttrs(x)))
  mdef <- mdef[!sapply(mdef, is.null)]
  wdef <- wdef[!sapply(wdef, is.null)]
  assign("winDefaults", wdef, .fuEnv)
  assign("macDefaults", mdef, .fuEnv)
}