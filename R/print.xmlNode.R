insertEntities <- XML:::insertEntities
#' overwrite the XML::saveXML.sink to avoid indent and tagseparator for cdata
print.XMLNode.ex <- function(x, ..., indent = "", tagSeparator = "\n")
  {
    if(length(xmlAttrs(x))) {
      tmp <- paste(names(xmlAttrs(x)),paste("\"", insertEntities(xmlAttrs(x)), "\"", sep=""), sep="=", collapse=" ")
    } else
      tmp <- ""

    if(length(x$namespaceDefinitions) > 0) {
      k = as(x$namespaceDefinitions, "character")
      ns = paste("xmlns", ifelse(nchar(names(k)), ":", ""), names(k), "=", ddQuote(k), sep = "", collapse = " ")
      #   ns <- paste(sapply(x$namespaceDefinitions,
      #                       function(x) {
      #                            paste("xmlns", if(nchar(x$id) > 0) ":" else "", x$id, "=", "\"", x$uri, "\"", sep="")
      #                       }), collapse=" ")

    } else
      ns <- ""


    # Add one space to the indentation level for the children.
    # This will accumulate across successive levels of recursion.
    subIndent <- paste(indent, " ", sep="")
    if(is.logical(indent) && !indent) {
      indent <- ""
      subIndent <- FALSE
    }


    if (length(xmlChildren(x)) == 0) {
      ## Empty Node - so difference is <nodename />
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "",
                                                      " ", ""), tmp, ifelse(ns != "", " ", ""), ns, "/>", tagSeparator,
                        sep = ""), sep = "")
    } else if (length(xmlChildren(x))==1 &&
               inherits(xmlChildren(x)[[1]],"XMLTextNode")) {
      ## Sole child is text node, print without extra white space.
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "",
                                                      " ", ""), tmp, ifelse(ns != "", " ", ""), ns, ">",
                        sep = ""), sep = "")
      kid = xmlChildren(x)[[1]]
      if(inherits(kid, "EntitiesEscaped"))
        txt = xmlValue(kid)
      else
        txt = insertEntities( xmlValue(kid) )

      cat(txt,sep="")
      cat(paste("</", xmlName(x, TRUE), ">", tagSeparator,
                sep = ""), sep = "")
    } else if(length(xmlChildren(x))==1 &&
              inherits(xmlChildren(x)[[1]],"XMLCDataNode")) {
      cat(paste("<", xmlName(x, TRUE), ifelse(tmp != "",
                                                      " ", ""), tmp, ifelse(ns != "", " ", ""), ns, ">",
                        sep = ""), sep = "")
      kid = xmlChildren(x)[[1]]
      print(kid, indent = "", tagSeparator = "")


      cat(paste("</", xmlName(x, TRUE), ">", tagSeparator,
                        sep = ""), sep = "")
    }else {
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "",
                                                      " ", ""), tmp, ifelse(ns != "", " ", ""), ns, ">", tagSeparator,
                        sep = ""), sep = "")
      for (i in xmlChildren(x))
         print.XMLNode.ex(i, indent = subIndent, tagSeparator = tagSeparator)

      cat(indent, paste("</", xmlName(x, TRUE), ">", tagSeparator,
                        sep = ""), sep = "")
    }
  }
#' overwrite saveXML.link to dispatch customed print.XMLNode
saveXML.link.ex =
  #
  # Need to handle a DTD here as the prefix argument..
  #
  function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
           doctype = NULL, encoding = getEncoding(doc), ...)
  {
    asString = is.null(file)
    if(asString)
      file = textConnection(NULL, "w")

    if(inherits(file, c("character", "connection"))) {
      sink(file)
      on.exit(sink())
    }

    if(!is.null(prefix))
      cat(as.character(prefix))

    if(!is.null(doctype))
      cat(as(doctype, "character"), '\n')

    #XXX Should we return file if it is not missing() || NULL ???

    print.XMLNode.ex(doc)

    if(asString)
      paste(textConnectionValue(file), collapse = "\n")
    else
      file
  }