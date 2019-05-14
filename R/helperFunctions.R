# This file was copied from the flowUtils package on 5/6/2019.
# This represents the work of Josef Spidlen and the other flowUtils 
# authors who are also accordingly credited in the authors list for CytoML.
# 
# Details:
# https://github.com/jspidlen/flowUtils/
# GitHub version: 1.35.8
# Bioconductor version: 1.49.0

#' @include gate-methods.R
NULL

#' @importFrom XML names.XMLNode
#' @importFrom flowCore unitytransform transformReference
getTransformationList <- function(dimensionList, flowEnv)
{  
    len=length(dimensionList)
    transformationList=list()
    while(len>0)
    {    
        subNodes=xmlChildren(dimensionList[[len]])[[1]]
        temp=switch(names.XMLNode(dimensionList[[len]]),
            "transformation"={
                              transName=sapply(xmlChildren(subNodes),xmlName)
                              dispatchTransform(transName,subNodes,flowEnv)
                             },
            "parameter"=unitytransform(xmlGetAttr(subNodes,"name")),
            "fcs-dimension"=unitytransform(xmlGetAttr(subNodes,"name")),
            "transformationReference"=transformReference(referenceId=as.character(xmlGetAttr(subNodes,"ref")),flowEnv)
                   ) 
        transformationList[[len]]=temp
        len=len-1
    }
    
    return(transformationList)
}

getTransformationListGml2 <- function(dimensionList, flowEnv)
{
    len = length(dimensionList)
    transformationRefs = list()
    compensationRefs = list()
    while (len > 0)
    {
        temp = xmlGetAttr(dimensionList[[len]], "transformation-ref")
        if (is.null(temp)) transformationRefs[[len]] = "unitytransform" 
        # Don't want to assign null to make sure the length of this list is right and the indexes match
        else transformationRefs[[len]] = temp

        temp = xmlGetAttr(dimensionList[[len]], "compensation-ref")
        if (is.null(temp)) compensationRefs[[len]] = "FCS"
        else compensationRefs[[len]] = temp

        len = len - 1
    }
    
    len = length(dimensionList)
    transformationList = list()
    while (len > 0)
    {   
        if (transformationRefs[[len]] == "unitytransform")
        {
            subNodes = xmlChildren(dimensionList[[len]])[[1]]
            temp = switch(names.XMLNode(dimensionList[[len]]),
                "fcs-dimension" = 
                {
                    parName = xmlGetAttr(subNodes, "name")
                    if (compensationRefs[[len]] == "FCS") 
                    {
                        compensatedParameter(
                            parameters=parName,
                            spillRefId="SpillFromFCS",
                            transformationId=paste(parName, "_compensated_according_to_FCS", sep=""),
                            searchEnv=flowEnv
                        )
                    }
                    else if (compensationRefs[[len]] == "uncompensated") unitytransform(parName)
                    else 
                    {
                        if (exists(parName, envir=flowEnv, inherits=FALSE) 
                            && class(flowEnv[[parName]])[1] == "compensatedParameter" 
                            && flowEnv[[parName]]@spillRefId == compensationRefs[[len]])
                        flowEnv[[parName]]
                        else 
                        {
                            write(paste("Failed to use spillover/spectrum matrix ", compensationRefs[[len]], " for compensated parameter ", parName, ". It seems that the matrix was not properly defined in the Gating-ML file.\n", sep=""), stderr())
                            unitytransform(parName)
                        }
                    }
                },
                "new-dimension" =
                {
                    newId = createOrUseGml2RatioTransformation(transformationRefs[[len]], compensationRefs[[len]], xmlGetAttr(subNodes, "transformation-ref"), flowEnv)
                    transformReference(referenceId=newId, flowEnv)
                }
            )
        }
        else
        {
            subNodes = xmlChildren(dimensionList[[len]])[[1]]
            temp = switch(names.XMLNode(dimensionList[[len]]),
                "fcs-dimension" = {
                    newId = createOrUseGml2Transformation(transformationRefs[[len]], compensationRefs[[len]], xmlGetAttr(subNodes, "name"), flowEnv)
                    transformReference(referenceId=newId, flowEnv)
                },
                "new-dimension" = 
                {
                    newId = createOrUseGml2RatioTransformation(transformationRefs[[len]], compensationRefs[[len]], xmlGetAttr(subNodes, "transformation-ref"), flowEnv)
                    transformReference(referenceId=newId, flowEnv)
                }
            )
        }
        transformationList[[len]]=temp
        len=len-1
    }
    
    transformationList
}

createOrUseGml2Transformation <- function(genericTransformationId, compensationRef, parameterName, flowEnv)
{
    appliedName <- paste(genericTransformationId, compensationRef, parameterName, sep = ".")
    if (!exists(appliedName, envir=flowEnv, inherits=FALSE))
    {
        if (compensationRef == "FCS")
            tempParameter = compensatedParameter(parameters=parameterName, spillRefId="SpillFromFCS", 
                transformationId=paste(parameterName, "_compensated_according_to_FCS", sep=""), searchEnv=flowEnv)
        else if (compensationRef == "uncompensated") tempParameter <- unitytransform(parameterName)
        else 
        {
            if (exists(parameterName, envir=flowEnv, inherits=FALSE) && class(flowEnv[[parameterName]])[1] == "compensatedParameter" 
                && flowEnv[[parameterName]]@spillRefId == compensationRef)
                tempParameter = flowEnv[[parameterName]]
            else 
            {
                write(paste("Failed to use spillover/spectrum matrix ", compensationRef, " for compensated parameter ", parameterName, 
                    ". It seems that the matrix was not properly defined in the Gating-ML file.\n", sep=""), stderr())
                tempParameter = unitytransform(parameterName)
            }
        }

        if (genericTransformationId == "unitytransform")
        {
            resultTransformation = tempParameter
        }
        else if (exists(genericTransformationId, envir=flowEnv, inherits=FALSE))
        {
            resultTransformation <- flowEnv[[genericTransformationId]]
            resultTransformation@parameters = tempParameter
            resultTransformation@transformationId = appliedName
        }
        else
        {
            write(paste("Failed to locate transformation ", genericTransformationId, 
                ". It seems that the transformation was not defined in the Gating-ML file. You won't be able to apply gates that are using this transformation.\n", 
                sep=""), stderr())
            resultTransformation = tempParameter
        }
        flowEnv[[appliedName]] = resultTransformation
    }
    appliedName
}


createOrUseGml2RatioTransformation <- function(genericTransformationId, compensationRef, ratioTransformationRef, flowEnv)
{
    fullRatioTransformationRef <- paste(genericTransformationId, compensationRef, ratioTransformationRef, sep = ".")
    if (genericTransformationId == "unitytransform" && compensationRef == "uncompensated") 
    {
        flowEnv[[fullRatioTransformationRef]] <- flowEnv[[ratioTransformationRef]]
        fullRatioTransformationRef
    }
    else
    {
        myRatioTr <- flowEnv[[ratioTransformationRef]]
        numeratorName <- myRatioTr@numerator@parameters
        denominatorName <- myRatioTr@denominator@parameters
        if (exists(fullRatioTransformationRef, envir=flowEnv, inherits=FALSE))
            fullRatioTransformationRef
        else
        {
            ## The code below is doing somethink like log(x)/log(y)
            #######################################################
            # fullNumeratorName <- createOrUseGml2Transformation(genericTransformationId, compensationRef, numeratorName, flowEnv)
            # fullDenominatorName <- createOrUseGml2Transformation(genericTransformationId, compensationRef, denominatorName, flowEnv)
            # appliedRatioTr <- myRatioTr
            # appliedRatioTr@numerator <- flowEnv[[fullNumeratorName]]
            # appliedRatioTr@denominator <- flowEnv[[fullDenominatorName]]
            # flowEnv[[fullRatioTransformationRef]] <- appliedRatioTr
            # fullRatioTransformationRef
            
            ## The code below is doing somethink like log(x/y)
            ## (this is what Gating-ML 2.0 asks for right now)
            fullNumeratorName <- createOrUseGml2Transformation("unitytransform", compensationRef, numeratorName, flowEnv)
            fullDenominatorName <- createOrUseGml2Transformation("unitytransform", compensationRef, denominatorName, flowEnv)
            appliedRatioTr <- myRatioTr
            appliedRatioTr@numerator <- flowEnv[[fullNumeratorName]]
            appliedRatioTr@denominator <- flowEnv[[fullDenominatorName]]
            appliedRatioTrRef <- paste("unitytransform", compensationRef, ratioTransformationRef, sep = ".")
            if (genericTransformationId == "unitytransform")
            {
                flowEnv[[fullRatioTransformationRef]] <- appliedRatioTr
                fullRatioTransformationRef
            }
            else
            {
                if (exists(genericTransformationId, envir=flowEnv, inherits=FALSE))
                {
                    resultTransformation <- flowEnv[[genericTransformationId]]
                    resultTransformation@parameters = appliedRatioTr
                    resultTransformation@transformationId = fullRatioTransformationRef
                    flowEnv[[fullRatioTransformationRef]] = resultTransformation
                    fullRatioTransformationRef
                }
                else
                {
                    stop(paste("Failed to locate transformation ", genericTransformationId, 
                                    ". It seems that the transformation was not defined in the Gating-ML file.", sep=""))
                }    
            }
        }
    }
}


getTransformationListForQuadrantGate <- function(quadrant, dividers, transformations, compensations, flowEnv) 
{
    len <- length(quadrant)
    transformationList <- list()

    while(len > 0)
    {   
        name <- names(quadrant)[[len]]
        parameterName <- dividers[[name]][[length(dividers[[name]])-1]]
        itIsRatio = dividers[[name]][[length(dividers[[name]])]]
        transformationRef <- transformations[[name]]
        compensationRef <- compensations[[name]]

        if (itIsRatio)
        {
            referencedName <- createOrUseGml2RatioTransformation(transformationRef, compensationRef, parameterName, flowEnv)
            transformationList[[len]] <- flowEnv[[referencedName]]
        }
        else
        {
            if (transformationRef == "unitytransform")
            {
                if (compensationRef == "FCS") 
                {
                    transformationList[[len]] <- compensatedParameter(
                        parameters=parameterName,
                        spillRefId="SpillFromFCS",
                        transformationId=paste(parameterName, "_compensated_according_to_FCS", sep=""),
                        searchEnv=flowEnv)
                }
                else if (compensationRef == "uncompensated") transformationList[[len]] <- unitytransform(parameterName)
                else 
                {
                    if (exists(parameterName, envir=flowEnv, inherits=FALSE) 
                        && class(flowEnv[[parameterName]])[1] == "compensatedParameter" 
                        && flowEnv[[parameterName]]@spillRefId == compensationRef)
                        transformationList[[len]] <- flowEnv[[parameterName]]
                    else 
                    {
                        write(paste("Failed to use spillover/spectrum matrix ", compensationRef, " for compensated parameter ", parameterName, ". It seems that the matrix was not properly defined in the Gating-ML file.\n", sep=""), stderr())
                        transformationList[[len]] <- unitytransform(parameterName)
                    }
                }
            }
            else
            {
                newId = createOrUseGml2Transformation(transformationRef, compensationRef, parameterName, flowEnv)
                transformationList[[len]] <- transformReference(referenceId=newId, flowEnv)
            }
        }

        len <- len-1
    }

    transformationList
}

getElementValueAsNumeric <- function(element)
{
    # This is ugly but it just extracts a numeric value of an element, 
    # i.e., <value>500</value> --> 500
    as.numeric((as.character((xmlChildren(element))[['text']]))[[6]])
}

getBounds <- function(value, name, dividers)
{
    lowerBound = -Inf
    upperBound = +Inf
    for (i in seq(length(dividers[[name]])-2))
    {
        if (dividers[[name]][[i]] < value) lowerBound = dividers[[name]][[i]] 
    }
    for (i in (length(dividers[[name]])-2):1)
    {
        if (dividers[[name]][[i]] > value) upperBound = dividers[[name]][[i]] 
    }
    c(lowerBound, upperBound)
}
    
getParameterList<-function(node,type,flowEnv)
{   parameters=list()
    nodeNames=names.XMLNode(node[[1]])
    len=length(nodeNames)
    subNodes=xmlChildren(node[[1]])

    for (i in seq(len))
    {
        temp=switch(nodeNames[i],
        "transformation"=
         {  
            if(type==0)
                trans<-subNodes     
            else
                trans<-subNodes[[i]]
            transName=sapply(xmlChildren(subNodes[[i]]),xmlName)
            dispatchTransform(transName,trans,flowEnv)
        },
        "parameter"=unitytransform(xmlGetAttr(subNodes[[i]],"name")),
        "transformationReference"=transformReference(xmlGetAttr(subNodes[[i]],"ref"),flowEnv)
                  ) 
        parameters[[i]]=temp
    }
    return(parameters)
}

# Note that there is a typo in examples 45,46, and 47 in the Gating-ML 2.0 (Version 2.0 – 2013-01-22) PDF
# Therefore, this function is made to be able to parse the fratio transformation with fcs-dimension
# elements being either childern of "fratio", or childern of the parent "transformation" element.
# So we can parse parameters from either this
# <transforms:transformation transforms:id="myRatio">
#   <transforms:fratio transforms:A="5" transforms:B="1" transforms:C="2">
#     <data-type:custom_info>Custom info may be part of any transformation definition.</data-type:custom_info>
#     <data-type:fcs-dimension data-type:name="PE-A" />
#     <data-type:fcs-dimension data-type:name="APC-A" />
#   </transforms:fratio>
# </transforms:transformation>
# or this
# <transforms:transformation transforms:id="myRatio">
#   <transforms:fratio transforms:A="5" transforms:B="1" transforms:C="2" />
#   <data-type:custom_info>Custom info may be part of any transformation definition.</data-type:custom_info>
#   <data-type:fcs-dimension data-type:name="PE-A" />
#   <data-type:fcs-dimension data-type:name="APC-A" />
# </transforms:transformation>
# (the custom info element is always optional) 
getGatingML2RatioParameterList <- function(node, flowEnv)
{
    parameters = list()
    if (length(node) == 1) 
        node = xmlChildren(node[[1]])
    len = length(node)
    if (len < 2 || len > 4)
    {
        stop("Failed to parse ratio tranformation. It seems that it wasn't defined properly in the Gating-ML file.\n")
    } 
    else
    {
        parameters[[1]] = unitytransform(xmlGetAttr(node[[len-1]], "name"))
        parameters[[2]] = unitytransform(xmlGetAttr(node[[len]], "name"))
    }
    parameters
}

getFluorochromeList<-function(node, flowEnv)
{   
    parameters=list()
    nodeNames=names.XMLNode(node[["fluorochromes"]])
    len=length(nodeNames)
    subNodes=xmlChildren(node[["fluorochromes"]])

    for (i in seq(len))
    {
        temp=switch(nodeNames[i],
            "fcs-dimension"=unitytransform(xmlGetAttr(subNodes[[i]], "name"))
        ) 
        parameters[[i]]=temp
    }
    return(parameters)
}

getDetectorList<-function(node, flowEnv)
{   
    parameters=list()
    nodeNames=names.XMLNode(node[["detectors"]])
    len=length(nodeNames)
    subNodes=xmlChildren(node[["detectors"]])

    for (i in seq(len))
    {
        temp=switch(nodeNames[i],
            "fcs-dimension"=unitytransform(xmlGetAttr(subNodes[[i]],"name"))
        ) 
        parameters[[i]]=temp
    }
    return(parameters)
}

getSide = function(g,side) 
    {       
          leaf = paste("http...www.isac.net.org.std.Gating.ML.v1.5.gating_leaf",side,sep="")
          node = paste("http...www.isac.net.org.std.Gating.ML.v1.5.gating_node",side,sep="")
          VAL=xmlElementsByTagName(g,paste("leaf",side,sep=""))
          if(length(VAL)==0) 
          {
            VAL=  xmlElementsByTagName(g,paste("node",side,sep=""))
            if(length(VAL)==0) stop(paste(leaf,"or",node,"is required at all levels of a decision tree."))
          }
          VAL[[1]]
    }
            
makeCall = function(param,thres,LT,GTE) 
        {       
          NUM = if((is.logical(LT) && LT) || is.call(LT)) 1 else 0
          NUM = NUM + if((is.logical(GTE) && GTE) || is.call(GTE)) 2 else 0
            
          LESS  = as.call(c(as.symbol("<"),as.symbol(param),thres))
          MORE  = as.call(c(as.symbol(">="),as.symbol(param),thres))
          switch(NUM+1,
          {FALSE},
          {if(is.logical(LT)) LESS else as.call(c(as.symbol("&"),LESS,LT))},
          {if(is.logical(GTE)) MORE else as.call(c(as.symbol("&"),MORE,GTE))},
          {as.call(c(as.symbol("|"),as.call(c(as.symbol("&"),LESS,LT)),as.call(c(as.symbol("&"),MORE,GTE))))}
                )
        }
decisionHelper = function(g,...) 
{       
          
      thres = xmlGetAttr(g,"threshold",Inf,as.numeric)
      param = getParameters(xmlChildren(g)[[1]])
      LT    = getSide(g,"LT")
      GTE   = getSide(g,"GTE")
      if(is(LT,"http...www.isac.net.org.std.Gating.ML.v1.5.gating_leafLT"))
      {       
              LT  = if(xmlGetAttr(LT,"inside")=="true") TRUE else FALSE 
      }    
      else
      {
              LT  = decisionHelper(LT)
      }
      if(is(GTE,"http...www.isac.net.org.std.Gating.ML.v1.5.gating_leafGTE")) 
      {
              GTE = if(xmlGetAttr(GTE,"inside")=="true") TRUE else FALSE 
      }
      else 
      {    
              GTE = decisionHelper(GTE)
      }
      
      makeCall(param,thres,LT,GTE)
        
}            


    
smartTreeParse = function(file,...) 
{
  handlers = list(comment=function(x,...)
  {
      NULL
  },
  startElement=function(x,...) 
  {
      class(x)=c(paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_"),make.names(xmlNamespace(x)),class(x))
      x
  }
  )
  xmlTreeParse(file,handlers=handlers,asTree=TRUE,fullNamespaceInfo=TRUE,...)
}