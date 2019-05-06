setGeneric(
    "identifyNode",
    def=function(object, flowEnv, ...) standardGeneric("identifyNode"),
    useAsDefault=function(object, flowEnv, ...)
    {
        stop(paste("Not a supported node in Gating-ML format:", paste(object, collapse = ", "), sep = " "))
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.datatypes_custom_info",
    function(object, flowEnv, ...) { }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_PolygonGate",
    function(object, flowEnv, ...)
    {
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 2) 
        {
            gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
            parentId = (xmlGetAttr(object, "parent_id", "NULL"))
            dimensionList= xmlElementsByTagName(object, "dimension")
            len = length(dimensionList)
            transformationList <- getTransformationListGml2(dimensionList, flowEnv)
            vertexList = xmlElementsByTagName(object, "vertex")
            len = length(vertexList)
            vertexLimits = matrix(nrow=len, ncol=length(dimensionList))
            for (i in seq(len)) vertexLimits[i,] = getParameters(vertexList[[i]]) 
            filt = polygonGate(filterId=gateId, .gate=vertexLimits, transformationList)

            if(parentId == "NULL")
            {
                flowEnv[[as.character(gateId)]] = filt
            }
            else
            {
                temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
                flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)    
            }
        }
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_RectangleGate",
    function(object, flowEnv, ...)
    {   
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 2) 
        {
            gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
            parentId = (xmlGetAttr(object, "parent_id", "NULL"))
            dimensionList = xmlElementsByTagName(object, "dimension")
            len = length(dimensionList)
            gateLimits = matrix(nrow=2, ncol=len)
            for (i in seq(len)) 
            {
                gateLimits[,i] = getParameters(dimensionList[[i]]) 
            }
            transformationList <- getTransformationListGml2(dimensionList, flowEnv)
            filt = rectangleGate(filterId=gateId, .gate=gateLimits, transformationList)
            if (parentId == "NULL")
            {
                flowEnv[[as.character(gateId)]] = filt
            }
            else
            {
                temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
                flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
            }
        }
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_EllipsoidGate",
    function(object, flowEnv, ...)
    {  
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 2) 
        {
            gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
            parentId = (xmlGetAttr(object, "parent_id", "NULL"))
            dimensionList = xmlElementsByTagName(object, "dimension")
            len = length(dimensionList)
            transformationList <- getTransformationListGml2(dimensionList, flowEnv)
            meanList = xmlElementsByTagName(object, "mean")
            meanLimits = getParameters(meanList[[1]])
            covarianceList = xmlChildren(xmlElementsByTagName(object, "covarianceMatrix")[[1]])
            len=length(covarianceList)
            covMat = matrix(nrow=len, ncol=length(dimensionList))
            for (i in seq(len)) 
            { 
                covMat[i,] = getParameters(covarianceList[[i]]) 
            }  
            distance = sqrt(getParameters(xmlElementsByTagName(object, "distanceSquare")[[1]]))
            filt = ellipsoidGate(filterId=gateId, .gate=covMat, mean=meanLimits, distance=distance, transformationList)
            if(parentId == "NULL")
            {
                flowEnv[[as.character(gateId)]] = filt
            }
            else
            {
                temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
                flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
            }
        }
    }
) 

# J. Spidlen, Oct 23, 2013:
# It seems that flowCore's Quad gate will not work for this easily since we want to
# have custom identifiers of these quads and want to be able to reference these
# as parent populations or Boolean gate arguments. Also, it doesn't seem to be 
# possible to use the quadGate on top of compensated or scaled parameters (since
# the constructor is taking parameter names and not transformations). So we are
# simply creating n-dimensional rectangle gates for all the Quads from here.
setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_QuadrantGate",
    function(object, flowEnv, ...)
    {   
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 2) 
        {
            gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
            parentId = (xmlGetAttr(object, "parent_id", "NULL"))
            dividerList = xmlElementsByTagName(object, "divider")
            dividers <- list()
            transformations <- list()
            compensations <- list()
            for (i in seq(length(dividerList)))
            {
                fcsDimension = xmlElementsByTagName(dividerList[[i]], "fcs-dimension")
                itIsRatio = FALSE
                if (length(fcsDimension) == 0) 
                {
                    fcsDimension = xmlElementsByTagName(dividerList[[i]], "new-dimension")
					name = xmlGetAttr(fcsDimension[[1]], "transformation-ref")
                    itIsRatio = TRUE
                }
                else 
                    name = xmlGetAttr(fcsDimension[[1]], "name")
				
				dividerValues <- list()
                dividerId = (xmlGetAttr(dividerList[[i]], "id", genid(flowEnv)))
                transformationRef = (xmlGetAttr(dividerList[[i]], "transformation-ref", "unitytransform"))
                compensationRef = (xmlGetAttr(dividerList[[i]], "compensation-ref", "FCS"))
                values = xmlElementsByTagName(dividerList[[i]], "value")
                for (j in seq(length(values)))
                {
                    dividerValues[[j]] = getElementValueAsNumeric(values[[j]]) 
                }
                dividerValues[[j + 1]] = as.character(name)
				dividerValues[[j + 2]] = itIsRatio
                dividers[[as.character(dividerId)]] = dividerValues
                transformations[[as.character(dividerId)]] = transformationRef
                compensations[[as.character(dividerId)]] = compensationRef
            }
        
            quadrantList = xmlElementsByTagName(object, "Quadrant")
            for (i in seq(length(quadrantList)))
            {
                quadrant <- getParameters(quadrantList[[i]])
                quadrantId = (xmlGetAttr(quadrantList[[i]], "id", genid(flowEnv)))
                len = length(quadrant)
                gateLimits = matrix(nrow=2, ncol=len)
                for (i in seq(len)) 
                {
                    gateLimits[,i] = getBounds(quadrant[[i]], names(quadrant)[[i]], dividers)
                }

                transformationList <- getTransformationListForQuadrantGate(quadrant, dividers, transformations, compensations, flowEnv)
                filt = rectangleGate(filterId=quadrantId, .gate=gateLimits, transformationList)
                if (parentId == "NULL")
                {
                    flowEnv[[as.character(quadrantId)]] = filt
                }
                else
                {
                    temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
                    flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
                }
            }    
        }        
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_BooleanGate",
    function(object, flowEnv, ...)
    {  
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 2) 
        {
            gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
            parentId = (xmlGetAttr(object, "parent_id", "NULL"))
            childern = xmlChildren(object)
            filt <- identifyNode(childern[[length(childern)]], flowEnv)
            if (parentId == "NULL")
            {
                filt@filterId = as.character(gateId)
                flowEnv[[as.character(gateId)]] = filt
            }
            else
            {
                temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
                flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
            }
        }
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_gateReference",
    function(object, flowEnv, ...)
    {     
        gateRefId = getParameters(object)
        useAsComplement = xmlGetAttr(object, "use-as-complement")
        gRef <- new("filterReference", name=gateRefId, env=flowEnv, filterId=gateRefId)
        if ((!is.null(useAsComplement)) && (useAsComplement == "true" || useAsComplement == "1"))
        {
            complFiltId = paste("Not", gateRefId, sep="_")
            complFilter <- new("complementFilter", filterId=complFiltId, filters=list(gRef))
            flowEnv[[complFiltId]] = complFilter
            new("filterReference", name=complFiltId, env=flowEnv, filterId=complFiltId)
        }
        else
            gRef
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_and",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        while (len > 0)
        {   
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
            len = len-1
        }
        new("intersectFilter", filterId="", filters=parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_or",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        while (len > 0)
        {
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
            len = len-1
        }
        new("unionFilter", filterId="", filters=parameters)
    }
)


setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_not",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        if (len == 1)
        {
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
        }
        else
        {
            stop("Not element should have only one operand")
        }
        new("complementFilter", filterId="", filters=parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_and",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        while (len > 0)
        {   
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
            len = len-1
        }
        new("intersectFilter", filterId="", filters=parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_or",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        while (len > 0)
        {
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
            len = len-1
        }
        new("unionFilter", filterId="", filters=parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_not",
    function(object, flowEnv, ...)
    {
        gateList = xmlChildren(object)
        len = length(gateList)
        parameters = list()
        if (len == 1)
        {
            parameters[[len]] = identifyNode(gateList[[len]], flowEnv)
        }
        else
        {
            stop("Not element should have only one operand")
        }
      new("complementFilter", filterId="", filters=parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_gateReference",
    function(object, flowEnv, ...)
    {     
        gateRefId = getParameters(object)
        new("filterReference", name=gateRefId, env=flowEnv, filterId=gateRefId)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_BooleanGate",
    function(object, flowEnv, ...)
    {  
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        filt <- identifyNode(xmlChildren(object)[[1]], flowEnv)
        if (parentId == "NULL")
        {
			filt@filterId = as.character(gateId)
			flowEnv[[as.character(gateId)]] = filt
        }
        else
    {
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
        }
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_RectangleGate",
    function(object, flowEnv, ...)
    {   
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        dimensionList = xmlElementsByTagName(object, "dimension")
        len = length(dimensionList)
        gateLimits = matrix(nrow=2, ncol=len)
        for (i in seq(len)) 
        {
            gateLimits[,i] = getParameters(dimensionList[[i]]) 
        }  
        transformationList <- getTransformationList(dimensionList, flowEnv)
        filt=rectangleGate(filterId=gateId, .gate=gateLimits, transformationList)
        if (parentId == "NULL")
        {
            flowEnv[[as.character(gateId)]] = filt
        }
        else
        {
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
        }
    }
) 

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_PolygonGate",
    function(object, flowEnv, ...)
    {  
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        dimensionList = xmlElementsByTagName(object, "dimension")
        len = length(dimensionList)
        transformationList <- getTransformationList(dimensionList, flowEnv)
        vertexList = xmlElementsByTagName(object, "vertex")
        len = length(vertexList)
        vertexLimits = matrix(nrow=len, ncol=length(dimensionList))
        for (i in seq(len)) 
        {
            vertexLimits[i,] = getParameters(vertexList[[i]]) 
        }  
        filt = polygonGate(filterId=gateId,.gate=vertexLimits,transformationList)
        if (parentId == "NULL")
        {
            flowEnv[[as.character(gateId)]] = filt
        }
        else
        {
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)    
        }                      
    }
) 


setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_EllipsoidGate",
    function(object, flowEnv, ...)
    {  
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        dimensionList= xmlElementsByTagName(object, "dimension")
        len = length(dimensionList)
        transformationList <- getTransformationList(dimensionList, flowEnv)
        meanList = xmlElementsByTagName(object, "mean")
        meanLimits = getParameters(meanList[[1]])
        covarianceList = xmlChildren(xmlElementsByTagName(object, "covarianceMatrix")[[1]])
        len = length(covarianceList)
        covMat = matrix(nrow=len, ncol=length(dimensionList))
        for (i in seq(len)) 
        {
            covMat[i,] = getParameters(covarianceList[[i]]) 
        }  
        distance = sqrt(getParameters(xmlElementsByTagName(object, "distanceSquare")[[1]]))
        filt = ellipsoidGate(filterId=gateId, .gate=covMat, mean=meanLimits, distance=distance, transformationList)
        if (parentId == "NULL")
        {
            flowEnv[[as.character(gateId)]] = filt
        }
        else
        {
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
        }
    }
) 
             
setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_PolytopeGate",
    function(object, flowEnv, ...)
    {  
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        dimensionList = xmlElementsByTagName(object, "dimension")
        len = length(dimensionList)
        transformationList <- getTransformationList(dimensionList, flowEnv)
        halfList = xmlElementsByTagName(object, "halfspace")
        len = length(halfList)
        halfValues = matrix(nrow=len, ncol=length(dimensionList) + 1)
        for (i in seq(len)) 
        {
            halfValues[i,] = sapply(xmlChildren(halfList[[i]]), getParameters) 
        }
        a = matrix(halfValues[,1:length(dimensionList)], ncol=length(dimensionList))
        b = (halfValues[,length(dimensionList)+1])
        filt = polytopeGate(filterId=gateId, .gate=a, b=b, transformationList)
        if (parentId == "NULL")
        {
            flowEnv[[as.character(gateId)]] = filt
        }
        else
        {   
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
        }
    }
)      
        
setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_DecisionTreeGate",
    function(object, flowEnv, ...)
    {  
        gateId = (xmlGetAttr(object, "id", genid(flowEnv)))
        parentId = (xmlGetAttr(object, "parent_id", "NULL"))
        root = xmlChildren(object)[[1]]
        test = decisionHelper(root)
        filt = do.call(expressionFilter, list(expr=test))
        if (parentId == "NULL")
        {
            flowEnv[[as.character(gateId)]] = filt
        }
        else
        {  
            temp = new("filterReference", name=parentId, env=flowEnv, filterId="NULL")
            flowEnv[[as.character(gateId)]] = new("subsetFilter", filters=list(filt, temp), filterId=gateId)
        }
    }
)
