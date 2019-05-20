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

transCompensation <- function(node, flowEnv)
{
    transformationId = (xmlGetAttr(node, "id", genid(flowEnv)))
    tempComp = xmlElementsByTagName(node, "compensation")
    spillRef = xmlGetAttr(tempComp[[1]], "spilloverMatrixRef")
    parameter = getParameters(xmlChildren(tempComp[[1]])[[1]])
    compensatedParameter(
        parameters=parameter,
        spillRefId=spillRef,
        transformationId=transformationId,
        searchEnv=flowEnv
    )
}

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.transformations_spilloverMatrix",
    function(object, flowEnv, ...)
    {
        transformationId = (xmlGetAttr(object, "id", genid(flowEnv)))
        tempCoeff = xmlElementsByTagName(object, "coefficient", recursive=TRUE)
        coefficients = as.numeric(sapply(tempCoeff, getParameters))
        parameters <- getParameterList(object, 0, flowEnv)  
        spillMatrix = matrix(coefficients, ncol=length(parameters), byrow=TRUE)
        colnames(spillMatrix) = sapply(parameters, getParameters)
        flowEnv[[as.character(transformationId)]] =
            compensation(spillover=spillMatrix, compensationId=as.character(transformationId), parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.transformations_spectrumMatrix",
    function(object, flowEnv, ...)
    {
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 1)
        {
            transformationId = (xmlGetAttr(object, "id", genid(flowEnv)))
            invertedAlready = (xmlGetAttr(object, "matrix-inverted-already", "false")) 
            invertedAlready = (invertedAlready == "true" || invertedAlready == "1")
            tempCoeff = xmlElementsByTagName(object, "coefficient", recursive=TRUE)
            coefficients = as.numeric(sapply(tempCoeff, getParameters))
            fluorochromes = getFluorochromeList(object, flowEnv)
            detectors = getDetectorList(object, flowEnv)

            if (invertedAlready)
            {
                # If the matrix has been inverted already, we will invert it back and
                # always just store the non-inverted spillover/spectrum matrix.
                spillMatrix = matrix(coefficients, ncol=length(fluorochromes), byrow=TRUE)
                spillMatrix = pseudoinverse(spillMatrix)
            }
            else
            {
                spillMatrix = matrix(coefficients, ncol=length(detectors), byrow=TRUE)
            }
            colnames(spillMatrix) = sapply(detectors, getParameters)
            rownames(spillMatrix) = sapply(fluorochromes, getParameters)
            spillId = as.character(transformationId) 
            flowEnv[[spillId]] = compensation(spillover=spillMatrix, compensationId=spillId, detectors)

            len = length(fluorochromes)
            while (len > 0)
            {
                compPar = compensatedParameter(
                    parameters=as.character(detectors[[len]]@parameters),
                    spillRefId=spillId,
                    transformationId=as.character(fluorochromes[[len]]@parameters),
                    searchEnv=flowEnv
                )
                flowEnv[[as.character(fluorochromes[[len]]@parameters)]] = compPar
                len = len - 1
            }
        }
    }
)
