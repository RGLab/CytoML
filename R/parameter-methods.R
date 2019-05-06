# This file was copied from the flowUtils package on 5/6/2019.
# This represents the work of Josef Spidlen and the other flowUtils 
# authors who are also accordingly credited in the authors list for CytoML.
# 
# Details:
# https://github.com/jspidlen/flowUtils/
# GitHub version: 1.35.8
# Bioconductor version: 1.49.0

setGeneric(
    "getParameters",
    def=function(object, ...) standardGeneric("getParameters"),
    useAsDefault=function(object, ...)
    {
        stop(paste("Not a supported Parameter node in GatingML 1.5 or 2.0 format:", paste(object, collapse = ", "), sep = " "))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_vertex",
    function(object, ...)
    {   
        as.numeric(sapply(xmlChildren(object), xmlGetAttr,"value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.datatypes_parameter",
    function(object, ...)
    {
        xmlGetAttr(object, "name")     
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_dimension",
    function(object, ...)
    {     
        c(xmlGetAttr(object, "min", default=-Inf, as.numeric), xmlGetAttr(object, "max", default=Inf, as.numeric))     
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_mean",
    function(object, ...)
    {   
        sapply(sapply(xmlChildren(object), xmlGetAttr,"value"), as.numeric)
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_row",
    function(object, ...)
    {   
      sapply(sapply(xmlChildren(object), xmlGetAttr, "value"), as.numeric)
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_distanceSquare",
    function(object, ...)
    {
        as.numeric(xmlGetAttr(object, "value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_Quadrant",
    function(object, ...)
    {   
        ret <- list()
        positionList = xmlElementsByTagName(object, "position")
        for (i in seq(length(positionList)))
        {
            ret[[as.character(xmlGetAttr(positionList[[i]], "divider_ref"))]] <- as.numeric(xmlGetAttr(positionList[[i]], "location")) 
        }
        ret
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.gating_gateReference",
    function(object, ...)
    {
        refId = as.character(xmlGetAttr(object, "ref"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v2.0.transformations_coefficient",
    function(object, ...)
    {      
        as.numeric(xmlGetAttr(object, "value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_dimension",
    function(object, ...)
    {     
        c(xmlGetAttr(object, "min", default=-Inf, as.numeric), xmlGetAttr(object, "max", default=Inf, as.numeric))     
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.datatypes_parameter",
    function(object, ...)
    {
        xmlGetAttr(object, "name")     
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_vertex",
    function(object, ...)
    {   
        as.numeric(sapply(xmlChildren(object), xmlGetAttr, "value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_entry",
    function(object, ...)
    {   
        as.numeric(xmlGetAttr(object, "value"))
    }
)
            
setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.transformations_coefficient",
    function(object, ...)
    {      
        as.numeric(xmlGetAttr(object, "value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_mean",
    function(object, ...)
    {   
        sapply(sapply(xmlChildren(object), xmlGetAttr, "value"), as.numeric)
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_row",
    function(object, ...)
    {   
        sapply(sapply(xmlChildren(object), xmlGetAttr, "value"), as.numeric)
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_distanceSquare",
    function(object, ...)
    {    
        as.numeric(xmlGetAttr(object, "value"))
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.transformations_transformationReference",
    function(object, ...)
    {    
        xmlGetAttr(object, "ref")
    }
)

setMethod(
    "getParameters",
    "http...www.isac.net.org.std.Gating.ML.v1.5.gating_gateReference",
    function(object, ...)
    {
        refId = as.character(xmlGetAttr(object, "ref"))
    }
)

setMethod(
    "getParameters",
    "unitytransform",
    function(object, ...)
    {
        as.character(object@parameters)
    }
)

setMethod(
    "getParameters",
    "transformReference",
    function(object, ...)
    {
        as.character(object@transformationId)
    }
)
