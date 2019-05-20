# This file was copied from the flowUtils package on 5/6/2019.
# This represents the work of Josef Spidlen and the other flowUtils 
# authors who are also accordingly credited in the authors list for CytoML.
# 
# Details:
# https://github.com/jspidlen/flowUtils/
# GitHub version: 1.35.8
# Bioconductor version: 1.49.0

####Generic function
setGeneric("parseGatingML",def=function(object,flowEnv,...)
standardGeneric("parseGatingML"),
	    useAsDefault=function(object,flowEnv,...)
			 {
				stop("Not a supported Gating-ML XML format")
			 }
	  )
 
genid = function(flowEnv)
          {
              flowEnv$MYidnum <- flowEnv$MYidnum + 1;
              paste("genid",flowEnv$MYidnum,sep="")
          }

### Methods definitions
setMethod("parseGatingML","http...www.isac.net.org.std.Gating.ML.v1.5.gating_Gating.ML",
          function(object,flowEnv,...)
          {   
              flowEnv$MYidnum = 0
              flowEnv$GatingMLVersion = 1.5
        
        
              for (node in xmlChildren(object))
              {
                  identifyNode(node,flowEnv);
              }
              
        }
      )

setMethod("parseGatingML", "http...www.isac.net.org.std.Gating.ML.v2.0.gating_Gating.ML",
          function(object, flowEnv, ...)
          {
              flowEnv$MYidnum = 0
              flowEnv$GatingMLVersion = 2.0
              for (node in xmlChildren(object))
              {
                  identifyNode(node, flowEnv);
              }
          }
)


read.gatingML = function(file,flowEnv,...)
{       
    flowEnv[['.flowUtilsRead.GatingML.PassNo']] <- 1
    parseGatingML(xmlRoot(smartTreeParse(file,...)),flowEnv)
    if (flowEnv[['GatingMLVersion']] == 2)
    {
        # Gating-ML 2.0 is parsed twice, gates are extracted at the
        # second pass to make sure we have all the transformations already.
        # This is since transformations are being reused for different FCS 
        # parameters and creating "placeholders" caused issues with ellipse gates 
        # (unable to find an inherited method for function ‘parameters’ for signature ‘"NULL"’)
        flowEnv[['.flowUtilsRead.GatingML.PassNo']] <- 2
        parseGatingML(xmlRoot(smartTreeParse(file,...)),flowEnv)
    }

    rm('.flowUtilsRead.GatingML.PassNo', envir=flowEnv)

    # This is no longer done since we do a two pass parsing of Gating-ML 2.0
    # createMissingAppliedTransforms(flowEnv)
}
