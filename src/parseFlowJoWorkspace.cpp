/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

#include "flowWorkspace/openWorkspace.hpp"
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
CYTOLIB_INIT()


/*
 * constructing GatingSet from xml file
 * _sampleNames should be provided since the additional keys besides sample name may be necessary to uniquely tag each sample
 */
//[[Rcpp::export(name=".cpp_parseWorkspace")]]
XPtr<GatingSet> parseWorkspace(string fileName,vector<string> sampleIDs
                            ,vector<string> sampleNames,bool isParseGate
                            ,unsigned short sampNloc,int xmlParserOption
                            , unsigned short wsType) 
{
		workspace * ws = openWorkspace(fileName, sampNloc,xmlParserOption, wsType);
		GatingSet * gs = ws->ws2gs(sampleIDs,isParseGate,sampleNames);
		delete ws;
		return XPtr<GatingSet>(gs);


}

