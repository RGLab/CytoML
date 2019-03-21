/*
 * test_header.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef TEST_HEADER_HPP_
#define TEST_HEADER_HPP_


#include <iostream>
#include <fstream>
#include <string>
#include <boost/algorithm/string.hpp>
#include "flowWorkspace/openWorkspace.hpp"
#include "flowWorkspace/flowJoWorkspace.hpp"
#include "cytolib/GatingSet.hpp"
#include "cytolib/GatingHierarchy.hpp"
#include "cytolib/transformation.hpp"
#include "cytolib/spline.hpp"
#include "ncdfFlow.hpp"
using namespace std;


struct testCase{
	string filename; //xml file name
	unsigned short wsType; //workspace type
	string colfile; // text file that records the compensated channel names
	string ncfile; // raw data stored in hdf format
	map<string,string> samples; // fcs file name vs sampleID
	unsigned short sampNloc; // the location where the sample name to be parsed
	string archive; // archived gating set dat file
	vector<bool> isEqual; // the bool vector records the counts discrepancy (using cv) between flowJo and flowCore
	float tolerance; // the threshold for cv value
	bool isParseGate; //whether to parse gate from xml
	int xmlParserOption;//xml parser option passed down to libxml2
	bool isTemplate;// whether test the template copying feature
	bool isLoadArhive;// whether to load archived gs
	bool isSaveArchive;
	unsigned archiveFormat;
	bool archiveType;// boost or google
	map<string,float> gains;
	vector<VertexID> skipPops;
//	vector<double> times;//global variable to collect run time

} ;
hdfFlow gs_attachCDF(GatingSet & gs,testCase myTest);
void gs_gating(GatingSet &gs,string curSample, hdfFlow nc);
void gh_counts(GatingHierarchy* gh,vector<bool> &isEqual, const float tolerance);
void clone_test(testCase myTest);
//void gs_parse(testCase,unsigned short,bool,bool);
void parser_test(testCase &);
void ncdf_test();
void compCalTbl();
void spline_test();
void cpConsTest();
#endif /* TEST_HEADER_HPP_ */
