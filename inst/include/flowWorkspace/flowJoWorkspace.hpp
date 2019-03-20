/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"
#include "cytolib/transformation.hpp"
#include <sstream>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>

class flowJoWorkspace:public workspace{
private:
	string versionList;
public:

	flowJoWorkspace(xmlDoc * doc){

		nodePath.group="/Workspace/Groups/GroupNode";// abs path
		nodePath.sampleRef=".//SampleRef";//relative GroupNode
		nodePath.sample="/Workspace/SampleList/Sample";//abs path
		nodePath.sampleNode="./SampleNode";//relative to sample

		nodePath.attrName = "name";
		nodePath.compMatName = "name";
		nodePath.compMatChName = "name";
		nodePath.compMatVal = "value";

		this->doc=doc;

	}

	/*get a vector of sampleID by the given groupID
	 * keep the returned results in char * format in case the non-numeric sampleID is stored
	 * make sure to free the memory of xmlChar * outside of the call
	 *
	 * used by GatingSet::parseWorkspace(unsigned short groupID,bool isParseGate)
	 */
	vector<string> getSampleID(unsigned short groupID)
	{

			xmlXPathContextPtr context = xmlXPathNewContext(doc);
			xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
			if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
				xmlXPathFreeObject(result);
				xmlXPathFreeContext(context);
	//	                COUT<<"No Groups"<<endl;;
		         throw(domain_error("No Groups infomation!"));
			}

			if(groupID== 0||groupID>=result->nodesetval->nodeNr)
			{
				xmlXPathFreeObject(result);
				xmlXPathFreeContext(context);
				 throw(invalid_argument("invalid GroupID provided!"));
			}
			xmlNodePtr cur=result->nodesetval->nodeTab[groupID];
			context->node=cur;
			xmlXPathObjectPtr sids=xmlXPathEval((xmlChar *)nodePath.sampleRef.c_str(),context);
			vector<string> sampleID;
			xmlNodeSetPtr nodeSet=sids->nodesetval;
			int size=nodeSet->nodeNr;

			for(int i=0;i<size;i++)
			{
				xmlNodePtr curNode=nodeSet->nodeTab[i];
				xmlChar * curSampleID= xmlGetProp(curNode,(xmlChar *)"sampleID");
				//to avoid memory leaking,store a copy of returned characters in string vector so that the dynamically allocated memory
				//can be freed right away in stead of later on.
				string sSampleID=(const char *)curSampleID;
				sampleID.push_back(sSampleID.c_str());
				xmlFree(curSampleID);
			}
	//			;

			xmlXPathFreeObject (result);
			xmlXPathFreeContext(context);
			xmlXPathFreeObject (sids);
			return(sampleID);
	}



	/*
	 * .
	 * By default sampleName is fetched from keywords/$FIL
	 *  However the $FIL may not be consistent with the sampleNode "name" attribute
	 *   and the physical FCS filename,so optionally we allow parsing it
	 *   from name attribute from SampleNode
	 *
	 */
	string getSampleName(wsSampleNode & node){
		string filename;
		switch(nodePath.sampNloc)
		{
			case 1:
			{
				xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword[@name='$FIL']");
				if(res->nodesetval->nodeNr!=1){
					xmlXPathFreeObject(res);
					throw(domain_error("$FIL keyword not found!"));
				}

				wsNode kwNode(res->nodesetval->nodeTab[0]);
				xmlXPathFreeObject(res);
				filename=kwNode.getProperty("value");
				break;
			}
			case 2:
			{
				xmlXPathObjectPtr res=node.xpathInNode("SampleNode");//get sampleNode
				wsNode sampleNode(res->nodesetval->nodeTab[0]);
				xmlXPathFreeObject(res);

				filename=sampleNode.getProperty(nodePath.attrName);//get property name from sampleNode
				break;
			}

			default:
				throw(domain_error("unknown sampleName Location!It should be either 'keyword' or 'sampleNode'."));
		}

		if(filename.empty())
			throw(domain_error("$FIL value is empty!"));
		return filename;
	}

	/*
	 * get transformation for one particular sample node
	 */



	//xquery the "SampleNode" within "sample" context
	wsRootNode getRoot(wsSampleNode sample)
	{
	//	COUT<<"parsing root node"<<endl;
		xmlXPathObjectPtr res=sample.xpathInNode(nodePath.sampleNode);
		wsRootNode node(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
	//	COUT<<nodePath.sampleNode<<endl;
		return node;
	}


	wsPopNodeSet getSubPop(wsNode * node)
	{

		xmlXPathObjectPtr res=node->xpathInNode(nodePath.popNode);
		int nChildren=res->nodesetval->nodeNr;
	//	wsPopNodeSet childenNodes(res->nodesetval->nodeTab,nChildren);
		wsPopNodeSet childenNodes;
		for(int i=0;i<nChildren;i++)
		{
			childenNodes.push_back(wsPopNode(res->nodesetval->nodeTab[i]));
		}

		xmlXPathFreeObject(res);

		return childenNodes;

	}

	/*
	 * this is for windows version currently because windows version does not have parameter nodes,
	 * Not sure whether to get "Range" info for windows though
	 *
	 *
	 */
	PARAM_VEC getTransFlag(wsSampleNode sampleNode){
		PARAM_VEC res;

		/*
		 * get total number of channels
		 */
		string path="Keywords/*[@name='$PAR']";
		xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
		wsNode parNode(parRes->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(parRes);
		unsigned short nPar=atoi(parNode.getProperty("value").c_str());

		/*
		 * get info about whether channel should be transformed
		 */

		for(unsigned i=1;i<=nPar;i++)
		{
			PARAM curParam;

			/*
			 * get curernt param name
			 */
			stringstream ss(stringstream::in | stringstream::out);
			ss << "Keywords/*[@name='$P"<< i<<"N']";
			path=ss.str();
			xmlXPathObjectPtr parN=sampleNode.xpathInNode(path);
			wsNode curPNode(parN->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parN);
			string pName=curPNode.getProperty("value");

			/*
			 * get current display flag
			 */
			stringstream ss1(stringstream::in | stringstream::out);
			ss1 << "Keywords/*[@name='P"<<i<<"DISPLAY']";
			path=ss1.str();
			xmlXPathObjectPtr parDisplay=sampleNode.xpathInNode(path);
			wsNode curDisplayNode(parDisplay->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parDisplay);
			string curFlag=curDisplayNode.getProperty("value");

			/**
			 * get PnR (mainly used to init T value in flog)
			 */
			stringstream ss2(stringstream::in | stringstream::out);
			ss2 << "Keywords/*[@name='$P"<<i<<"R']";
			path=ss2.str();
			xmlXPathObjectPtr parR=sampleNode.xpathInNode(path);
			wsNode curRNode(parR->nodesetval->nodeTab[0]);
			xmlXPathFreeObject(parR);
			string curR=curRNode.getProperty("value");
			/*
			 * check if data is already stored in log scale
			 * so that pnR may need to be calculated by PnE
			 */
			float f1 = 0, f2 = 0;//init with default values in case PnE is missing
			stringstream ss3(stringstream::in | stringstream::out);
			ss3 << "Keywords/*[@name='$P"<<i<<"E']";
			path=ss3.str();
			xmlXPathObjectPtr parE=sampleNode.xpathInNode(path);
			if(parE->nodesetval->nodeNr > 0)
			{
				wsNode curENode(parE->nodesetval->nodeTab[0]);

				string curE=curENode.getProperty("value");
				vector<string> tokens;
				boost::split(tokens, curE, boost::is_any_of(","));
				f1 = stof(tokens[0]);
				f2 = stof(tokens[1]);
				if(f1 > 0 && f2 == 0)//correct f2 for legacy FCS 2.0
					f2 = 1;
			}
			xmlXPathFreeObject(parE);
			if(f1 > 0)
				curParam.range = pow(10, f1) * f2;
			else
				curParam.range=atoi(curR.c_str());
			curParam.param=pName;
			curParam.log=curFlag.compare("LOG")==0;


			if(g_loglevel>=GATING_SET_LEVEL)
				COUT<<pName<<":"<<curFlag;
			/*
			 * We can't determine '$TIMESTEP' soly from workspace since the this keyword value in xml is not as reliable as the one in FCS TEXT
			 */
	//		if(pName.compare("Time") == 0||pName.compare("time") == 0){
	//			path="Keywords/*[@name='$TIMESTEP']";
	//			xmlXPathObjectPtr parRes=sampleNode.xpathInNode(path);
	//			wsNode parNode(parRes->nodesetval->nodeTab[0]);
	//			xmlXPathFreeObject(parRes);
	//			string sTimestep = parNode.getProperty("value");
	//			curParam.timestep=strtod(sTimestep.c_str(), NULL);
	//			if(g_loglevel>=GATING_SET_LEVEL)
	//				COUT<<sTimestep;
	//		}
			if(g_loglevel>=GATING_SET_LEVEL)
						COUT<<endl;

			res.push_back(curParam);
		}
		return res;
	}
	/*
	 *Note: nodeProperties is dynamically allocated and up to caller to free it
	 */
	void to_popNode(wsRootNode & node, nodeProperties & np){



		/*
		 * in order to make the pop names comparable accross samples
		 * force the root node name as "root" (it was stored as fcs filenames in some fj ws)
		 */

		np.setName("root");

		POPSTATS fjStats;
		fjStats["count"]=atoi(node.getProperty("count").c_str());
		np.setStats(fjStats,false);


	}

	void to_popNode(wsPopNode &node,nodeProperties & np,bool isParseGate=false){



		//add pop name
		np.setName(node.getProperty(nodePath.attrName).c_str());

		if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"parse the population Node:"<<np.getName()<<endl;
		//add pop counts
		POPSTATS fjStats;
		string sCount = node.getProperty("count");
		//set the empty stats to -1
		fjStats["count"] = sCount.empty()?-1:atoi(sCount.c_str());
		np.setStats(fjStats,false);

		try
		{
			if(isParseGate)np.setGate(getGate(node));
		}
		catch (logic_error & e) {

			throw(logic_error("extracting gate failed:" + np.getName() + "--" + e.what()));
		}


	}

	void parseVersionList(){
		wsNode root(this->doc->children);
		xmlXPathObjectPtr res = root.xpath("/Workspace");
		wsNode curNode(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		this->versionList=curNode.getProperty("versionList");

	}
	/*
	 * get the minimum initial digit from the version list string
	 */
	unsigned short getVersionMin(){
		int res=numeric_limits<int>::max();
		vector<string> vlist;
		boost::split(vlist,versionList,boost::is_any_of(";"));
		for(vector<string>::iterator it=vlist.begin();it!=vlist.end();it++)
		{
			string curVer=*it;
			boost::erase_all(curVer,"Pre");
			vector<string> digits;
			boost::split(digits,curVer,boost::is_any_of("."));
			curVer=digits.at(0).c_str();
			boost::algorithm::trim((curVer));
			if(!curVer.empty())
			{
				int toCompare=boost::lexical_cast<int>(curVer);
				res=min(res,toCompare);
			}

		}
		return res;
	}

	vector<BOOL_GATE_OP> parseBooleanSpec(string specs,vector<string> gPaths){

		vector<BOOL_GATE_OP> res;

		/*
		 * parse the spec strings to get logical operations among populations
		 */

		boost::replace_all(specs," ","");

		//tokenize by boolean operator: & or |

		boost::char_separator<char> sep("", "&|"); // first arg specify dropped seperator and secoond for the kept separators
		boost::tokenizer<boost::char_separator<char>> tokens(specs, sep);
		unsigned short i = 0;
		vector<string> popTokens, opTokens;

		for(string thisToken : tokens)
		{

			if(i%2 == 0)
				popTokens.push_back(thisToken);//like G0, G1...
			else
				opTokens.push_back(thisToken);//operators: |, &
			i++;

		}
		unsigned short nPopulations=popTokens.size();
		if(nPopulations!=gPaths.size())
		{
			cout << specs << endl;
			for(auto p:gPaths)
				cout << p << " ";
			cout << endl;
			for(auto p:popTokens)
						cout << p << " ";
			cout << endl;
			throw(domain_error("the logical operators and the gating paths do not pair correctly!"));
		}


		for(unsigned j=0;j<nPopulations;j++)
		{

			BOOL_GATE_OP gOpObj;


			string curPopToken=popTokens.at(j);
			string curOpToken;
			if(j==0)
				curOpToken="&";//assign and operation to the first token and actually it is not used
			else
				curOpToken=opTokens.at(j-1);
			/*
			 * extract number from token
			 */
			string sIndex=boost::erase_all_copy(curPopToken,"!");
			boost::erase_all(sIndex,"G");
			unsigned short index = boost::lexical_cast<unsigned short>(sIndex);
			/*
			 * select respective gating path string and split it into vector
			 */
			string curPath=gPaths.at(index);
			boost::split(gOpObj.path,curPath,boost::is_any_of("/"));
			/*
			 * the path could either one of the two types:
			 * 1) /pop1
			 * 2) /pop1/pop2
			 * the first case indicate it is a single reference node name,which requires nearest ancestor search
			 * the second is a full path or partial path of the nodes,requires a path matching
			 * EDIT:turns out the second case is also partial path.
			 */
			if(gOpObj.path.at(0).empty())
				gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string


			// parse negate operator
			gOpObj.isNot=curPopToken.find("!")!=string::npos;


			/*
			 * if not |,we assume it as & by skipping the actual matching with "&"
			 * since it stores as &amp; in xml
			 */
			gOpObj.op=boost::iequals(curOpToken,"|")?'|':'&';

			/*
			 * push the parsed gating path vector and operator into result vector
			 */
			res.push_back(gOpObj);

		}
		return res;

	}
};





#endif /* FLOWJOWORKSPACE_HPP_ */
