/*
 * winFlowJoWorkspace.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef WINFLOWJOWORKSPACE_HPP_
#define WINFLOWJOWORKSPACE_HPP_
#include "flowJoWorkspace.hpp"

namespace CytoML
{
class winFlowJoWorkspace:public flowJoWorkspace{
public:



	  winFlowJoWorkspace(xmlDoc * doc):flowJoWorkspace(doc){
			if(g_loglevel>=GATING_SET_LEVEL)
				COUT<<"windows version of flowJo workspace recognized."<<endl;
	  	//(unfortunately libxml2 doesn't support union query e.g.('*/(Population|NotNode)')
	  	nodePath.popNode="./*/*[name()='Population' or name()='NotNode' or name()='OrNode' or name()='AndNode']";//relative to sampleNode
	  	nodePath.gateDim="*[local-name()='dimension']";//relative to gateNode
	  	nodePath.gateParam="*[local-name()='parameter']";//relative to dimNode


	  }


	  string xPathSample(string sampleID){
	  			string xpath=nodePath.sample;
	  			xpath.append("/DataSet[@sampleID='");
	  			xpath.append(sampleID);
	  			xpath.append("']/..");
	  			return xpath;

	  }
	  /*
	   * choose the trans from global trans vector to attach to current sample
	   */
	  trans_local getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag, const trans_global_vec & gTrans, bool prefixed){

	  	trans_local res;
	  	unsigned sampleID=atoi(root.getProperty("sampleID").c_str());
	  	string sPrefix=comp.prefix;

	  	/*
	  	 *  save trans back to trans_local for each channel
	  	 */

	  	trans_map tp=res.getTransMap();
	  	for(trans_global_vec::const_iterator it=gTrans.begin();it!=gTrans.end();it++)
	  	{
	  		vector<int> sampleVec=it->getSampleIDs();
	  		vector<int>::iterator sampleRes=find(sampleVec.begin(),sampleVec.end(),sampleID);
	  		/*
	  		 * when sampleID matched, then get trans from current global trans group
	  		 */
	  		if(sampleRes!=sampleVec.end())
	  		{


	  			for(PARAM_VEC::iterator isTransIt=transFlag.begin();isTransIt!=transFlag.end();isTransIt++)
	  			{
	  				string curChName=isTransIt->param;
	  				if(isTransIt->log)
	  				{
	  					string curCmpChName = curChName;
	  					//TODO:check the logic here(compare with mac version)
	  					if(prefixed)
	  					    curCmpChName = sPrefix + curCmpChName;//append prefix
	  					trans_map curtp=it->getTransMap();
	  					trans_map::iterator resIt=curtp.find(curCmpChName);
	  					//try generic version if channel-specific is not found
	  					if(resIt==curtp.end())
	  						resIt = curtp.find("*");
	  					shared_ptr<transformation> curTrans;
	  					if(resIt!=curtp.end())
	  						curTrans=resIt->second;
	  					else{
	  						/*
	  						 * use default biexp trans if nothing matched
	  						 */
	  						curTrans.reset(new biexpTrans());

	  					}

	  					tp[curCmpChName]=curTrans;

	  					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
  							COUT<<curCmpChName<<":"<<curTrans->getName()<<" "<<curTrans->getChannel()<<endl;

	  					/*
	  					 * calculate calibration table from the function
	  					 */
	  					if(!curTrans->computed())
	  					{
	  						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	  							COUT<<"computing calibration table..."<<endl;
	  						curTrans->computCalTbl();
	  					}

	  					if(!curTrans->isInterpolated())
	  					{
	  						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	  							COUT<<"spline interpolating..."<<endl;
	  						curTrans->interpolate();
	  					}

	  				}

	  			}

	  			/*
	  			 * assume this sampleID will not appear in any other global trans group
	  			 */
	  			break;
	  		}


	  	}

	  	res.setTransMap(tp);
	  	return res;
	  }

	  /*
	   *parsing transformations from CompensationEditor node and
	   *store in global container within gs
	   *
	   */
	  trans_global_vec getGlobalTrans(){


	  	trans_global_vec res;


	  	/*
	  	 * get CompensationEditor node
	  	 */

	  	string path="/Workspace/CompensationEditor";
	  	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	  	xmlXPathObjectPtr CompEdres = xmlXPathEval((xmlChar *)path.c_str(), context);
	  	if(xmlXPathNodeSetIsEmpty(CompEdres->nodesetval))
	  	{
	  		COUT<<"no CompensationEditor found!"<<endl;
	  		xmlXPathFreeObject(CompEdres);
	  		xmlXPathFreeContext(context);
	  		return(res);
	  	}
	  	wsNode compEdNode(CompEdres->nodesetval->nodeTab[0]);

	  	/*
	  	 * parse all Compensation nodes and store them in global trans container
	  	 */
	  	path="/Workspace/CompensationEditor/Compensation";
	  	wsNode node(doc->children);
	  	xmlXPathObjectPtr compNodeRes =node.xpath(path);
	  	unsigned short nCompNodes=compNodeRes->nodesetval->nodeNr;
	  	if(nCompNodes<=0)
	  	{
	  		COUT<<"compensation not found!"<<endl;
	  		xmlXPathFreeObject(compNodeRes);
	  		return(res);
	  	}
	  	for(unsigned i =0;i<nCompNodes;i++)
	  	{
	  		wsNode compNode(compNodeRes->nodesetval->nodeTab[i]);
	  		string compName=compNode.getProperty("name");

	  		trans_global curTg;
	  		curTg.setGroupName(compName);

	  		if(g_loglevel>=GATING_SET_LEVEL)
	  			COUT<<"group:"<<compName<<endl;
	  		/*
	  		 * parse transformations for current compNode
	  		 */
	  		trans_map curTp=curTg.getTransMap();
	  		path=".//*[local-name()='logicle']";//get logicle(actually it is biexp)
	  		xmlXPathObjectPtr TransRes=compNode.xpathInNode(path);
	  		for(int j=0;j<TransRes->nodesetval->nodeNr;j++)
	  		{
	  			wsNode transNode(TransRes->nodesetval->nodeTab[j]);

	  			string pname=transNode.getProperty("parameter");
	  			/*
	  			 * when channel name is not specified
	  			 * try to parse it as the generic trans that is applied to channels
	  			 * that do not have channel-specific trans defined in workspace
	  			 * ,
	  			 */
	  			if(pname.empty())
	  				pname="*";

	  			string transType=(const char*)transNode.getNodePtr()->name;
	  			if(transType.compare("logicle")==0)
	  			{

	  				if(g_loglevel>=GATING_SET_LEVEL)
	  					COUT<<"logicle func:"<<pname<<endl;
	  				shared_ptr<biexpTrans> curTran (new biexpTrans());
	  				curTran->setName(compName);


	  				curTran->setChannel(pname);
	  				curTran->pos=atof(transNode.getProperty("T").c_str());
	  				curTran->neg=atof(transNode.getProperty("w").c_str());
	  				curTran->widthBasis=atof(transNode.getProperty("m").c_str());
	  				/*
	  				 * do the lazy calibration table calculation and interpolation
	  				 * when it gets saved in gh
	  				 */
	  				curTp[curTran->getChannel()]=curTran;
	  			}
	  			else
	  			{
	  				xmlXPathFreeObject(TransRes);
	  				xmlXPathFreeObject(compNodeRes);
	  				throw(domain_error("unknown tranformation type!" + transType));
	  			}


	  		}
	  		xmlXPathFreeObject(TransRes);
	  		/*
	  		 * parse sample list
	  		 */
	  		path="Samples/Sample";//get logicle(actually it is biexp)
	  		xmlXPathObjectPtr sampleRes=compNode.xpathInNode(path);
	  		unsigned nSample=sampleRes->nodesetval->nodeNr;
	  		vector<int> sampleIDs;
	  		for(unsigned j=0;j<nSample;j++)
	  		{
	  			wsNode curNode(sampleRes->nodesetval->nodeTab[j]);
	  			string curSampleID=curNode.getProperty("sampleID");
	  			sampleIDs.push_back(atoi(curSampleID.c_str()));
	  		}
	  		curTg.setSampleIDs(sampleIDs);
	  		xmlXPathFreeObject(sampleRes);
	  		curTg.setTransMap(curTp);
	  		/*
	  		 * push the tg object to global container
	  		 */
	  		res.push_back(curTg);
	  	}
	  	xmlXPathFreeObject(compNodeRes);


	  	return res;
	  }


	  compensation getCompensation(wsSampleNode sampleNode)
	  {
	  	compensation comp;

	  	xmlXPathObjectPtr res=sampleNode.xpathInNode("*[local-name()='spilloverMatrix']");
	  	if(res->nodesetval->nodeNr > 1)
	  	{
	  		xmlXPathFreeObject(res);
	  		throw(domain_error("not valid compensation node!"));
	  	}
	  	else if(res->nodesetval->nodeNr == 0)
	  	{
	  		 //no comp defined

	  		comp.cid="-2";
	  		comp.prefix="";
	  		comp.suffix="";
	  		comp.comment="none";
	  		comp.name="none";
	  	}
	  	else
	  	{

	  		wsNode node(res->nodesetval->nodeTab[0]);
	  		xmlXPathFreeObject(res);

	  		comp.cid=node.getProperty("id");
	  		comp.prefix=node.getProperty("prefix");
	  		comp.suffix=node.getProperty("suffix");
	  		/*
	  		 * -1:Acquisition-defined,to be computed from data
	  		 * -2:None
	  		 * empty:data is compensated already,spillover matrix can be read from keyword node or empty
	  		 * other:the spillover matrix is stored at special compensation node,
	  		 * 			and this cid serves as id to index that node. in pc version, we observe it is also stored at curent
	  		 * 			sampleNode,to keep the parsing consistent,we still look for it from the special compensation node within the context of xml root
	  		 */
	  		if(comp.cid.compare("-1")==0)
	  		{
	  			comp.comment="Acquisition-defined";
	  			comp.prefix="Comp-";
	  		}
	  		else if(comp.cid.compare("-2")==0)
	  			comp.comment="none";
	  		else if(comp.cid.empty())
	  			throw(domain_error("empty cid not supported yet!"));
	  		else
	  		{
	  			/*
	  			 * directly look for comp from spilloverMatrix node under sampleNode
	  			 */
	  			string path="*[local-name()='spillover']";
	  			xmlXPathObjectPtr resX=node.xpathInNode(path);

	  			/*
	  			 * deprecated:look for comp from global comp node.
	  			 * currently this is done by matching cid,yet it has been proved to be wrong,
	  			 * instead,should look for sampleNode to match sampleID
	  			 */
	  	//		string path="/Workspace/CompensationEditor/Compensation[@name='"+comp.cid+"']/*[local-name()='spilloverMatrix']/*[local-name()='spillover']";
	  	////			COUT<<path<<endl;
	  	//		xmlXPathObjectPtr resX=node.xpath(path);
	  			unsigned nX=resX->nodesetval->nodeNr;
	  			for(unsigned i=0;i<nX;i++)
	  			{
	  				wsNode curMarkerNode_X(resX->nodesetval->nodeTab[i]);
	  				comp.marker.push_back(curMarkerNode_X.getProperty("parameter"));
	  				xmlXPathObjectPtr resY=curMarkerNode_X.xpathInNode("*[local-name()='coefficient']");
	  				unsigned nY=resY->nodesetval->nodeNr;
	  				for(unsigned j=0;j<nY;j++)
	  				{
		  				wsNode curMarkerNode_Y(resY->nodesetval->nodeTab[j]);
		  				// On first pass through, add the detector names as well
	  					if(i == 0)
	  						comp.detector.push_back(curMarkerNode_Y.getProperty("parameter"));
		  				string sValue=curMarkerNode_Y.getProperty("value");
		  				comp.spillOver.push_back(atof(sValue.c_str()));
	  				}
	  				xmlXPathFreeObject(resY);
	  			}
	  			xmlXPathFreeObject(resX);

	  		}
	  	}
	  	return comp;
	  }

	  /*
	   * ellipsoidGate is a specialized ellipGate that is stored as transformed scale and
	   * scaled in 256 * 256 scale
	   */
	  gatePtr getGate(wsEllipseGateNode & node){
	  	/*
	  	 * using the same routine of polygon gate to parse 4 ellipse coordinates
	  	 */
	  	wsPolyGateNode pGNode(node.getNodePtr());
	  	auto pg=dynamic_pointer_cast<polygonGate>(getGate(pGNode, "*[local-name()='edge']/*[local-name()='vertex']"));
	  	vector<coordinate> v=pg->getParam().getVertices();


	  	/*
	  	 * copy four coordinates
	  	 */
	  	if(v.size()!=4)
	  		throw(domain_error("invalid number of antipode pionts of ellipse gate!"));

	  	unique_ptr<ellipsoidGate> gate(new ellipsoidGate(v, pg->getParam().getNameArray()));
	  	//Shift gets lost in ellipsoidGate constructor, so need to add it back in
	  	gate->setShift(getShift(node));
	  	return gatePtr(gate.release());

	  }

	  gatePtr getGate(wsCurlyQuadGateNode & node){
	  	//get intersection point
	  	xmlXPathObjectPtr resPara=node.xpathInNode(nodePath.gateDim);
	  	int nParam=resPara->nodesetval->nodeNr;
	  	if(nParam!=2)
	  		throw(logic_error("invalid number of dimensions for CurlyQuad gate!"));
	  	/*
	  	 * parse the parameters
	  	 */

	  	vector<string> dims;
	  	coordinate intersect;
	  	string quadPattern;
	  	for(int i=0;i<nParam;i++)
	  	{
	  		wsNode curPNode(resPara->nodesetval->nodeTab[i]);

	  		//get parameter name from the children node
	  		string dim;
	  		xmlXPathObjectPtr resPName=curPNode.xpathInNode(nodePath.gateParam);
	  		dim = wsNode(resPName->nodesetval->nodeTab[0]).getProperty("name");
	  		xmlXPathFreeObject(resPName);
	  		dims.push_back(dim);

	  		//get coordinates from properties

	  		string sVal=curPNode.getProperty("min");
	  		if(sVal.empty()){
	  			sVal = curPNode.getProperty("max");
	  			quadPattern.append("-");//note that it is opposite of the population sign
	  			if(sVal.empty())
	  				throw(logic_error("Can't find min or max property in dimension: " + dim));
	  		}else
	  			quadPattern.append("+");
	  		if(i == 0)
	  			intersect.x = boost::lexical_cast<double>(sVal);
	  		else
	  			intersect.y = boost::lexical_cast<double>(sVal);
	  	}
	  	QUAD quad;
	  	if(quadPattern == "-+")
	  		quad = Q1;
	  	else if(quadPattern == "++")
	  		quad = Q2;
	  	else if(quadPattern == "+-")
	  		quad = Q3;
	  	else if(quadPattern == "--")
	  		quad = Q4;
	  	else
	  		throw(domain_error("unrecognized quadPattern: " + quadPattern));
	  	//save it to param of polygonGate
	  	paramPoly pp;
	  	vector<coordinate> vert;
	  	vert.push_back(intersect);
	  	pp.setVertices(vert);
	  	pp.setName(dims);
	  	unique_ptr<CurlyQuadGate> gate(new CurlyQuadGate(pp, quad));
	  	//shift gets lost in CurlyQuadGate constructor, so need to add it back in
	  	gate->setShift(getShift(node));
	  	return gatePtr(gate.release());

	  }

	  /*
	   * The only difference of parsing between polygon and ellipsoid
	   * resides in vertexPath, default is "*[local-name()='vertex']", which is for polygon
	   * "*[local-name()='edge']/*[local-name()='vertex']" is for ellipsoidGate
	   */
	  gatePtr getGate(wsPolyGateNode & node, string vertexPath = "*[local-name()='vertex']"){
	  	/*
	  	 * not sure what is the criteria for using ellipseGate
	  	 * since it is of the same format of polygonGate
	  	 */

	  			unique_ptr<polygonGate> gate(new polygonGate());
	  			//get the negate flag
	  			gate->setNegate(node.getProperty("eventsInside")=="0");
	  			paramPoly p;
	  			vector<coordinate> v;
	  			vector<string> pn;

	  			//TODO:get parameter name(make sure the the order of x,y are correct)
	  			string polyGateDim=nodePath.gateDim+"/"+nodePath.gateParam;
	  			xmlXPathObjectPtr resPara=node.xpathInNode(polyGateDim);
	  			int nParam=resPara->nodesetval->nodeNr;
	  			if(nParam!=2)
	  			{
	  //				COUT<<"the dimension of the polygon gate:"<<nParam<<" is invalid!"<<endl;
	  				throw(domain_error("invalid dimension of the polygon gate!"));
	  			}
	  			for(int i=0;i<nParam;i++)
	  			{
	  				wsNode curPNode(resPara->nodesetval->nodeTab[i]);
	  				string curParam=curPNode.getProperty("name");
	  				pn.push_back(curParam);
	  			}
	  			xmlXPathFreeObject(resPara);

	  			//get vertices
	  			xmlXPathObjectPtr resVert=node.xpathInNode(vertexPath);
	  			for(int i=0;i<resVert->nodesetval->nodeNr;i++)
	  			{
	  				wsNode curVNode(resVert->nodesetval->nodeTab[i]);

	  				/*for each vertice node
	  				**get one pair of coordinates
	  				*/
	  				xmlXPathObjectPtr resCoord=curVNode.xpathInNode("*[local-name()='coordinate']");

	  				xmlNodeSetPtr nodeSet=resCoord->nodesetval;
	  				int nCoord=nodeSet->nodeNr;
	  				if(nCoord!=2)
	  				{
	  					xmlXPathFreeObject(resVert);
	  					xmlXPathFreeObject(resCoord);

	  					throw(domain_error("invalid  number of coordinates!"));
	  				}
	  				//get the coordinates values from the property of respective node
	  				coordinate pCoord;
	  				pCoord.x=atof(wsNode(nodeSet->nodeTab[0]).getProperty("value").c_str());
	  				pCoord.y=atof(wsNode(nodeSet->nodeTab[1]).getProperty("value").c_str());
	  				//and push to the vertices vector of the gate object
	  				v.push_back(pCoord);

	  				xmlXPathFreeObject(resCoord);
	  			}
	  			xmlXPathFreeObject(resVert);

	  			p.setName(pn);
	  			p.setVertices(v);
	  			gate->setParam(p);
	  			gate->setShift(getShift(node));
	  			return gatePtr(gate.release());
	  }

	  gatePtr getGate(wsRectGateNode & node){
		  	  gatePtr thisGate;
	  			//get parameter name
	  			xmlXPathObjectPtr resPara=node.xpathInNode(nodePath.gateDim);
	  			int nParam=resPara->nodesetval->nodeNr;
	  			/*
	  			 * parse the parameters
	  			 */
	  			vector<paramRange> r;
	  			string quad_pattern = "";
	  			for(int i=0;i<nParam;i++)
	  			{
	  				wsNode curPNode(resPara->nodesetval->nodeTab[i]);
	  				paramRange thisR;
	  				//get coordinates from properties
	  				/*
	  				 * be aware that numeric_limits<double>::min() return the minimum positive value
	  				 * instead of negative,so we have to use -max()
	  				 */
	  				string sMin=curPNode.getProperty("min");
	  				if(sMin.empty())
	  				{
	  					thisR.setMin(-numeric_limits<double>::infinity());
						quad_pattern += "-";
	  				}
	  				else
	  					thisR.setMin(atof(sMin.c_str()));

	  				string sMax=curPNode.getProperty("max");
	  				if(sMax.empty())
	  				{
	  					thisR.setMax(numeric_limits<double>::infinity());
						quad_pattern += "+";
	  				}
	  				else
	  					thisR.setMax(atof(sMax.c_str()));

	  				//get parameter name from the children node
	  				xmlXPathObjectPtr resPName=curPNode.xpathInNode(nodePath.gateParam);
	  				thisR.setName(wsNode(resPName->nodesetval->nodeTab[0]).getProperty("name"));
	  				xmlXPathFreeObject(resPName);
	  				r.push_back(thisR);

	  			}
	  			if(nParam==1){
	  				/*
	  				 * parse as rangeGate
	  				 */
	  				unique_ptr<rangeGate> g(new rangeGate());
	  				if(g_loglevel>=GATE_LEVEL)
	  					COUT<<"constructing rangeGate.."<<endl;
	  				//get the negate flag
	  				g->setNegate(node.getProperty("eventsInside")=="0");
	  				g->setParam(r.at(0));
	  				thisGate.reset(g.release());

	  			}else if(nParam==2){

	  				/*
	  				 * convert pRanges to polygon data structure
	  				 */
	  				paramPoly p;
	  				vector<coordinate> v;
	  				vector<string> pn;
	  				pn.push_back(r.at(0).getName());//x
	  				pn.push_back(r.at(1).getName());//y

	  				unique_ptr<polygonGate> g;
	  				auto uid = generate_uid();
	  				if(quad_pattern == "-+")
					{
						g.reset(new quadGate(p, uid, Q1));
						v.push_back({r[0].getMax(), r[1].getMin()});
						if(g_loglevel>=GATE_LEVEL)
							COUT<<"constructing quadGate..Q1"<<endl;
					}
	  				else if(quad_pattern == "++")
					{
						g.reset(new quadGate(p, uid, Q2));
						v.push_back({r[0].getMin(), r[1].getMin()});
						if(g_loglevel>=GATE_LEVEL)
							COUT<<"constructing quadGate..Q2"<<endl;
					}
	  				else if(quad_pattern == "+-")
					{
						g.reset(new quadGate(p, uid, Q3));
						v.push_back({r[0].getMin(), r[1].getMax()});
						if(g_loglevel>=GATE_LEVEL)
							COUT<<"constructing quadGate..Q3"<<endl;
					}
	  				else if(quad_pattern == "--")
					{
						g.reset(new quadGate(p, uid, Q4));
						v.push_back({r[0].getMax(), r[1].getMax()});
						if(g_loglevel>=GATE_LEVEL)
							COUT<<"constructing quadGate..Q4"<<endl;
					}
	  				else
					{
						g.reset(new rectGate());
						if(g_loglevel>=GATE_LEVEL)
							COUT<<"constructing rectGate.."<<endl;
						/*
						 * since rectangle gate in windows version defined differently from mac (simply as 4-point polygon)
						 * so make sure the order of vertices is correct during the conversion to polygon here
						 * lb->lt->rt->rb
						 */
						coordinate lb,lt,rb,rt;//left bottom,left top,right top,right top
						lb.x=r.at(0).getMin();
						lb.y=r.at(1).getMin();

						rt.x=r.at(0).getMax();
						rt.y=r.at(1).getMax();

						v.push_back(lb);
		  //				v.push_back(lt);
						v.push_back(rt);
		  //				v.push_back(rb);
					}

	  				p.setVertices(v);
	  				p.setName(pn);
	  				g->setParam(p);
	  				g->setNegate(node.getProperty("eventsInside")=="0");
					thisGate.reset(g.release());

	  			}else if(nParam!=2)
	  			{
	  				xmlXPathFreeObject(resPara);
	  				throw(domain_error("invalid  dimension of the rectangle gate!"));
	  			}


	  			xmlXPathFreeObject(resPara);


	  			thisGate->setShift(getShift(node));
	  			return thisGate;
	  }
	  gatePtr getGate(wsPopNode & node){

	  	if(node.getName()=="NotNode"){
	  		/*
	  		 * NotNode is a special population node that negate its dependent
	  		 * We parse it as a boolean gate
	  		 */
	  		xmlXPathObjectPtr resPaths=node.xpathInNode("Dependents/Dependent");
	  		if(resPaths->nodesetval->nodeNr != 1)
	  			throw(domain_error("'NotNode' must have one (and only one) 'Dependent'"));
	  		wsNode curGPNode(resPaths->nodesetval->nodeTab[0]);
	  		vector<string> gPaths;
	  		gPaths.push_back(curGPNode.getProperty("name"));
	  		unique_ptr<boolGate> gate(new boolGate());
	  		xmlXPathFreeObject(resPaths);
	  		gate->boolOpSpec = parseBooleanSpec("!G0", gPaths);
			return gatePtr(gate.release());
	  	}else if(node.getName()=="OrNode"||node.getName()=="AndNode"){
	  		/*
	  		 * NotNode is a special population node that negate its dependent
	  		 * We parse it as a boolean gate
	  		 */
	  		xmlXPathObjectPtr resPaths=node.xpathInNode("Dependents/Dependent");
	  		unsigned nDep = resPaths->nodesetval->nodeNr;
	  		if(nDep < 2)
	  			throw(domain_error("'OrNode' must have at least two 'Dependent' nodes"));
	  		vector<string> gPaths;
	  		std::stringstream specs;
	  		for(unsigned i =0; i < nDep; i++){
	  			wsNode curGPNode(resPaths->nodesetval->nodeTab[i]);
	  			gPaths.push_back(curGPNode.getProperty("name"));
	  			string op = node.getName()=="OrNode"?"|":"&";
	  			if(i == 0)
	  				op = "";
	  			specs << op << "G" << i ;
	  		}

	  		unique_ptr<boolGate> gate(new boolGate());
	  		xmlXPathFreeObject(resPaths);
	  		gate->boolOpSpec = parseBooleanSpec(specs.str(), gPaths);
			return gatePtr(gate.release());
	  	}else{


	  		xmlXPathObjectPtr resGate=node.xpathInNode("Gate/*");
	  		if(resGate->nodesetval->nodeNr!=1)
	  			throw(logic_error("invalid 'Gate' node!"));
	  		wsNode gNode(resGate->nodesetval->nodeTab[0]);
	  		xmlXPathFreeObject(resGate);
	  		const xmlChar * gateType=gNode.getNodePtr()->name;
	  		if(xmlStrEqual(gateType,(const xmlChar *)"PolygonGate"))
	  		{
	  			wsPolyGateNode pGNode(gNode.getNodePtr());
	  			if(g_loglevel>=GATE_LEVEL)
	  				COUT<<"parsing PolygonGate.."<<endl;
	  			return(getGate(pGNode));

	  		}
	  		else if(xmlStrEqual(gateType,(const xmlChar *)"RectangleGate"))
	  		{
	  			wsRectGateNode rGNode(gNode.getNodePtr());
	  			if(g_loglevel>=GATE_LEVEL)
	  				COUT<<"parsing RectangleGate.."<<endl;
	  			return(getGate(rGNode));
	  		}
	  		else if(xmlStrEqual(gateType,(const xmlChar *)"EllipsoidGate"))
	  		{
	  			wsEllipseGateNode eGNode(gNode.getNodePtr());
	  			if(g_loglevel>=GATE_LEVEL)
	  				COUT<<"parsing EllipsoidGate.."<<endl;
	  			return(getGate(eGNode));
	  		}
	  		else if(xmlStrEqual(gateType,(const xmlChar *)"CurlyQuad"))
	  		{
	  			wsCurlyQuadGateNode curlyQNode(gNode.getNodePtr());
	  			if(g_loglevel>=GATE_LEVEL)
	  				COUT<<"parsing CurlyQuad.."<<endl;
	  			return(getGate(curlyQNode));
	  		}
	  		else
	  		{
	  	//		COUT<<"gate type: "<<gateType<<" is not supported!"<<endl;
	  			throw(logic_error("invalid  gate type!"));
	  		}
	  	}
	  }

};

class xFlowJoWorkspace:public winFlowJoWorkspace{
public:
	xFlowJoWorkspace(xmlDoc * _doc):winFlowJoWorkspace(_doc){
		if(g_loglevel>=GATING_SET_LEVEL)
			COUT<<"version X"<<endl;
		nodePath.gateParam="*[local-name()='fcs-dimension']";
	}

	trans_global_vec getGlobalTrans(){trans_global_vec res;
										return res;
										};
	trans_local getTransformation(wsRootNode root,const compensation & comp, PARAM_VEC & transFlag, const trans_global_vec & gTrans, bool prefixed){

		trans_local res;

		/*
		 * get transformations node
		 */
		xmlXPathObjectPtr transParentNodeRes=root.xpathInNode("../Transformations");
		unsigned short nTransParentNodes=transParentNodeRes->nodesetval->nodeNr;
		if(nTransParentNodes<=0)
		{
			COUT<<"Transformation not found!"<<endl;
			xmlXPathFreeObject(transParentNodeRes);
			return(res);
		}else if(nTransParentNodes>1){
			throw(domain_error("More than one 'Transformations' node found!"));
		}

		wsNode transParentNode(transParentNodeRes->nodesetval->nodeTab[0]);

		/*
		 * parse each individual transformation
		 */
		xmlXPathObjectPtr transRes=transParentNode.xpathInNode("child::*");

		trans_map curTp=res.getTransMap();

		for(int j=0;j<transRes->nodesetval->nodeNr;j++)
		{
			wsNode transNode(transRes->nodesetval->nodeTab[j]);

			//parse the parameter name
			string pname;
			xmlXPathObjectPtr paramRes=transNode.xpathInNode("*[local-name()='parameter']");
			if(paramRes->nodesetval->nodeNr!=1)
				pname="";
			else{
				wsNode paramNode(paramRes->nodesetval->nodeTab[0]);
				pname=paramNode.getProperty("name");
				}


			xmlXPathFreeObject(paramRes);
			/*
			 * when channel name is not specified
			 * try to parse it as the generic trans that is applied to channels
			 * that do not have channel-specific trans defined in workspace
			 * ,
			 */
			if(pname.empty())
				pname="*";
			if(derived_params.find(pname)!=derived_params.end())
			{
				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"skip parsing transformation for derived parameters:"<<pname<<endl;
				continue;
			}

			string transType=(const char*)transNode.getNodePtr()->name;
		    if(transType.compare("logicle")==0)
			{

					if(g_loglevel>=GATING_SET_LEVEL)
							COUT<<"logicle func:"<<pname<<endl;
					shared_ptr<biexpTrans> curTran(new biexpTrans());

					curTran->setName("");
					curTran->setChannel(pname);
					curTran->pos=atof(transNode.getProperty("M").c_str());
					curTran->neg=atof(transNode.getProperty("A").c_str());
					float w = atof(transNode.getProperty("W").c_str());
					curTran->widthBasis= -pow(10, 2*w);
					curTran->maxValue=atof(transNode.getProperty("T").c_str());
					unsigned short thisLen=atoi(transNode.getProperty("length").c_str());
					curTran->channelRange = thisLen;
//                      if(thisLen!=256)
//                              throw(domain_error("length is not 256 for biex transformation!"));
					/*
					 * do the lazy calibration table calculation and interpolation
					 * when it gets saved in gh
					 */
					curTp[curTran->getChannel()]=curTran;
			}
			else if(transType.compare("biex")==0)
			{

				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"biex func:"<<pname<<endl;
				shared_ptr<biexpTrans> curTran(new biexpTrans());
				curTran->setName("");
				curTran->setChannel(pname);
				curTran->pos=atof(transNode.getProperty("pos").c_str());
				curTran->neg=atof(transNode.getProperty("neg").c_str());
				curTran->widthBasis=atof(transNode.getProperty("width").c_str());
				curTran->maxValue=atof(transNode.getProperty("maxRange").c_str());
				unsigned short thisLen=atoi(transNode.getProperty("length").c_str());
				curTran->channelRange = thisLen;
	//			if(thisLen!=256)
	//				throw(domain_error("length is not 256 for biex transformation!"));
				/*
				 * do the lazy calibration table calculation and interpolation
				 * when it gets saved in gh
				 */
				curTp[curTran->getChannel()]=curTran;
			}else if(transType.compare("linear")==0){
				//only meaningful for scaling ellipsoidGate from 256 back to raw
				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"flin func:"<<pname<<endl;
//				double minRange=atof(transNode.getProperty("minRange").c_str());
				double maxRange=atof(transNode.getProperty("maxRange").c_str());
				if(maxRange==0)
					maxRange = 1;

				shared_ptr<scaleTrans> curTran(new scaleTrans(maxRange,maxRange));

				curTran->setName("");
				curTran->setChannel(pname);

				curTp[curTran->getChannel()]=curTran;

			}else if(transType.compare("log")==0){
				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"flog func:"<<pname<<endl;
				double offset=atof(transNode.getProperty("offset").c_str());
				double decade=atof(transNode.getProperty("decades").c_str());
				// get top scale value from PnR stored in transFlag
				PARAM_VEC::const_iterator pit = findTransFlag(transFlag, pname, comp.prefix, comp.suffix);
				if(pit==transFlag.end())
					throw(domain_error(pname + " does not exist in transFlag vector!"));
				shared_ptr<logTrans> curTran(new logTrans(offset,decade,1,pit->range));

				curTran->setName("");
				curTran->setChannel(pname);

				curTp[curTran->getChannel()]=curTran;
			}else if(transType.compare("fasinh")==0){
				if(g_loglevel>=GATING_SET_LEVEL)
					COUT<<"fasinh func:"<<pname<<endl;
				double length=atof(transNode.getProperty("length").c_str());
				double maxRange=atof(transNode.getProperty("maxRange").c_str());
				double M=atof(transNode.getProperty("M").c_str());
				double T=atof(transNode.getProperty("T").c_str());
				double A=atof(transNode.getProperty("A").c_str());
				shared_ptr<fasinhTrans> curTran(new fasinhTrans(maxRange,length, T,A,M));

				curTran->setName("");
				curTran->setChannel(pname);

				curTp[curTran->getChannel()]=curTran;
			}
			else
				throw(domain_error(pname + ": unknown tranformation type!"+ transType));

		}
		xmlXPathFreeObject(transRes);
		xmlXPathFreeObject(transParentNodeRes);

		res.setTransMap(curTp);

		return res;
	}
	bool is_fix_slash_in_channel_name(){return true;};
};
};
#endif /* WINFLOWJOWORKSPACE_HPP_ */
