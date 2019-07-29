#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;
/*
 * copied from flowWorkspace, eventually not needed once more R code is wrapped into c
 */
vector<BOOL_GATE_OP> boolFilter_R_to_C(List filter){


			/*
			 * get specification from R
			 */
			StringVec refs=as<StringVec>(filter["refs"]);
			StringVec op=as<StringVec>(filter["op"]);
			BoolVec isNot=as<BoolVec>(filter["isNot"]);

			/*
			 * convert to c class
			 */
			vector<BOOL_GATE_OP> res;
			for(unsigned i=0;i<refs.size();i++)
			{

				BOOL_GATE_OP gOpObj;

				boost::split(gOpObj.path,refs.at(i),boost::is_any_of("/"));
				if(gOpObj.path.at(0).empty())
					gOpObj.path.erase(gOpObj.path.begin());//remove the first empty string

				gOpObj.isNot=isNot.at(i);
				gOpObj.op=boost::iequals(op.at(i),"|")?'|':'&';

				res.push_back(gOpObj);
			}
			return (res);
}
//* copied from flowWorkspace, eventually not needed once more R code is wrapped into c
gate * newGate(List filter){

	StringVec names=filter.names();

	unsigned short gateType=as<unsigned short>(filter["type"]);

	bool isNeg=as<bool>(filter["negated"]);
	gate * g;

	switch(gateType)
	{
		case RANGEGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			rangeGate * rg=new rangeGate();
			rg->setNegate(isNeg);

			DoubleVec p=as<DoubleVec>(filter["range"]);

			paramRange pRange;
			pRange.setName(params.at(0));
			pRange.setMin(p.at(0));
			pRange.setMax(p.at(1));

			rg->setParam(pRange);

			g=rg;

			break;
		}
		case POLYGONGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			polygonGate * pg=new polygonGate();

			pg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			NumericMatrix boundaries=as<NumericMatrix>(filter["boundaries"]);
			for(int i=0;i<boundaries.nrow();i++)
			{
				coordinate pCoord;
				pCoord.x=boundaries(i,0);
				pCoord.y=boundaries(i,1);
				v.push_back(pCoord);

			}
			pp.setVertices(v);

			pg->setParam(pp);

			g=pg;

			break;
		}
		case RECTGATE:
		{
			StringVec params=as<StringVec>(filter["params"]);
			rectGate * rectg=new rectGate();

			rectg->setNegate(isNeg);

			paramPoly pp;
			pp.setName(params);

			vector<coordinate> v;
			NumericMatrix boundaries=as<NumericMatrix>(filter["boundaries"]);
			for(int i=0;i<boundaries.nrow();i++)
			{
				coordinate pCoord;
				pCoord.x=boundaries(i,0);
				pCoord.y=boundaries(i,1);
				v.push_back(pCoord);

			}
			pp.setVertices(v);

			rectg->setParam(pp);

			g=rectg;
			break;

		}
		case BOOLGATE:
		{
			boolGate * bg=new boolGate();

			bg->setNegate(isNeg);
			bg->boolOpSpec = boolFilter_R_to_C(filter);
			g=bg;
			break;

		}
		case LOGICALGATE:
		{
			logicalGate * lg = new logicalGate();
			lg->setNegate(isNeg);
			g = lg;
			break;
		}
		case CLUSTERGATE:
		{
			clusterGate * cg = new clusterGate(as<string>(filter["cluster_method_name"]));
			cg->setNegate(isNeg);
			g = cg;
			break;
		}
		case ELLIPSEGATE:
		{



			//parse the mean
			DoubleVec mean=as<DoubleVec>(filter["mu"]);
			coordinate mu(mean.at(0), mean.at(1));
			double dist = as<double>(filter["dist"]);

			//parse cov mat
			vector<coordinate> cov;
			NumericMatrix covMat=as<NumericMatrix>(filter["cov"]);
			for(int i=0;i<covMat.nrow();i++)
			{
				coordinate p;
				p.x=covMat(i,0);
				p.y=covMat(i,1);
				cov.push_back(p);

			}

			ellipseGate * eg = new ellipseGate(mu, cov,dist);
			eg->setNegate(isNeg);

			// parse the parameter names
			StringVec params=as<StringVec>(filter["params"]);
			paramPoly pp;
			pp.setName(params);
			eg->setParam(pp);

			g=eg;

			break;
		}
		default:
			throw(domain_error("unsupported gate type!valid types: POLYGONGATE(1),RANGEGATE(2),BOOLGATE(3),RECTGATE(5),LOGICALGATE(6)"));

	}
	g->setTransformed(TRUE);
	return g;

}

//[[Rcpp::export]]
string graph_node(vector<string> params){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point

	return node.append_graph_node(params).to_string();
}

//[[Rcpp::export]]
List bool_node(List bool_gate, string pop, int count, vector<string> not_node_vec
					, vector<string> params, string subNode){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");//avoid R xml parsing warning about the default ':anonymous' node name

	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
	unordered_set<string> not_node_set;
	for(auto n : not_node_vec)
	  not_node_set.insert(n);
	// Rcout << not_node.size()<< endl;
	unique_ptr<boolGate> g(dynamic_cast<boolGate*>(newGate(bool_gate)));
//	Rcout << "start gate_node"<< endl;
  try{
	  flowjo_xml_node gate_node = node.generate_bool_pop(*g, pop, count, not_node_set, params, subNode);
  }
  catch(const std::exception &e)
  {
    stop(e.what());
  }
  catch(const char * c)
  {
    stop(c);
  }
	// Rcout << not_node.size()<< endl;
	//update NotNode_set
	not_node_vec.clear();
	// Rcout << NotNode_set.size()<< endl;
	for(auto n : not_node_set)
	  not_node_vec.push_back(n);
	// Rcout << NotNode_set.size()<< endl;
	return List::create(node.to_string(), not_node_vec);
	 

}
