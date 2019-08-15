#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;
INIT_CYTOLIB_ML();
//[[Rcpp::export]]
string append_subpopulation_node(XPtr<GatingSet> gs, string sn, vector<string> pops, bool show_hidden){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
	flowjo_xml_node::gh = gs->getGatingHierarchy(sn);
	flowjo_xml_node::show_hidden = show_hidden;
	try{
	  flowjo_xml_node res = node.append_subpopulation_node(pops);
	}
	catch(const std::exception &e)
	{
	stop(e.what());
	}
	catch(const char * c)
	{
	stop(c);
	  }
	return node.to_string();
}

