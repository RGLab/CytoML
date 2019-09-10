#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;
INIT_CYTOLIB_ML();
//[[Rcpp::export]]
string workspace_node(XPtr<GatingSet> gs, string outputdir, bool show_hidden){
	string res;
	try{
		flowjo_xml_node::show_hidden = show_hidden;

		auto doc = workspace_node(*gs, outputdir);
		auto node = flowjo_xml_node(*(doc.children().begin()));
		res = node.to_string();
	}
	catch(const std::exception &e)
	{
	stop(e.what());
	}
	catch(const char * c)
	{
	stop(c);
	  }
	return res;
}
