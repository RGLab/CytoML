#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;
INIT_CYTOLIB_ML();
//[[Rcpp::export]]
void gs_to_flowjo(XPtr<GatingSet> gs, string outputfile, bool show_hidden){
	string res;
	try{
		flowjo_xml_node::show_hidden = show_hidden;

		gs_to_flowjo(*gs, outputfile);
	}
	catch(const std::exception &e)
	{
	stop(e.what());
	}
	catch(const char * c)
	{
	stop(c);
	  }
}
