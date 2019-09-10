#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;
INIT_CYTOLIB_ML();
//[[Rcpp::export]]
string append_sample_node(XPtr<GatingSet> gs, string sn, int sampleId, bool show_hidden){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
	flowjo_xml_node::sample_name = sn;
	flowjo_xml_node::gh = gs->getGatingHierarchy(sn);
	flowjo_xml_node::show_hidden = show_hidden;
	flowjo_xml_node::derived_params = DERIVED_PARAM();//must reset it since it is static and persistent across invocations
	try{
	  flowjo_xml_node res = node.append_sample_node(sampleId);
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

//[[Rcpp::export]]
string append_keywords_node(){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
//	flowjo_xml_node::gh = gs->getGatingHierarchy(sn);
//	flowjo_xml_node::show_hidden = show_hidden;
//	flowjo_xml_node::derived_params = DERIVED_PARAM();//must reset it since it is static and persistent across invocations
	try{
	  flowjo_xml_node res = node.append_keywords_node();
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
//[[Rcpp::export]]
string append_derived_params_node(string outputdir){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
//	flowjo_xml_node::gh = gs->getGatingHierarchy(sn);
//	flowjo_xml_node::show_hidden = show_hidden;
//	flowjo_xml_node::derived_params = DERIVED_PARAM();//must reset it since it is static and persistent across invocations
	try{
	  flowjo_xml_node res = node.append_derived_params_node(outputdir);
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
//[[Rcpp::export]]
string append_transformation_node(){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	ws_node.set_name("dummy");
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point
//	flowjo_xml_node::gh = gs->getGatingHierarchy(sn);
//	flowjo_xml_node::show_hidden = show_hidden;
//	flowjo_xml_node::derived_params = DERIVED_PARAM();//must reset it since it is static and persistent across invocations
	try{
	  flowjo_xml_node res = node.append_transformation_node();
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

