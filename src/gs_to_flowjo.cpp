#include "cytolib-ml/flowjo_xml_node.hpp"
#include "flowWorkspace.h"
using namespace Rcpp;
using namespace cytolib;

//[[Rcpp::export]]
string graph_node(vector<string> params){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point

	return node.append_graph_node(params).to_string();
}

//[[Rcpp::export]]
string bool_node(List bool_gate, string pop, int count, CharacterVector NotNode_set, subNode){
	xml_document doc;//create empty doc
	xml_node ws_node = doc.append_child();//add first dummy node
	flowjo_xml_node node(ws_node);//wrap into flowjo node as the starting point

	return node.generate_bool_pop(params).to_string();

}
