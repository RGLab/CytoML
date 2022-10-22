/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
#include <cpp11.hpp>
#include "CytoML/openWorkspace.hpp"
#include "flowWorkspace.h"
using namespace cytolib;
using namespace CytoML;
WS_INIT()

//only needed for win
[[cpp11::register]]
void setLogLevel(int short loglevel) {
  
  g_loglevel = loglevel;
  
}
GatingSet * getGsPtr(SEXP _gsPtr){

	if(R_ExternalPtrAddr(_gsPtr)==0)
			throw(domain_error("Null GatingSet pointer!"));
	cpp11::external_pointer<GatingSet>gs(_gsPtr);

        return gs.get();

}
/*
 * can't use module for exposing overloaded methods
 */


[[cpp11::register]]
cpp11::external_pointer<flowJoWorkspace> open_workspace(std::string filename, int sample_name_location, int xmlParserOption)
{

  unique_ptr<flowJoWorkspace> ws = openWorkspace(filename, static_cast<SAMPLE_NAME_LOCATION>(sample_name_location),xmlParserOption);

  return cpp11::external_pointer<flowJoWorkspace>(ws.release());
}


[[cpp11::register]]
cpp11::external_pointer<GatingSet> parse_workspace(cpp11::external_pointer<flowJoWorkspace> ws
                                  , int group_id
                                  , cpp11::list subset
                                  , bool execute
                                  , std::string path
								  , cpp11::external_pointer<GatingSet> cytoset
                                  , std::string backend_dir
								  , std::string backend
                                  , bool includeGates
                                  , vector<std::string> additional_keys
                                  , bool additional_sampleID
                                  , vector<std::string> keywords
                                  , bool is_pheno_data_from_FCS
                                  , bool keyword_ignore_case
                                  , float extend_val
                                  , float extend_to
                                  , bool channel_ignore_case
                                  , bool leaf_bool
								  , bool include_empty_tree
								  , bool skip_faulty_gate
								  , cpp11::list comps
								  , bool transform
								  , std::string fcs_file_extension
								  , bool greedy_match
								 , SEXP fcs_parse_arg
                                 , int num_threads = 1
)
{
  ParseWorkspaceParameters config;
  //ws parser config
  config.data_dir = path;
  config.cf_dir = backend_dir;
  config.fmt = FileFormat::H5;
  config.is_gating = execute;
  config.is_parse_gate = includeGates;
  config.is_pheno_data_from_FCS = is_pheno_data_from_FCS;
  config.keyword_ignore_case = keyword_ignore_case;
  config.keywords_for_pheno_data = keywords;
  config.keywords_for_uid = additional_keys;
  config.keywords_for_uid_sampleID = additional_sampleID;
  config.gate_extend_trigger_value = extend_val;
  config.gate_extend_to = extend_to;
  config.channel_ignore_case = channel_ignore_case;
  config.compute_leaf_bool_node = leaf_bool;
  config.include_empty_tree = include_empty_tree;
  config.skip_faulty_node = skip_faulty_gate;
  config.fcs_file_extension = fcs_file_extension;
  config.greedy_match = greedy_match;
  config.transform = transform;
  
  SEXP nm = subset.names();
  if(!Rf_isNull(nm))//without NULL checking, the following line will fail
  {
	  vector<std::string> filter_names = cpp11::as_cpp<vector<std::string> >(nm);

	  for(int i = 0; i < filter_names.size(); i++)
	  {
		std::string filter_name = filter_names[i];
		config.sample_filters[filter_name] = cpp11::as_cpp<vector<std::string>>(subset[filter_name]);
	  }
  }
  //fcs parser config
  config.fcs_read_param = sexp_to_fcs_read_param(fcs_parse_arg);
//  config.fcs_read_param.data.num_threads = num_threads;
  config.num_threads = num_threads;
  if(comps.size()==1&&Rf_isNull(comps.names()))
  {
	  if(!Rf_isMatrix(comps[0]))
		cpp11::stop("compensation must be of the type of cpp11::doubles_matrix<>, ");

	  config.global_comp = mat_to_comp(cpp11::as_cpp<cpp11::doubles_matrix<>>(comps[0]));
  }
  else
	  config.compensation_map = list_to_comps(comps);

  unique_ptr<GatingSet> gs = ws->to_GatingSet(group_id, config, *cytoset);
  return cpp11::external_pointer<GatingSet>(gs.release());
}

[[cpp11::register]]
SEXP get_keywords_by_id(cpp11::external_pointer<flowJoWorkspace> ws, int sample_id)
{
  return kw_to_sexp(ws->get_keywords(sample_id).getPairs());
}

[[cpp11::register]]
SEXP get_keywords_by_name(cpp11::external_pointer<flowJoWorkspace> ws, std::string sample_name)
{
  wsSampleNode node = ws->get_sample_node(sample_name);
  return kw_to_sexp(ws->get_keywords(node).getPairs());
}

[[cpp11::register]]
cpp11::list get_sample_groups(cpp11::external_pointer<flowJoWorkspace> ws)
{

  vector<SampleGroup> groups = ws->get_sample_groups();
  int nGroup = groups.size();
  cpp11::writable::integers group_ids(nGroup);
  cpp11::writable::strings group_names(nGroup);
  cpp11::writable::list sample_ids(nGroup);
  for(int i = 0; i < nGroup; i++)
  {
    group_ids[i] = i;
    group_names[i] = groups[i].group_name;
    int nSample = groups[i].sample_ids.size();
    cpp11::writable::integers sample_id_vec(nSample);
    for(int j = 0; j < nSample; j++)
      sample_id_vec[j] = groups[i].sample_ids[j];
    sample_ids[i] = sample_id_vec;
  }

  return cpp11::list({cpp11::named_arg("groupID") = group_ids
                    , cpp11::named_arg("groupName") = group_names
                     , cpp11::named_arg("sampleID") = sample_ids
  });
}

[[cpp11::register]]
cpp11::list get_samples(cpp11::external_pointer<flowJoWorkspace> ws)
{

  vector<SampleGroup> groups = ws->get_sample_groups();
  int nGroup = groups.size();
  cpp11::writable::list grouplist(nGroup);
  ParseWorkspaceParameters config;
  config.include_empty_tree = true;
  for(int i = 0; i < nGroup; i++)
  {
    const vector<SampleInfo> & sample_info_vec = ws->get_sample_info(groups[i].sample_ids, config);
    int nSample = sample_info_vec.size();
    cpp11::writable::list samples(nSample);

    for(int j = 0; j < nSample; j++)
    {
      const SampleInfo & sample_info = sample_info_vec[j];
      samples[j] = cpp11::list({cpp11::named_arg("sampleID") = sample_info.sample_id
                                  , cpp11::named_arg("name") = sample_info.sample_name
                                  , cpp11::named_arg("count") = sample_info.total_event_count
                                  , cpp11::named_arg("pop.counts") = sample_info.population_count
      });

    }

    grouplist[i] = samples;
  }

  return grouplist;
}



[[cpp11::register]]
std::string get_xml_file_path(cpp11::external_pointer<flowJoWorkspace> ws)
{
  return ws->get_xml_file_path();
}


