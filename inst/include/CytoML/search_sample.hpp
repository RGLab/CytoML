/*
 * search_sample.hpp
 *
 *  Created on: May 28, 2020
 *      Author: wjiang2
 */

#ifndef INCLUDE_CYTOML_SEARCH_SAMPLE_HPP_
#define INCLUDE_CYTOML_SEARCH_SAMPLE_HPP_

#include <cytolib/H5CytoFrame.hpp>
namespace CytoML
{
	struct SampleInfo{
		int sample_id;
		string sample_name;
		int total_event_count;
		int population_count;
		compensation comp;
		wsSampleNode sample_node;
		KEY_WORDS keywords;
	};
	struct SampleGroup{
		string group_name;
		vector<int> sample_ids;
	};
	struct ParseWorkspaceParameters
	{
		 bool is_gating = true;; // whether to load FCS data and perform gating
		 bool is_parse_gate = true;; // whether to parse the gates, can be turned off for parsing pop stats only
		 bool is_pheno_data_from_FCS = false;; // whether to extract keywords for pdata from FCS or workspace
		 vector<string> keywords_for_pheno_data = {}; // the keywords to be extracted as pdata for cytoframe
		 vector<string> keywords_for_uid= {"$TOT"};  // keywords to be used along with sample name for constructing sample uid
		 bool keywords_for_uid_sampleID = false;
		 bool keyword_ignore_case = false;; // whether to ignore letter case when extract keywords for pdata (can be turned off when the letter case are not consistent across samples)
		 bool channel_ignore_case = false;; //  whether the colnames(channel names) matching needs to be case sensitive (e.g. compensation, gating..)
		 float gate_extend_trigger_value = 0; //the threshold that determine wether the gates need to be extended. default is 0. It is triggered when gate coordinates are below this value.
		 float gate_extend_to = -4000;// the value that gate coordinates are extended to. Default is -4000. Usually this value will be automatically detected according to the real data range.
		 unordered_map<string, vector<string>> sample_filters;
		 string data_dir = ""; //path for FCS directory
		 bool is_h5 = true;;
		 bool compute_leaf_bool_node = true;
		 bool include_empty_tree = false;
		 bool skip_faulty_node = false;
		 string h5_dir = fs::temp_directory_path().string();// output path for generating the h5 files
		 FCS_READ_PARAM fcs_read_param;
		 unordered_map<string, compensation> compensation_map;//optional customized sample-specific compensations
		 compensation global_comp;
		 string fcs_file_extension = ".fcs";
		 bool greedy_match = false;
		 bool transform = true;
		 int num_threads = 1;
	};
	typedef pair<string, GatingHierarchyPtr> GS_Item;

	/*
	 * This class wrap some sample matching related logics
	 * i.e. match the FCS or cytoset to the flowJo samples
	 * by name, total events, keywords
	 */
	class FlowJoSampleSearch{


		public:

			/**
			 * Generate the uniquely identifiable id for each sample
			 * by concatenating sample name with some other keywords and
			 * the sampleID if desired
			 * @param node
			 * @param keywords_for_uid
			 * @param keywords_for_uid_sampleID Whether sampleID should be included in guid
			 * @param sample_id The sampleID to be used if keywords_for_uid_sampleID is true
			 * @return
			 */
			string concatenate_keywords(const KEY_WORDS & keywords, const vector<string> & keywords_for_uid
					, bool keywords_for_uid_sampleID, int sample_id)
			{
				string uid = "";
				for(const string & key : keywords_for_uid)
				{
					auto it = keywords.find(key);
					if(it == keywords.end())
						throw(domain_error("Keyword not found in workspace: " + key + " for sample " + uid));
					uid += "_" + it->second;
				}
				uid = keywords_for_uid_sampleID ? ("_" + std::to_string(sample_id) + uid) : uid;
				return uid;
			}



			/**
			 *
			 * @param src either vector of string represent fcs paths or a GatingSet(i.e. cytoset)
			 * @param sample_name sample name referred by flowjo to be matched to
			 * @param total_event_count total event from flowjo  to be matched to
			 * @param ws_key_seq keywords from flowjo  to be matched to
			 * @param config
			 * @param backend_uri the backend file path that helps caller to determine whether the
			 * 			returned cytoframe is from fcs or from the existing cytoframe
			 * @return MemCytoFrame that matches to flowJo sample
			 */
			template <typename Container, typename Element>
				shared_ptr<MemCytoFrame> search_for_data(Container & src
						, const int sample_id, const string & sample_name
					, const int & total_event_count, const string & ws_key_seq
					, const ParseWorkspaceParameters & config, string & backend_uri)
			{
				FCS_READ_PARAM fcs_read_param = config.fcs_read_param;
				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					COUT<<endl<<"Searching for FCS for sample: " + sample_name <<endl;
				//try to search by file name first
				struct Match{
					Element item;
					bool total_events;
					bool keys;
					Match(Element v1, bool v2, bool v3):item(v1),total_events(v2), keys(v3){};
				};
				vector<Match> matches;
				CytoFrameView cv;
				bool isfound = false;

				// Gather files that match sample_name
				for(auto i : src){
					if(get_sample_name(i)== sample_name){
						cv = load_cytoframeview(i, fcs_read_param, backend_uri);

						// Check if total # events matches
						auto match_total = (stoi(cv.get_keyword("$TOT")) == total_event_count);

						// Check if keywords match
						string fcs_key_seq = concatenate_keywords(cv.get_keywords(), config.keywords_for_uid, config.keywords_for_uid_sampleID, sample_id);
						auto match_keys = (fcs_key_seq == ws_key_seq);

						matches.push_back(Match(i, match_total, match_keys));

						// If greedy_match is set, jump out at first good match so
						// sample still parses even with duplicates in directory or subdirectories
						if(match_total && config.greedy_match)
							break;
					}
				}

				// No matches found from simple filename check. Check $FIL keyword for prospective matches
				if(matches.size() == 0){
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT << "No FCS file found with filename matching sample name " << sample_name << ". Checking for files with $FIL keyword matching sample name." << endl;
					for(auto i : src){
						cv = load_cytoframeview(i, fcs_read_param, backend_uri);

						// Just gather those with $FIL matching sample name
						if(cv.get_keyword("$FIL") == sample_name){

							// Check if total # events matches
							auto match_total = (stoi(cv.get_keyword("$TOT")) == total_event_count);

							// Check if keywords match
							string fcs_key_seq = concatenate_keywords(cv.get_keywords()
																		, config.keywords_for_uid
																		, config.keywords_for_uid_sampleID
																		, sample_id);
							auto match_keys = (fcs_key_seq == ws_key_seq);

							matches.push_back(Match(i, match_total, match_keys));

							// If greedy_match is set, jump out at first good match so
							// sample still parses even with duplicates in directory or subdirectories
							if(match_total && config.greedy_match)
								break;
						}
					}
				}

				// If there are multiple matching files from prior gathering steps
				// attempt to resolve ambiguity by first trying $TOT, then sampleID+keywords
				switch(matches.size()){
				case 0: // No filename match
				{
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT << "No FCS file found for sample name " << sample_name << "." << endl;
					break;
				}
				case 1: // Unambiguous match
				{
					if(!matches[0].total_events)
						PRINT("FCS file found for sample " + sample_name + " has incorrect total number of events. Sample will be excluded.\n");
					else{
						cv = load_cytoframeview(matches[0].item, fcs_read_param, backend_uri);
						isfound = true;
					}
					break;
				}
				default: // Ambiguous match. Need to check $TOT and maybe keywords
				{
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT << "Multiple FCS files found for sample " << sample_name << ". Attempting to use total number of events to resolve ambiguity." << endl;
					switch(count_if(matches.begin(), matches.end(), [](const Match & v){return v.total_events;})){ // number that match $TOT
					case 0: // None matching total number of events
					{
						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
							PRINT("No FCS files found for sample " + sample_name + " have correct total number of events. Sample will be excluded.\n");
						break;
					}
					case 1:
					{
					  int match_final = distance(matches.begin(), find_if(matches.begin(), matches.end(), [](const Match & v){return v.total_events;}));
					  	cv = load_cytoframeview(matches[match_final].item, fcs_read_param, backend_uri);
						isfound = true;
						break;
					}
					default: // Ambiguity remains. Check keywords + sampleID and clip out those that do not match
					{
						if(g_loglevel>=GATING_HIERARCHY_LEVEL)
							COUT << "Multiple FCS files remain for sample " << sample_name << ". Attempting to use additional keywords and sampleID to resolve ambiguity." << endl;
						switch(count_if(matches.begin(), matches.end(), [](const Match & v){return v.total_events&&v.keys;})){ // number that match $TOT AND sampleID + key sequence
						case 0:
						{
							if(g_loglevel>=GATING_HIERARCHY_LEVEL)
								PRINT("No FCS files for sample " + sample_name + " match specified keywords. Sample will be excluded.\n");
							break;
						}
						case 1:
						{
							int match_final = distance(matches.begin(), find_if(matches.begin(), matches.end(), [](const Match & v){return v.total_events&&v.keys;}));
							cv = load_cytoframeview(matches[match_final].item, fcs_read_param, backend_uri);
							isfound = true;
							break;
						}
						default: // Still ambiguous. Report the multiple possibilities.
						{
							string candidates;
							for (auto i : matches)
							  if(i.total_events&&i.keys)
								candidates += get_sample_info(i.item) + "\n";
							throw(domain_error("Multiple FCS files match sample " + sample_name + " by filename, event count, and keywords.\n"
											   "Candidates are: \n" + candidates +
											   "Please move incorrect files out of this directory or its subdirectories.\n"));
						}
						}
					}
					}
				}
				}
				if(isfound)
				{
					backend_uri = cv.get_h5_file_path();
					if(backend_uri=="")
					{//return the pointer loaded from fcs as it is
						return dynamic_pointer_cast<MemCytoFrame>(cv.get_cytoframe_ptr());

					}
					else
					{
						//ensure view is realized before return the data that is loaded from cytoset
						if(cv.is_row_indexed()||cv.is_col_indexed())
						{
							cv = cv.copy_realized(backend_uri, true);
						}
						return shared_ptr<MemCytoFrame>(new MemCytoFrame(*cv.get_cytoframe_ptr()));
					}

				}
				else
					return shared_ptr<MemCytoFrame>();

			}

			/*
			 * overloaded functions to dispatch to either cs iterator
			 * or file paths iterator
			 */

			//get sample name from matched gs iterator
			string get_sample_name(GS_Item it){return it.first;}
			//get file path from matched fcs
			string get_sample_name(string path){return path_base_name(path);}
			//get sample name from matched gs iterator
			string get_sample_info(GS_Item it){return it.first;}
			//get file path from matched fcs
			string get_sample_info(string path){return path;}

			//convert from the matched cf to  memory cytoframe
			CytoFrameView load_cytoframeview(GS_Item it
												, const FCS_READ_PARAM & fcs_read_param
												, string & backend_uri){
				auto cv = it.second->get_cytoframe_view();
				backend_uri = cv.get_h5_file_path();
				if(backend_uri=="")
					throw(domain_error("In-memory cytoset is not supported!"));

				return cv;
			}

			//load the matched fcs into memory cytoframe
			CytoFrameView load_cytoframeview(string path
												, const FCS_READ_PARAM & fcs_read_param
												, string & backend_uri){
				shared_ptr<MemCytoFrame> fr(new MemCytoFrame(path, fcs_read_param));
				fr->read_fcs_header();
				return CytoFrameView(fr);
			}

	};
}


#endif /* INCLUDE_CYTOML_SEARCH_SAMPLE_HPP_ */
