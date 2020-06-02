/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"
#include "cytolib/trans_group.hpp"
#include <cytolib/H5CytoFrame.hpp>
#include <sstream>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>
#include "search_sample.hpp"
#define TBB_PREVIEW_SERIAL_SUBSET 1

//solve windows build issues
#ifdef Free
#undef Free
#endif
#ifdef Realloc
#undef Realloc
#endif


#include <tbb/tbb.h>
#include "tbb/task_scheduler_init.h"
#include <tbb/spin_mutex.h>
using namespace tbb;
namespace CytoML
{


typedef tbb::spin_mutex GsMutexType;

class flowJoWorkspace:public workspace{
private:
	string versionList;//used for legacy mac ws
	GsMutexType GsMutex, TransMutex, h5Mutex;

public:

	flowJoWorkspace(xmlDoc * doc){

		nodePath.group="/Workspace/Groups/GroupNode";// abs path
		nodePath.sampleRef=".//SampleRefs/SampleRef";//relative GroupNode
		nodePath.sample="/Workspace/SampleList/Sample";//abs path
		nodePath.sampleNode="./SampleNode";//relative to sample

		nodePath.attrName = "name";
		nodePath.compMatName = "name";
		nodePath.compMatChName = "name";
		nodePath.compMatVal = "value";

		this->doc=doc;
		doc_root = wsNode(xmlDocGetRootElement(doc));

	}

	string get_xml_file_path(){
		return filepath;
	}
	/*
	  * Constructor that starts from a particular sampleNode from workspace to build a tree
	  */
	GatingHierarchyPtr to_GatingHierarchy(const wsSampleNode curSampleNode,bool is_parse_gate,const trans_global_vec & _gTrans)
	 {


		GatingHierarchyPtr gh(new GatingHierarchy());
		wsRootNode root=getRoot(curSampleNode);
		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			 COUT<<endl<<"parsing DerivedParameters..."<<endl;
		get_derivedparameters(curSampleNode);

	 	if(is_parse_gate)
	 	{


	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing compensation..."<<endl;
	 		compensation comp=getCompensation(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing trans flags..."<<endl;
	 		PARAM_VEC transFlag=getTransFlag(curSampleNode);

	 		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
	 			COUT<<endl<<"parsing transformation..."<<endl;

	 		trans_local trans, trans_raw;
	 		{
				GsMutexType::scoped_lock lock1(TransMutex);//biexp interpolation could be performed on the shared global trans object

				//prefixed version
				trans = getTransformation(root,comp,transFlag,_gTrans, true);

				/*
				 * unprefixed version. Both version of trans are added (sometime they are identical)
				 * so that the trans defined on uncompensated channel (e.g. SSC-A) can still be valid
				 * without being necessarily adding comp prefix.
				 * It is mainly to patch the legacy workspace of mac or win where the implicit trans is added for channel
				 * when its 'log' keyword is 1.
				 * vX doesn't have this issue since trans for each parameter/channel
				 * is explicitly defined in transform node.
				 */


				 trans_raw=getTransformation(root,comp,transFlag,_gTrans, false);
	 		}
	 		//merge raw version of trans map to theprefixed version
	 		trans_map tp = trans_raw.getTransMap();
	 		for(trans_map::iterator it=tp.begin();it!=tp.end();it++)
	 		{
	 			trans.addTrans(it->first, it->second);
	 		}
	 		gh.reset(new GatingHierarchy(comp, transFlag, trans));

	 	}
	 	if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"parsing DerivedParameters..."<<endl;
	 	get_derivedparameters(curSampleNode);

	 	if(g_loglevel>=POPULATION_LEVEL)
	 		COUT<<endl<<"parsing populations..."<<endl;


	 	/*
		  * set root node first before recursively add the other nodes
		  * since root node does not have gates as the others do
		  */
	 	populationTree &tree = gh->getTree();
	 	VertexID pVerID=0;
	 	nodeProperties & np = tree[0];
	 	to_popNode(root, np);
	 	addPopulation(tree, pVerID,&root,is_parse_gate);

	 	return gh;

	 }

	unique_ptr<GatingSet> to_GatingSet(unsigned group_id, ParseWorkspaceParameters config, GatingSet & cytoset)
	{
		if(config.is_gating)
		{
			if(!config.transform)
				COUT << "'transform = false' is ignored when 'is_gating' is set to true!" << endl;
		}
		auto sg = get_sample_groups();

		if(group_id >= sg.size()){
			 throw(domain_error("Invalid group id: " + to_string(group_id) + "\n"));
		}

		vector<SampleInfo> sample_infos = get_sample_info(sg[group_id].sample_ids, config);

		auto it_filter = config.sample_filters.find("name");
		if(it_filter != config.sample_filters.end())
			config.sample_filters.erase(it_filter);								// remove name filter from filters

		unique_ptr<GatingSet> gsPtr(new GatingSet());
		 /*
		  * parsing global calibration tables
		  */
		 trans_global_vec gTrans;
		 if(config.is_parse_gate)
		 {
			 if(g_loglevel>=GATING_SET_LEVEL)
				 COUT<<"... start parsing global transformations... "<<endl;
			 gTrans=getGlobalTrans();

		 }
		 if(cytoset.size()>0)
		 {
			 if(cytoset.begin()->second->get_cytoframe_view_ref().get_h5_file_path()=="")
				throw(domain_error("In-memory cytoset is not supported!"));
		 }

		 string data_dir = config.data_dir;
		 fs::path h5_dir = fs::path(config.h5_dir);
		 if(config.is_gating)
		 {
			 if(!config.is_h5)
			 {
				 if(cytoset.size()>0)
				 {
					 config.is_h5 = true;
					 PRINT("GatingSet is set to disk-based backend because cytoset is supplied as data input! \n");
				 }
			 }

			if(data_dir == "")
			{
				//use xml dir
				string xml_filepath = get_xml_file_path();
				data_dir = path_dir_name(xml_filepath);
				data_dir = data_dir.empty() ? "." : data_dir; 
			}
			h5_dir = fs::path(gsPtr->generate_h5_folder(h5_dir.string()));
		 }

		/*
		 * try to parse each sample
		 */
		GatingSet & gs = *gsPtr;
		tbb::task_scheduler_init init(config.num_threads);


		if(config.num_threads <=1)
			tbb::serial::parallel_for<int>(0, sample_infos.size(), 1, [&, this](int i){
				this->parse_sample(sample_infos[i], config, data_dir, h5_dir, gTrans, gs, cytoset);
			});
		else
			tbb::parallel_for<int>(0, sample_infos.size(), 1, [&, this](int i){
							this->parse_sample(sample_infos[i], config, data_dir, h5_dir, gTrans, gs, cytoset);
						});
		if(gsPtr->size() == 0)
			throw(domain_error("No samples in this workspace to parse!"));
		//keep the cs in sync with backend file
		if(cytoset.size()>0)
		{
			for(auto i : cytoset)
			{
				//reload to ensure the old view is purged
				auto uri = i.second->get_cytoframe_view_ref().get_h5_file_path();
				i.second->set_cytoframe_view(CytoFrameView(CytoFramePtr(new H5CytoFrame(uri))));
			}

		}

		return gsPtr;
	}
	void parse_sample(const SampleInfo &sample_info
			, const ParseWorkspaceParameters & config_const
			, const string &data_dir, const fs::path &h5_dir
			, const trans_global_vec&gTrans_const
			, GatingSet &gs
			, GatingSet & cytoset)
	{

		if(g_loglevel>=GATING_HIERARCHY_LEVEL)
			COUT<<endl<<"... start parsing sample: "<< sample_info.sample_name <<"... "<<endl;
		FlowJoSampleSearch fjsearch;
		//generate uid
		string ws_key_seq = fjsearch.concatenate_keywords(sample_info.keywords
				, config_const.keywords_for_uid, config_const.keywords_for_uid_sampleID, sample_info.sample_id);
		string uid = sample_info.sample_name + ws_key_seq;
		shared_ptr<MemCytoFrame> frptr;
		string h5_filename;

		if(config_const.is_gating)
		{
			//match FCS

			// Note sample_info.total_event_count comes from root node population and so may not agree with $TOT key value for the sample
			auto tot_it = sample_info.keywords.find("$TOT");
			if(tot_it == sample_info.keywords.end())
				throw(domain_error("$TOT keyword not found in workspace for sample " + sample_info.sample_name));
			int total_event_count = stoi(tot_it->second);

			auto cfg = config_const;
			cfg.fcs_read_param.header.is_fix_slash_in_channel_name = is_fix_slash_in_channel_name();

			if(cytoset.size() > 0)//search cytoset
				frptr = fjsearch.search_for_data<GatingSet, GS_Item>(cytoset
						,sample_info.sample_id, sample_info.sample_name, total_event_count, ws_key_seq
						, cfg, h5_filename);
			else
			{//search fcs directory
				vector<string> all_file_paths = list_files(data_dir, config_const.fcs_file_extension);

				frptr = fjsearch.search_for_data<vector<string>, string>(all_file_paths
						,sample_info.sample_id, sample_info.sample_name, total_event_count, ws_key_seq
						, cfg, h5_filename);

			}
			if(!frptr){
			  PRINT("FCS not found for sample " + uid + " from searching the file extension: " + config_const.fcs_file_extension + "\n");
			}

		}
		else
			frptr.reset(new MemCytoFrame());//add dummy frame to hold pData




		//proceed gate parsing when data is available or gate-parsing only
		if(frptr || !config_const.is_gating)
		{

			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
				cout<<endl<<"Extracting pheno data from keywords for sample: " + uid<<endl;


			//set pdata
			frptr->set_pheno_data("name", sample_info.sample_name);

			KEY_WORDS keys = sample_info.keywords;
			if(!config_const.is_gating&&config_const.is_pheno_data_from_FCS)
				throw(domain_error("Can't parse phenodata from FCS when 'is_gating' is set to false!"));

			if(config_const.is_pheno_data_from_FCS)
				keys = frptr->get_keywords();
			else
				keys = sample_info.keywords;

			for(const string & k : config_const.keywords_for_pheno_data)
			{
				//todo::case insensitive search
				if(config_const.keyword_ignore_case)
					throw(domain_error("keyword_ignore_case not supported yet!"));
				auto it_k = keys.find(k);
				if(it_k == keys.end())
					throw(domain_error("keyword '" + k + "' not found in sample: " + uid));

				frptr->set_pheno_data(it_k->first, it_k->second);
			}

			//filter sample by FCS keyword
			if(config_const.is_pheno_data_from_FCS && !check_sample_filter(config_const.sample_filters, frptr->get_pheno_data()))//skip parsing this sample if not passed filter
			{
				if(g_loglevel>=GATING_HIERARCHY_LEVEL)

				cout<<endl<<"Skipping sample: " + uid + " by filter"<<endl;

				return;
			}


			//parse gating tree
			GatingHierarchyPtr gh = to_GatingHierarchy(sample_info.sample_node,config_const.is_parse_gate,gTrans_const);

			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
				COUT<<"Gating hierarchy created: "<<uid<<endl;
			if(config_const.is_gating)
			{
				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					cout<<endl<<"Gating ..."<<endl;
				if(h5_filename=="")
				{
					//load the data into the header-only version of cytoframe
					frptr->read_fcs_data();

				}
				MemCytoFrame & fr = *frptr;
				//check if external comps are provided
				compensation comp;
				if(!config_const.global_comp.empty())
				{
					comp = config_const.global_comp;
				}
				else
				{
					auto it_comp = config_const.compensation_map.find(uid);
					if(it_comp != config_const.compensation_map.end())
					{
						comp = it_comp->second;

					}
				}
				if(!comp.empty())
				{
					comp.cid = "1";
					gh->set_compensation(comp, false);
				}

				gh->compensate(fr);
				gh->transform_gate();
				// add the scaleTrans for the implicit time transformation based on $TIMESTEP in FCS
				// This reproduces some of the logic of cytolib::CytoFrame::scale_time_channel
				string time_channel = "time";
				auto idx = frptr->get_col_idx(time_channel, ColType::channel);
				if(idx >= 0){
					EVENT_DATA_TYPE timestep = frptr->get_time_step(time_channel);
					if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					PRINT("Adding transformation to multiply "+time_channel+" by :"+ to_string(timestep) + "\n");

					shared_ptr<scaleTrans> timeTrans(new scaleTrans(timestep));

					timeTrans->setGateOnlyFlag(false);
					timeTrans->setName(time_channel);
					timeTrans->setChannel(time_channel);
					timeTrans->setType(SCALE);

					trans_local lTrans = gh->getLocalTrans();
					lTrans.addTrans(time_channel, timeTrans);
					gh->addTransMap(lTrans.getTransMap());
				}
				gh->shift_gate();
				gh->transform_data(fr);
				gh->extendGate(fr, config_const.gate_extend_trigger_value);
				gh->gating(fr, 0,false, config_const.compute_leaf_bool_node, config_const.skip_faulty_node);


			}
			else
			{
				if(config_const.transform)
				{
					gh->transform_gate();
					gh->shift_gate();
					gh->extendGate(config_const.gate_extend_trigger_value, config_const.gate_extend_to);
				}


			}


			if(config_const.is_gating&&config_const.is_h5)
			{

				{
					GsMutexType::scoped_lock lock(h5Mutex);
					if(h5_filename=="")//write to new h5 file when it is loaded from fcs
					{
						h5_filename = (h5_dir/uid).string() + ".h5";
					}

					frptr->write_h5(h5_filename);
					gh->set_cytoframe_view(CytoFrameView(CytoFramePtr(new H5CytoFrame(h5_filename, false))));
				}
			}
			else
			  gh->set_cytoframe_view(CytoFrameView(frptr));

			{
			  GsMutexType::scoped_lock lock(GsMutex);
			  if(gs.find(uid) != gs.end()){
			    throw(domain_error("Duplicated GUIDs detected within group: " + uid
                            + "\n Consider adding additional keywords to the GUID with argument \"additional.keys\""
                            + "or setting argument \"additional.sampleID = TRUE\" to disambiguate samples further."));
			  }
			  gs.add_GatingHierarchy(gh, uid, false);
			}

		}



	 }



	vector<wsGroupNode> get_group_nodes()
	{
		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		//parse group info
		xmlXPathObjectPtr grp_result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
		xmlXPathFreeContext(context);

		if(xmlXPathNodeSetIsEmpty(grp_result->nodesetval)){
			xmlXPathFreeObject(grp_result);
			 throw(domain_error("No Groups infomation!"));
		}
		int nGroup = grp_result->nodesetval->nodeNr;
		vector<wsGroupNode> res(nGroup);
		for(int i = 0; i < nGroup; i++)
		{
			xmlNodePtr cur=grp_result->nodesetval->nodeTab[i];
			res[i] = wsGroupNode(cur);
		}
		xmlXPathFreeObject(grp_result);

		return res;
	}
	/**
	 * It includes samples with 0 populations
	 * @return
	 */
	vector<SampleGroup> get_sample_groups()
	{

		vector<SampleGroup> res;
		vector<wsGroupNode> nodes = get_group_nodes();
		unsigned nGroup = nodes.size();

		for(unsigned i = 0; i < nGroup; i++)
		{
			SampleGroup sg;
			wsGroupNode grp_node = nodes[i];
			//parse sample ref node
			xmlXPathObjectPtr sids=grp_node.xpathInNode(nodePath.sampleRef.c_str());
			xmlNodeSetPtr nodeSet=sids->nodesetval;
			int nSample = nodeSet->nodeNr;
			//filter out empty groups to be consistent with historical behavior
			if(nSample > 0)
			{
				sg.group_name = nodes[i].getProperty(nodePath.attrName.c_str());
				if(g_loglevel>=GATING_SET_LEVEL)
						cout<<endl<<"Parsing " + to_string(nSample) + " sample refs for group: " + sg.group_name <<endl;

				sg.sample_ids.resize(nSample);
				for(int j = 0; j < nSample; j++)
				{
					wsNode cur_sample_ref_node(nodeSet->nodeTab[j]);
					string sampleID=cur_sample_ref_node.getProperty("sampleID");
					sg.sample_ids[j] = boost::lexical_cast<int>(sampleID);
				}
				res.push_back(sg);
			}
			xmlXPathFreeObject(sids);

		}

		return res;

	}
	/*
	 * * It excludes samples with 0 populations
	 */
	vector<SampleInfo> get_sample_info(const vector<int> & sample_ids, const ParseWorkspaceParameters & config)
	{

		vector<SampleInfo> sample_info_vec;
		for(int j = 0; j < sample_ids.size(); j++)
		{
//			if(g_loglevel>=GATING_SET_LEVEL)
//				cout<<endl<<"Search sample for sampleID: " + sampleID <<endl;
			//parse sample node
			int sample_id = sample_ids[j];
			auto sample_vec = get_sample_node(sample_id);
			if(sample_vec.size() > 0)
			{
				if(sample_vec.size() > 1)
					throw(domain_error("multiple sample nodes matched to the sample id: " + to_string(sample_id) + "!"));

				if(g_loglevel>=GATING_HIERARCHY_LEVEL)
					cout<<endl<<"Parsing sample information for sampleID: " + to_string(sample_id) <<endl;
				SampleInfo sample_info;
				sample_info.sample_node = sample_vec[0];
				sample_info.sample_id = sample_id;
				sample_info.population_count = get_population_count(sample_info.sample_node);

				// filter sample by pop.count
				if(sample_info.population_count == 0&&!config.include_empty_tree)
					continue;

				// Note in rare cases this may not agree with $TOT key value for the sample
				sample_info.total_event_count = get_event_count(getRoot(sample_info.sample_node));
				//cache sample_name for the same reason
				sample_info.sample_name = get_sample_name(sample_info.sample_node);
				//filter by name
				unordered_map<string, vector<string>> sample_filter = config.sample_filters;
				auto it_filter = sample_filter.find("name");



				if(it_filter != sample_filter.end())
				{

					if(g_loglevel>=GATING_SET_LEVEL)
						cout<<endl<<"filter by sample name: " + sample_info.sample_name <<endl;
					bool pass_name_filter = check_sample_filter<PDATA>(*it_filter,{{"name",sample_info.sample_name}});
					// remove name filter from filters
					sample_filter.erase(it_filter);
					if(!pass_name_filter)
						continue;

				}

				//pre-parse and cache the keywords from ws to avoid parsing it multiple times later
				sample_info.keywords = get_keywords(sample_info.sample_node);
				//filter by other pheno data
				if(!config.is_pheno_data_from_FCS)
				{
					if(g_loglevel>=GATING_SET_LEVEL)
						cout<<endl<<"filter by workspace keyword: " <<endl;
					if(!check_sample_filter<KEY_WORDS>(sample_filter, sample_info.keywords))
						continue;
				}
				//save the sample info that passed the filter
				sample_info_vec.push_back(sample_info);

			}
		}
		return sample_info_vec;
	}

	template<typename T>
	bool check_sample_filter(const pair<string, vector<string>> & sample_filter, const T & pheno_data)
	{

		//apply each filter
		string filter_name = sample_filter.first;
		const auto & it_pdata = pheno_data.find(filter_name);
		if(it_pdata == pheno_data.end())
			throw(domain_error("filter '" + filter_name + "' not found in sample pheno data: "));

		const vector<string> & filter_values = sample_filter.second;
		return find(filter_values.begin(), filter_values.end(), it_pdata->second) != filter_values.end();

	}

	template<typename T>
	bool check_sample_filter(const unordered_map<string, vector<string>> & sample_filter, const T & pheno_data)
	{
		for(auto & it_filter : sample_filter)
		{
			if(!check_sample_filter(it_filter, pheno_data))
				return false;
		}
		return true;
	}
	KEY_WORDS get_keywords(int sample_id)
	{
		auto sample_vec = get_sample_node(sample_id);
		if(sample_vec.size() == 0)
			throw(domain_error("sample id: " + to_string(sample_id) + " not found!"));
		if(sample_vec.size() > 1)
			throw(domain_error("multiple sample nodes matched to the sample id: " + to_string(sample_id) + "!"));

		return get_keywords(sample_vec[0]);
	}
	KEY_WORDS get_keywords(wsSampleNode & node)
	{
		KEY_WORDS keys;
		xmlXPathObjectPtr res=node.xpathInNode("Keywords/Keyword");
		xmlNodeSetPtr nodeSet=res->nodesetval;
		for(int j = 0; j < nodeSet->nodeNr; j++)
		{
			wsNode key_node(nodeSet->nodeTab[j]);
			string val = key_node.getProperty("value");
			boost::trim(val);
			keys[key_node.getProperty("name")] = val;
		}
		return keys;
	}

	vector<wsSampleNode> get_sample_node(int sample_id){
		string s_sample_id = to_string(sample_id);
		string path = xPathSample(s_sample_id);
		xmlXPathObjectPtr res = doc_root.xpath(path, false);

		vector<wsSampleNode> node_vec;
		if(res&&res->nodesetval)
		{
			int nSample = res->nodesetval->nodeNr;
			node_vec.resize(nSample);

			for(int i = 0; i < nSample; i++)
				node_vec[i] = wsSampleNode(res->nodesetval->nodeTab[i]);
		}
		xmlXPathFreeObject(res);
		return node_vec;
	}
	/**
	 * Search for sample node by name
	 * Currently it is used by Rcpp API
	 * @param sample_name
	 * @return
	 */
	wsSampleNode get_sample_node(string sample_name)
	{
		xmlXPathObjectPtr res;
		switch(nodePath.sample_name_location)
		{
			case SAMPLE_NAME_LOCATION::KEY_WORD:
			{
				res = doc_root.xpath(nodePath.sample + "/Keywords/Keyword[@name='$FIL' and @value='" + sample_name + "']/../..");

				break;

			}
			case SAMPLE_NAME_LOCATION::SAMPLE_NODE:
			{
				res = doc_root.xpath(nodePath.sample + "/SampleNode[@name='" + sample_name + "']/..");
				break;
			}

			default:
				throw(domain_error("unknown sampleName Location!It should be either 'keyword' or 'sampleNode'."));
		}
		if(res->nodesetval->nodeNr == 0)
		{
			xmlXPathFreeObject(res);
			throw(domain_error("sample not found: " + sample_name));
		}
		if(res->nodesetval->nodeNr > 1)
		{
			xmlXPathFreeObject(res);
			throw(domain_error("Multiple sample nodes found for : " + sample_name));
		}
		wsSampleNode node(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		return node;
	}
	/*
	 * .
	 * By default sampleName is fetched from keywords/$FIL
	 *  However the $FIL may not be consistent with the sampleNode "name" attribute
	 *   and the physical FCS filename,so optionally we allow parsing it
	 *   from name attribute from SampleNode
	 *
	 */
	string get_sample_name(wsSampleNode & node){
		string filename;
		switch(nodePath.sample_name_location)
		{
			case SAMPLE_NAME_LOCATION::KEY_WORD:
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
			case SAMPLE_NAME_LOCATION::SAMPLE_NODE:
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
		//fs:: make_preferred not working yet, have to manually handle windows path
		boost::replace_all(filename, "\\", "/");
		return path_base_name(filename);
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

			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
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
			if(g_loglevel>=GATING_HIERARCHY_LEVEL)
						COUT<<endl;

			res.push_back(curParam);
		}
		return res;
	}


	int get_event_count(const wsRootNode & node)
	{

		return atoi(node.getProperty("count").c_str());
	}

	int get_population_count(const wsSampleNode & node)
	{
		//libxml doesn't seem to support count() function in xpath
		xmlXPathObjectPtr res = node.xpathInNode(".//Population");
		int n = res->nodesetval->nodeNr;
		xmlXPathFreeObject(res);
		return n;
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
		fjStats["count"]= get_event_count(node);
		np.setStats(fjStats,false);


	}

	void to_popNode(wsPopNode &node,nodeProperties & np,bool is_parse_gate=false){



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
			if(is_parse_gate)np.setGate(getGate(node));
		}
		catch (logic_error & e) {

			throw(logic_error("extracting gate failed:" + np.getName() + "--" + e.what()));
		}


	}

	//used for legacy mac ws
	void parseVersionList(){
		wsNode root(this->doc->children);
		xmlXPathObjectPtr res = root.xpath("/Workspace");
		wsNode curNode(res->nodesetval->nodeTab[0]);
		xmlXPathFreeObject(res);
		this->versionList=curNode.getProperty("versionList");

	}
	/*
	 * get the minimum initial digit from the version list string
	 * //used for legacy mac ws
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

	vector<EVENT_DATA_TYPE> getShift(wsNode & node) const{
		//get any necessary shifts (as in the case of magnetic gates)
		string adjustX_str = node.getProperty("adjustX");
		string adjustY_str = node.getProperty("adjustY");
		EVENT_DATA_TYPE adjustX = adjustX_str.empty() ? 0.0 : atof(adjustX_str.c_str());
		//adjustY appears to be negated but not sure where this is documented
		EVENT_DATA_TYPE adjustY = adjustY_str.empty() ? 0.0 : -atof(adjustY_str.c_str());
		return vector<EVENT_DATA_TYPE>{adjustX, adjustY};
	}
};



};

#endif /* FLOWJOWORKSPACE_HPP_ */
