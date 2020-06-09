#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE Suites
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <chrono>
#include "test_header.hpp"

//CYTOLIB_INIT()

WS_INIT()

//unsigned short myTestPolymorphism(){
//	gate * g= NULL;
//
//	rectGate rectg =rectGate();
//	g=&rectg;
//
//	rectGate * newG = dynamic_cast<rectGate*>(g);
//	return newG->getType();
//
//}
//BOOST_AUTO_TEST_SUITE(Polymorph)
//BOOST_AUTO_TEST_CASE(gateCastDown)
//{
//	BOOST_CHECK(myTestPolymorphism() == RECTGATE);
//	BOOST_CHECK(myTestPolymorphism() != POLYGONGATE);
//
//}
//BOOST_AUTO_TEST_SUITE_END()
//
//BOOST_AUTO_TEST_SUITE(RegExp)
//BOOST_AUTO_TEST_CASE(flowChannelMatch)
//{
//	string strPattern = "[FS]SC-[AWH]";
//	boost::regex ex(strPattern);
//
//	BOOST_CHECK(boost::regex_match( "FSC",ex) == false);
//	BOOST_CHECK(boost::regex_match( "FSC-A",ex) == true);
//	BOOST_CHECK(boost::regex_match( "FSC-AD",ex) == false);
//}
//
//BOOST_AUTO_TEST_SUITE_END()
struct globalFixture{
	globalFixture(){

	};
	~globalFixture(){};

};
BOOST_GLOBAL_FIXTURE(globalFixture);
struct parseWorkspaceFixture{
	parseWorkspaceFixture(): argc(boost::unit_test::framework::master_test_suite().argc),
	           argv(boost::unit_test::framework::master_test_suite().argv)
	{
		/*
		 * parse argv
		 */
		map<string, string> arg_map;
		for(int i = 1; i < argc; i++){
			string thisArg = argv[i];
			vector<string> strSplit;
			boost::split(strSplit,thisArg, boost::is_any_of("="));
			if(strSplit.size() != 2)
				throw(domain_error("invalid arguments!"));
			else{
				string argName = strSplit.at(0);
				boost::replace_first(argName, "--", "");
				string argValue = strSplit.at(1);
				arg_map[argName] = argValue;
			}
		}
		map<string, string>::iterator it;

//		it = arg_map.find("archiveType");
//		myTest.archiveType = it==arg_map.end()?true:it->second == "PB";

		it = arg_map.find("isLoadArchive");

		myTest.isLoadArhive = it==arg_map.end()?false:boost::lexical_cast<bool>(it->second);

		it = arg_map.find("isSaveArchive");
		myTest.isSaveArchive = it==arg_map.end()?false:boost::lexical_cast<bool>(it->second);

		it = arg_map.find("g_loglevel");
		g_loglevel = it==arg_map.end()?false:boost::lexical_cast<unsigned>(it->second);

		it = arg_map.find("backend");
		if(it==arg_map.end()||it->second=="tile")
		{
			myTest.config.fmt = FileFormat::TILE;
		}
		else
			myTest.config.fmt = FileFormat::H5;

		it = arg_map.find("num_threads");
		myTest.config.num_threads = it==arg_map.end()?1:boost::lexical_cast<unsigned>(it->second);


	};



	~parseWorkspaceFixture(){

	};
   int argc;
   char **argv;
	testCase myTest;

};

BOOST_FIXTURE_TEST_SUITE(parseWorkspace,parseWorkspaceFixture)

BOOST_AUTO_TEST_CASE(redefined_error)
{
	auto ws = openWorkspace("../wsTestSuite/attr_redefined_err.xml", SAMPLE_NAME_LOCATION::SAMPLE_NODE, 1);
}

BOOST_AUTO_TEST_CASE(duplicatedSampleID)
{
	//test mem leaks
	for(auto i : {1,2,3,4,5})
		auto ws = openWorkspace("../wsTestSuite/duplicatedSampleID/Ustekin_G26_sas_IMMPORT2.495809.xml", myTest.sample_name_location,myTest.xmlParserOption);
//	ws.reset();
}
BOOST_AUTO_TEST_CASE(nogate)
{
	myTest.filename="../wsTestSuite/no-gate.wsp";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]={"CytoTrol_CytoTrol_1.fcs"};
	myTest.config.include_empty_tree = true;
	myTest.config.data_dir = "../wsTestSuite/Cytotrol/NHLBI/Tcell";
	myTest.config.keywords_for_uid={};
	myTest.group_id = 0;
	myTest.archive="../output/no-gate/gs";
//	g_loglevel = GATE_LEVEL;

	parser_test(myTest);

//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(ManuallyIncludedSamples)
{

	myTest.filename="../wsTestSuite/ManuallyIncludedSamples.wsp";
	myTest.config.is_gating = false;
	unique_ptr<flowJoWorkspace> ws = openWorkspace(myTest.filename, myTest.sample_name_location,myTest.xmlParserOption);
	unique_ptr<GatingSet> gs = ws->to_GatingSet(2, myTest.config);
	BOOST_CHECK_EQUAL(gs->size(), 10);

}
BOOST_AUTO_TEST_CASE(flog)
{
	myTest.filename="../wsTestSuite/flog/log.wsp";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]={"CytoTrol_CytoTrol_1.fcs"};
	myTest.config.data_dir = "../wsTestSuite/Cytotrol/NHLBI/Tcell";
	myTest.config.keywords_for_uid={};
	myTest.group_id = 0;
	myTest.archive="../output/flog/gs";
//	g_loglevel = GATE_LEVEL;

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(flog_PnE_space)
{

	myTest.filename="../wsTestSuite/flog PnE/Liver.wsp";
	myTest.sample_name_location = SAMPLE_NAME_LOCATION::SAMPLE_NODE;
	myTest.config.sample_filters["name"]={"Tissues_Liver_001.fcs"};

	myTest.config.fcs_read_param.data.which_lines = {1000};
	unique_ptr<flowJoWorkspace> ws = openWorkspace(myTest.filename, myTest.sample_name_location,myTest.xmlParserOption);
	unique_ptr<GatingSet> gs = ws->to_GatingSet(0, myTest.config);
	BOOST_CHECK_EQUAL(gs->begin()->second->get_cytoframe_view().n_rows(), 1000);

	myTest.archive="../output/flog_PnE/gs";
	myTest.tolerance = 0.1;
	myTest.skipPops = {18,19};
	myTest.config.fcs_read_param.data.which_lines = {};

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(flog_PnE)
{

	myTest.filename="../wsTestSuite/flog_PnE/Liver.wsp";
	myTest.sample_name_location = SAMPLE_NAME_LOCATION::SAMPLE_NODE;
	myTest.config.sample_filters["name"]={"Tissues_Liver_001.fcs"};

	myTest.config.fcs_read_param.data.which_lines = {1000};
	unique_ptr<flowJoWorkspace> ws = openWorkspace(myTest.filename, myTest.sample_name_location,myTest.xmlParserOption);
	unique_ptr<GatingSet> gs = ws->to_GatingSet(0, myTest.config);
	BOOST_CHECK_EQUAL(gs->begin()->second->get_cytoframe_view().n_rows(), 1000);

	myTest.archive="../output/flog_PnE/gs";
	myTest.tolerance = 0.1;
	myTest.skipPops = {18,19};
	myTest.config.fcs_read_param.data.which_lines = {};

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(gate_extension)
{

	myTest.filename="../wsTestSuite/gate_extension/02-15-2013 ICS.xml";
	//myTest.wsType = WS_TYPE::WS_MAC_3;
	myTest.config.sample_filters["name"]={"9148_Neg1.fcs"};
	myTest.archive="../output/gate_extension/gs";
	myTest.tolerance = 0.1;
	myTest.skipPops = {15};
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(PBMC_HIPC_trial)
{

	myTest.filename="../wsTestSuite/PBMC/HIPC_trial/data/HIPC_trial.xml";
	myTest.config.sample_filters["name"]={"004_A1_A01.fcs","004_B1_B01.fcs"};
	myTest.config.keywords_for_uid = {};
//	myTest.config.num_threads = 2;
	myTest.archive="../output/HIPC_trial/gs";
//	g_loglevel = GATE_LEVEL;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(PBMC_Blomberg)
{
	myTest.filename="../wsTestSuite/PBMC/Blomberg/Exp2_Tcell.wsp";
	//myTest.wsType = WS_TYPE::WS_WIN;
	myTest.config.sample_filters["name"]={"Exp2_Sp004_1_Tcell.fcs", "Exp2_Sp004_2_Tcell.fcs"};
	myTest.sample_name_location=SAMPLE_NAME_LOCATION::SAMPLE_NODE;


	myTest.archive="../output/Blomberg/gs";
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(ITN029ST)
{
	myTest.filename="../fjWsExamples/QA_template.xml";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]={"01107122_F11_I003.fcs", "01177007_F02_I016.fcs"};
	myTest.archive="../output//ITN/gs";
	myTest.config.data_dir = "../wsTestSuite/ITN029ST";
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Cytotrol_NHLBI)
{
	myTest.filename="../wsTestSuite/Cytotrol/NHLBI/flowJo/NHLBI.xml";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]={"CytoTrol_CytoTrol_1.fcs"};
	myTest.config.data_dir = "../wsTestSuite/Cytotrol/NHLBI/Tcell";
	myTest.config.keywords_for_uid={};
	myTest.group_id = 3;
	myTest.archive="../output/NHLBI/gs/gs";
//	g_loglevel = GATE_LEVEL;

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(bypassfaultynode)
{
	myTest.filename="../wsTestSuite/bypassfaultynode.xml";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]={"CytoTrol_CytoTrol_1.fcs"};
	myTest.config.data_dir = "../wsTestSuite/Cytotrol/NHLBI/Tcell";
	myTest.config.skip_faulty_node = true;
	myTest.config.keywords_for_uid={};
	myTest.group_id = 3;
	myTest.archive="../output/NHLBI/gs/gs";
//	g_loglevel = GATE_LEVEL;

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
//BOOST_AUTO_TEST_CASE(HVTN080_batch_1057) //missing fcs
//{
//	myTest.filename="../fjWsExamples/080 Batch 1057 M.xml";
//	//myTest.wsType = WS_TYPE::WS_MAC;
//	myTest.config.sample_filters["name"]={"517614.fcs"};
//	myTest.config.data_dir = "../wsTestSuite/HVTN/080";
//	myTest.archive="../output/HVTN080/gs";
//
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
//BOOST_AUTO_TEST_CASE(HVTN080_batch_0939)//missing fcs
//{
//	myTest.filename="../fjWsExamples/080 batch 0939.xml";
//	//myTest.wsType = WS_TYPE::WS_MAC;
//	myTest.config.sample_filters["name"]={"461648.fcs"};
//
//	myTest.config.data_dir = "../wsTestSuite/HVTN/080";
//
//	myTest.archive="../output/NormalizationData/gs";
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_A)
{
	myTest.filename="../wsTestSuite/vX/Lesson_8_vX.wsp";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]={"A1.fcs"};



	myTest.archive="../output/vX/A1/gs/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(Lesson_8_vX_B)
{
	myTest.filename="../wsTestSuite/vX/Lesson_8_vX.wsp";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]={"B1 .fcs"};



	myTest.archive="../output/vX/B1/gs/gs";

	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}

/**
 * this workspace somehow breaks on load BS archive for ellipsoidGate
 * But it works with PB archive
 *  either it is to be further investigation or to be ignored if no hisotry BS archive associated with ellipsoidGate
 */
BOOST_AUTO_TEST_CASE(bioaster)
{

	myTest.filename="../wsTestSuite/bioaster_ellipsoidGate/Matrice 1.wsp";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]={"PANEL 1_Matrice 1.fcs"};



	myTest.archive="../output/bioaster/gs/gs";


	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(ragon)
{
	myTest.filename="../wsTestSuite/Ragon/neut v non neut v9.xml";
	//myTest.wsType = WS_TYPE::WS_MAC_3;
	myTest.config.sample_filters["name"]={"477889_env_cct_norm_concatenated.txt"};

	myTest.config.fcs_file_extension = ".txt";
	myTest.archive="../output/Ragon/gs";




	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(JJ)
{
	myTest.filename="../wsTestSuite/JJ/JJ_FlowJo_.xml";
	//myTest.wsType = WS_TYPE::WS_MAC;
	myTest.config.sample_filters["name"]= {"Panel 2_A9001.fcs"};


//
	myTest.archive="../output/JJ/gs";

	myTest.tolerance = 0.5;


//	map<string,float> gains;
//	gains["Time"] = 0.01;
//	myTest.gains = gains;
	vector<VertexID> skip;
	skip.push_back(174);
	myTest.skipPops = skip;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
/**
 * EllipsoidGate defined on both linear and non-linear channels
 */
BOOST_AUTO_TEST_CASE(treg)
{
	myTest.filename="../wsTestSuite/McGill/Treg/20131206_Treg.1.ellipseidGate.wsp";
	myTest.archive="../output/McGill/Treg/gs";
	myTest.group_id = 3;
	myTest.config.cf_dir = "../output/McGill/Treg";
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
//BOOST_AUTO_TEST_CASE(EllipsoidGate_10)
//{
//	myTest.filename="../wsTestSuite/EllipsoidGate_10.2/mA J21 for HT.wsp";
//	//myTest.wsType = WS_TYPE::WS_VX;
//	myTest.config.sample_filters["name"]= {"MarquageA_Lung_IN_001.fcs"};
//
//
//
//	myTest.archive="../output/EllipsoidGate_10.2/gs";
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
/**
 *  CyTOF data with elliposoidGate and fasinh transformation defined
 */
BOOST_AUTO_TEST_CASE(provide)
{
	myTest.filename="../wsTestSuite/PROVIDE/batch1 local and week 53.wsp";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]= {"1097pi_cells_found_normalized.fcs"};
	myTest.sample_name_location=SAMPLE_NAME_LOCATION::SAMPLE_NODE;
	myTest.config.keywords_for_uid={};

	myTest.archive="../output/PROVIDE/gs";

	myTest.tolerance = 0.2;
	vector<VertexID> skip;
//	skip.push_back(16);
//	skip.push_back(99);
//	myTest.skipPops = skip;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(curlyquad1)
{
	myTest.filename="../wsTestSuite/curlyQuad/example1/20151208_TBNK_DS.xml";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]= {"A1001.001.fcs"};


//
	myTest.archive="../output/curlyQuad1/gs";

	myTest.tolerance = 0.15;


//	map<string,float> gains;
//	gains["Time"] = 0.01;
//	myTest.gains = gains;
	vector<VertexID> skip;
//	skip.push_back(16);
//	skip.push_back(99);
//	myTest.skipPops = skip;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(curlyquad2)
{
	myTest.filename="../wsTestSuite/curlyQuad/example2/20-Apr-2016.wsp";
	//myTest.wsType = WS_TYPE::WS_VX;
	myTest.config.sample_filters["name"]= {"CytoTrol_CytoTrol_1.fcs"};



	myTest.archive="../output/curlyQuad2/gs";

//	myTest.tolerance = 0.2;


//	map<string,float> gains;
//	gains["Time"] = 0.01;
//	myTest.gains = gains;
	vector<VertexID> skip;
//	skip.push_back(16);
//	skip.push_back(99);
//	myTest.skipPops = skip;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
BOOST_AUTO_TEST_CASE(faultylinearTransform)
{
    myTest.filename="../wsTestSuite/faultylinearTransform/FlowJo Test.wsp";
	myTest.config.sample_filters["name"]= {"P1_1_concatenated.txt_340.fcs"};

//    myTest.sampNloc=1;
    myTest.archive="../output/faultylinearTransform/gs";

//  myTest.tolerance = 0.2;


//  map<string,float> gains;
//  gains["Time"] = 0.01;
//  myTest.gains = gains;
    vector<VertexID> skip;
//  skip.push_back(16);
//  skip.push_back(99);
//  myTest.skipPops = skip;
    parser_test(myTest);

    vector<bool> isTrue(myTest.isEqual.size(), true);
    BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}

BOOST_AUTO_TEST_CASE(searchRefNode)
{
	myTest.filename="../wsTestSuite/searchRefNode/2583-Y-MAL067-FJ.xml";
	//myTest.wsType = WS_TYPE::WS_MAC_3;
	myTest.config.sample_filters["name"]= {"1379326.fcs"};



	myTest.archive="../output/searchRefNode/gs";
	vector<VertexID> skip;
	  skip.push_back(9);
	//  skip.push_back(99);
	  myTest.skipPops = skip;
	parser_test(myTest);

	vector<bool> isTrue(myTest.isEqual.size(), true);
	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());

}
//BOOST_AUTO_TEST_CASE(invalid_biexp_parameter)
//{
//
//	myTest.filename="/home/wjiang2/R/x86_64-pc-linux-gnu-library/3.4/flowWorkspaceData/extdata/A2004Analysis.xml";
//	//myTest.wsType = WS_TYPE::WS_MAC;
//	myTest.config.sample_filters["name"]={"a2004_O1T2pb05i_A1_A01.fcs"};
//
//
//
//	myTest.archive="../output/invalid_biexp_parameter/gs";
////	g_loglevel = GATE_LEVEL;
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
//BOOST_AUTO_TEST_CASE(mssm)
//{
//	myTest.filename="../data/mssm/CFSP_Analysis14.wsp";
//	//myTest.wsType = WS_TYPE::WS_VX;
//	myTest.config.sample_filters["name"]={"35120.fcs"};
//
//
//
//	myTest.archive="../data/mssm/gs";
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
//BOOST_AUTO_TEST_CASE(RV144) //workspace missing (original  .xml is not valid xml)
//{
//	myTest.filename="../fjWsExamples/Batch 1264 RV144.wsp";
//	//myTest.wsType = WS_TYPE::WS_WIN;
//	myTest.config.sample_filters["name"]={"977531.fcs"};
//
//
//
//	myTest.archive="../output/RV144/gs/gs";
//
//
//
//	parser_test(myTest);
//
//	vector<bool> isTrue(myTest.isEqual.size(), true);
//	BOOST_CHECK_EQUAL_COLLECTIONS(myTest.isEqual.begin(), myTest.isEqual.end(),isTrue.begin(), isTrue.end());
//
//}
BOOST_AUTO_TEST_SUITE_END()
