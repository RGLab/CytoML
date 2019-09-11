// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/cytoml2.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// gs_to_flowjo
void gs_to_flowjo(XPtr<GatingSet> gs, string outputfile, bool show_hidden);
RcppExport SEXP _cytoml2_gs_to_flowjo(SEXP gsSEXP, SEXP outputfileSEXP, SEXP show_hiddenSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<GatingSet> >::type gs(gsSEXP);
    Rcpp::traits::input_parameter< string >::type outputfile(outputfileSEXP);
    Rcpp::traits::input_parameter< bool >::type show_hidden(show_hiddenSEXP);
    gs_to_flowjo(gs, outputfile, show_hidden);
    return R_NilValue;
END_RCPP
}
// open_workspace
XPtr<flowJoWorkspace> open_workspace(string filename, int sample_name_location, int xmlParserOption);
RcppExport SEXP _cytoml2_open_workspace(SEXP filenameSEXP, SEXP sample_name_locationSEXP, SEXP xmlParserOptionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type sample_name_location(sample_name_locationSEXP);
    Rcpp::traits::input_parameter< int >::type xmlParserOption(xmlParserOptionSEXP);
    rcpp_result_gen = Rcpp::wrap(open_workspace(filename, sample_name_location, xmlParserOption));
    return rcpp_result_gen;
END_RCPP
}
// parse_workspace
XPtr<GatingSet> parse_workspace(XPtr<flowJoWorkspace> ws, int group_id, List subset, bool execute, string path, string h5_dir, bool includeGates, vector<string> additional_keys, bool additional_sampleID, vector<string> keywords, bool is_pheno_data_from_FCS, bool keyword_ignore_case, float extend_val, float extend_to, bool channel_ignore_case, bool leaf_bool, List comps, bool transform, string fcs_file_extension, FCS_READ_PARAM fcs_parse_arg, int num_threads);
RcppExport SEXP _cytoml2_parse_workspace(SEXP wsSEXP, SEXP group_idSEXP, SEXP subsetSEXP, SEXP executeSEXP, SEXP pathSEXP, SEXP h5_dirSEXP, SEXP includeGatesSEXP, SEXP additional_keysSEXP, SEXP additional_sampleIDSEXP, SEXP keywordsSEXP, SEXP is_pheno_data_from_FCSSEXP, SEXP keyword_ignore_caseSEXP, SEXP extend_valSEXP, SEXP extend_toSEXP, SEXP channel_ignore_caseSEXP, SEXP leaf_boolSEXP, SEXP compsSEXP, SEXP transformSEXP, SEXP fcs_file_extensionSEXP, SEXP fcs_parse_argSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type group_id(group_idSEXP);
    Rcpp::traits::input_parameter< List >::type subset(subsetSEXP);
    Rcpp::traits::input_parameter< bool >::type execute(executeSEXP);
    Rcpp::traits::input_parameter< string >::type path(pathSEXP);
    Rcpp::traits::input_parameter< string >::type h5_dir(h5_dirSEXP);
    Rcpp::traits::input_parameter< bool >::type includeGates(includeGatesSEXP);
    Rcpp::traits::input_parameter< vector<string> >::type additional_keys(additional_keysSEXP);
    Rcpp::traits::input_parameter< bool >::type additional_sampleID(additional_sampleIDSEXP);
    Rcpp::traits::input_parameter< vector<string> >::type keywords(keywordsSEXP);
    Rcpp::traits::input_parameter< bool >::type is_pheno_data_from_FCS(is_pheno_data_from_FCSSEXP);
    Rcpp::traits::input_parameter< bool >::type keyword_ignore_case(keyword_ignore_caseSEXP);
    Rcpp::traits::input_parameter< float >::type extend_val(extend_valSEXP);
    Rcpp::traits::input_parameter< float >::type extend_to(extend_toSEXP);
    Rcpp::traits::input_parameter< bool >::type channel_ignore_case(channel_ignore_caseSEXP);
    Rcpp::traits::input_parameter< bool >::type leaf_bool(leaf_boolSEXP);
    Rcpp::traits::input_parameter< List >::type comps(compsSEXP);
    Rcpp::traits::input_parameter< bool >::type transform(transformSEXP);
    Rcpp::traits::input_parameter< string >::type fcs_file_extension(fcs_file_extensionSEXP);
    Rcpp::traits::input_parameter< FCS_READ_PARAM >::type fcs_parse_arg(fcs_parse_argSEXP);
    Rcpp::traits::input_parameter< int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(parse_workspace(ws, group_id, subset, execute, path, h5_dir, includeGates, additional_keys, additional_sampleID, keywords, is_pheno_data_from_FCS, keyword_ignore_case, extend_val, extend_to, channel_ignore_case, leaf_bool, comps, transform, fcs_file_extension, fcs_parse_arg, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// get_keywords_by_id
KW_PAIR get_keywords_by_id(XPtr<flowJoWorkspace> ws, int sample_id);
RcppExport SEXP _cytoml2_get_keywords_by_id(SEXP wsSEXP, SEXP sample_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type sample_id(sample_idSEXP);
    rcpp_result_gen = Rcpp::wrap(get_keywords_by_id(ws, sample_id));
    return rcpp_result_gen;
END_RCPP
}
// get_keywords_by_name
KW_PAIR get_keywords_by_name(XPtr<flowJoWorkspace> ws, string sample_name);
RcppExport SEXP _cytoml2_get_keywords_by_name(SEXP wsSEXP, SEXP sample_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< string >::type sample_name(sample_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(get_keywords_by_name(ws, sample_name));
    return rcpp_result_gen;
END_RCPP
}
// get_sample_groups
List get_sample_groups(XPtr<flowJoWorkspace> ws);
RcppExport SEXP _cytoml2_get_sample_groups(SEXP wsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    rcpp_result_gen = Rcpp::wrap(get_sample_groups(ws));
    return rcpp_result_gen;
END_RCPP
}
// get_samples
List get_samples(XPtr<flowJoWorkspace> ws);
RcppExport SEXP _cytoml2_get_samples(SEXP wsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    rcpp_result_gen = Rcpp::wrap(get_samples(ws));
    return rcpp_result_gen;
END_RCPP
}
// get_xml_file_path
string get_xml_file_path(XPtr<flowJoWorkspace> ws);
RcppExport SEXP _cytoml2_get_xml_file_path(SEXP wsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<flowJoWorkspace> >::type ws(wsSEXP);
    rcpp_result_gen = Rcpp::wrap(get_xml_file_path(ws));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cytoml2_gs_to_flowjo", (DL_FUNC) &_cytoml2_gs_to_flowjo, 3},
    {"_cytoml2_open_workspace", (DL_FUNC) &_cytoml2_open_workspace, 3},
    {"_cytoml2_parse_workspace", (DL_FUNC) &_cytoml2_parse_workspace, 21},
    {"_cytoml2_get_keywords_by_id", (DL_FUNC) &_cytoml2_get_keywords_by_id, 2},
    {"_cytoml2_get_keywords_by_name", (DL_FUNC) &_cytoml2_get_keywords_by_name, 2},
    {"_cytoml2_get_sample_groups", (DL_FUNC) &_cytoml2_get_sample_groups, 1},
    {"_cytoml2_get_samples", (DL_FUNC) &_cytoml2_get_samples, 1},
    {"_cytoml2_get_xml_file_path", (DL_FUNC) &_cytoml2_get_xml_file_path, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_cytoml2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
