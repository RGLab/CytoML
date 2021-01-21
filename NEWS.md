# CytoML 1.0.0

* First submission

# CytoML 1.1.2

* support diva workspace parsing


# CytoML 1.11.11 (Bioconductor 3.10)

## API Changes

* Change handling of quad gates according to RGLab/cytolib#16
* Renaming of methods:
    * `openWorkspace` -> `open_diva_xml`, `open_flowjo_xml`
    * `cytobankExperiment` -> `open_cytobank_experiment`
    * `cytobank2GatingSet` -> `cytobank_to_gatingset`
    * `parseWorkspace` -> `flowjo_to_gatingset`, `diva_to_gatingset`
    * `getSampleGroups` -> `fj_ws_get_sample_groups`, `diva_get_sample_groups`
    * `getSamples` -> `fj_ws_get_samples`, `diva_get_samples`
    * `getKeywords` -> `fj_ws_get_keywords`
    * `getCompensationMatrices` -> `ce_get_compensations`
    * `getTransformation` -> `ce_get_transformations`
    * `compare.counts` -> `gs_compare_cytobank_counts`

* Renaming of classes:
    * `divaWorkspace` -> `diva_workspace`
    * `flowJoWorkspace` -> `flowjo_workspace`
    
* Add `CytoML.par.set`, `CytoML.par.get` for setting parameters in `CytoML` namespace

## Fixes/internal changes

* Make `gatingset_to_cytobank` export cytobank ML with attribute namespaces
* Allow `diva_to_gatingset `to use compensation matrix from xml
* Pass `...` args from `cytobank_to_gatingset` appropriately down to FCS parser
* Fix some issues with scaling of gates parsed from Diva workspace (#64)
* Guard against unsupported transformations being added to `GatingSet` during Diva parsing
* Switch `diva_to_gatingset` to using `flowjo_log_trans` instead of `logtGml2_trans`
* Fix ported flowUtils::xmlTag to enable self-closing tags
* Make `gating.graphGML` lookup tailored gates by FCS name as well as file id
* Add some flexibility to `getSpilloverMat` used in `gatingset_to_flowjo`

# CytoML 1.13.x (Bioconductor 3.11)

## API Changes

* Rename argument `sampNLoc` -> `sample_names_from` in `open_flowjo_xml`
* All parsers (`flowjo/cytobank/diva_to_gatingset`) now return `GatingSet` based on `cytoset` rather than `ncdfFlowSet`
* Add `trans` argument to `cytobank_to_gatingset` to allow overriding of transformations from gatingML file (#76)
* `gatingset_to_flowjo` now uses a docker image with a compiled converter: hub.docker.com/r/wjiang2/gs-to-flowjo
* Some updates to how `flowjo_to_gatingset` searches for FCS files (#77)
* Add include_empty_tree option to `flowjo_to_gatingset` to include samples without gates
* Allow `gatingset_to_flowjo` to take a path to a `GatingSet` archive directory
* Add `gating_graphGML` to replace `gating.graphGML` method for `openCyto::gating generic`
* Filter samples by panel when parsing cytobank experiment and add `ce_get_samples`, `ce_get_panels`


## Fixes/internal changes

* Automatic time scaling of samples from FlowJo workspaces now handled by `flowjo_to_gatingset` RGLab/cytolib#33
* Handle change to default `stringsAsFactors=FALSE` in R 4.0
* Eliminated extra intermediate files left in temp directory during workspace parsing
* Switch usage of `GatingSetList` to `merge_gs_list`
* Solve some Windows build issues
* Switch from `experimental::filesystem` to `boost::filesystem` in C++ FlowJo parser
* Add CytoML XSD to installation

