
#ifndef __CYTOML_H__
#define __CYTOML_H__
#include "flowWorkspace.h"
#include "CytoML/openWorkspace.hpp"
#include "cytolib-ml/flowjo_xml_node.hpp"

using namespace CytoML;
using namespace cytolib;

//header included somewhere(not sure where exactly) defines FALSE,which interferes RcppExports.cpp
#ifdef FALSE
  #undef FALSE
#endif

#endif // __flowWorkspace_h__
