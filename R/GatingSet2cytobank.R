#' Convert a GatingSet to a Cytobank-compatible gatingML
#'
#' this function retrieves the gates from GatingSet and writes a customed GatingML-2.0 file
#' that can be imported into cytobank.
#'
#' The process can be divided into four steps:
#' 1. Read in gate geometry, compensation and transformation from gatingSet
#' 2. Rescale gate boundaries with flowJoTrans() so gates can be displayed properly in Cytobank
#' 3. Save gates and hierarchy structure to R environment
#' 4. Write environment out to gatingML using write.GatingML()
#'
#' @importFrom  flowUtils write.gatingML
#' @importFrom XML saveXML xmlTreeParse xmlRoot
#' @importFrom utils localeToCharset packageVersion
#' @export
#' @return nothing
#' @param gs a GatingSet object
#' @param outFile a file name
#' @param showHidden whether to include the hidden population nodes in the output
#' @param cytobank.default.scale logical flag indicating whether to use the default Cytobank asinhtGml2 settings.
#'                              Currently it should be set to TRUE in order for gates to be displayed properly in Cytobank
#'                              because cytobank currently does not parse the global scale settings from GatingML.
#' @param ...
#'        rescale.gate default is TRUE. which means the gate is rescaled to the new scale that is understandable by cytobank.
#'        It is recommended not to change this behavior unless user wants to export to a gatingML file used for other purpose other
#'        than being imported into cytobank.
#' @examples
#' library(flowWorkspace)
#'
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'
#' Rm("CD8", gs)
#'
#' #output to cytobank
#' outFile <- tempfile(fileext = ".xml")
#' GatingSet2cytobank(gs, outFile) #type by default is 'cytobank'
#'
#'
GatingSet2cytobank <- function(gs, outFile, showHidden = FALSE, cytobank.default.scale = TRUE, ...){

  #convert comp and trans as GML2 compatible format and save to env
  if(cytobank.default.scale)
    warning("With 'cytobank.default.scale' set to 'TRUE', data and gates will be re-transformed with cytobank's default scaling settings, which may affect how gates look like.")

  flowEnv <- new.env(parent = emptyenv())
  res <- export_comp_trans(gs, flowEnv, cytobank.default.scale = cytobank.default.scale, type = "cytobank")
  #convert gates to GML2
  export_gates_cytobank(gs, flowEnv, res[["trans.Gm2objs"]], res[["trans"]], res[["compId"]], showHidden = showHidden, ...)

  tmp <- tempfile(fileext = ".xml")#ensure correct file extension for xmlTreeParse to work
  flowUtils::write.gatingML(flowEnv, tmp)
  tree <- xmlTreeParse(tmp, trim = FALSE)
  root <- xmlRoot(tree)
  # browser()

  root <- addCustomInfo(root, gs, flowEnv, showHidden = showHidden, cytobank.default.scale = cytobank.default.scale)
  #add pop (GateSet/BooleanAndGate)
  root <- addGateSets(root, gs, flowEnv[["guid_mapping"]], showHidden = showHidden)
  #add experiment info to custom node
  root <- addExperimentInfo(root)
  suppressWarnings(saveXML(root, file = outFile))#(suppress the warning due to the usage of deprecated structure call in saveXML)
}

export_gates_cytobank <- function(gs, flowEnv, trans.Gm2objs, trans, compId, showHidden, rescale.gate = TRUE)
{
  #add gates and pops(as GateSets)
  nodePaths <- getNodes(gs, showHidden = showHidden)[-1]
  gh <- gs[[1]]

  fcs_guids <- sampleNames(gs)
  rng <- range(getData(gh, use.exprs = FALSE))
  grp.list <- sapply(fcs_guids, function(sn){
    grps <- ggcyto:::merge.quad.gates(gs[[sn]], nodePaths)
    #unlist the grp so that the gates that can't be merged to quadgates
    #will still be treated by cytobank independently but the quad.gates will be specially treated  as the one QuadrantGate node
    grps <- lapply(grps, function(grp){
      if(is.list(grp))
      {
        if(is.list(grp[["popIds"]]))
          grp <- list(c(grp[["popIds"]], grp["parentId"]))
        else
          grp <- grp[["popIds"]]
      }
      grp
    })
    unlist(grps, recursive = FALSE)
  }, simplify = FALSE)

  gate_id <- 0
  for(i in seq_along(grp.list[[1]])){
    gate.obj <- grp.list[[1]][[i]]
    if(is.list(gate.obj))
    {
      isQuad <- TRUE
      #quad gates
      #  <-  #get parent as the dummy node path for there is no such path for quadGate in gs
      nodePath <- paste0(gate.obj[["parentId"]], " sub")
      gates <- sapply(grp.list, function(grp){
        grp[[i]][["quad.gate"]]
        })
      # gate_id <- gate_id + 5 #preserve 4 spaces for quadrants#TODO:no longer needed, since quadrants use id instead of gate_id

    }else
    {
      isQuad <- FALSE
      nodePath <- gate.obj
      # gate_id <- nodePath
      gates <- getGate(gs, nodePath)
      # gate_id <- gate_id + 1#increment gate id

    }

    gate_id <- gate_id + 1#increment gate id

    for(fcs_id in seq_along(fcs_guids)){
      fcs_guid <- fcs_guids[fcs_id]
      gate <- gates[[fcs_guid]]

      #cytobank does not support negated gate
      #we have to create inverse gate on our end
      if(!isQuad)
      {
        if(flowWorkspace:::isNegated(gs[fcs_guid][[1]], nodePath))
          gate <- inverse(gate, rng)
      }
      #transform to raw scale
      #and attach comp and trans reference to parameters
      gate <- processGate(gate, trans.Gm2objs, compId, flowEnv, rescale.gate, trans)

      # parent <- getParent(gs, nodePath)
      # if(parent == "root")
      #   parent_id <- 0
      # else
      #   parent_id <- match(parent, nodePaths)

      guid <- paste("gate", gate_id, fcs_id, sep = "_")#can't save path into xml attr use idx i here
      identifier(gate) <- guid
      #save quadrant info to gate
      if(isQuad)
      {
        attr(gate, "quad.pop.name") <- gate.obj[["pop.name"]]
        attr(gate, "quad.pattern") <- gate.obj[["quad.pattern"]]

      }
      attr(gate, "nodePath") <- nodePath
      #add gate
      flowEnv[[guid]] <- gate


    }
  }
}


#' @importFrom base64enc base64encode base64decode
base64encode_cytobank <- function(x){
  x <- base64encode(charToRaw(x))
  x <- gsub("=", ".", x)
  x <- gsub("\\+", "_", x)
  x <- gsub("/", "-", x)
  x
}
base64decode_cytobank <- function(x){
  x <- gsub("\\.", "=", x)
  x <- gsub("_", "\\+", x)
  x <- gsub("-", "/", x)
  base64decode(x)
}


#' @importFrom XML xmlTree
addGateSets <- function(root, gs, showHidden, guid_mapping)
{

  nodePaths <- names(guid_mapping)
  # browser()
  newNodes <- lapply(nodePaths, function(nodePath){

                      guid <- guid_mapping[[nodePath]]
                      gate_id <- strsplit(guid, split = "_")[[1]][[2]]
                      curNode <- nodePath
                      pop_name <- basename(nodePath)
                      gate_id_path <- gate_id
                      names(gate_id_path) <- curNode
                      # browser()
                      repeat{
                        curNode <- getParent(gs, curNode)
                        if(curNode == "root")
                          break
                        else{
                          cur_parent_id <- match(curNode, nodePaths)
                          names(cur_parent_id) <- curNode
                          gate_id_path <- c(cur_parent_id, gate_id_path)
                        }

                      }
                      GateSetNode(gate_id, pop_name, gate_id_path, guid_mapping)
                    })

  addChildren(root, kids = newNodes)
}

#' @importFrom jsonlite toJSON
#' @importFrom XML xmlNode
GateSetNode <- function(gate_id, pop_name, gate_id_path, guid_mapping){

  attrs = c("gating:id" = paste("GateSet", gate_id, sep = "_"))

  definition <- toJSON(list(gates = as.vector(gate_id_path), negGates = vector()))

  #duplicate the refs if it is the root
  ref_gate_path <- names(gate_id_path)
  if(length(ref_gate_path) == 1)
    ref_gate_path <- c(ref_gate_path, ref_gate_path)
  xmlNode("gating:BooleanGate", attrs = attrs
          , xmlNode("data-type:custom_info"
                    , xmlNode("cytobank"
                              , xmlNode("name", pop_name)
                              , xmlNode("gate_set_id", gate_id)
                              , xmlNode("definition", I(definition))#set AsIs to avoid xml escaping
                              )
                    )

         ,  xmlNode("gating:and"
                  #create two dummy reference
                  , .children = lapply(ref_gate_path, function(gate_path){

                    guid <- guid_mapping[[gate_path]]
                    attrs = c("gating:ref" = guid)
                    xmlNode("gating:gateReference", attrs = attrs)
                  })
                )
        )
}

#' add customInfo nodes to each gate node and add BooleanAndGates
#' @inheritParams GatingSet2cytobank
#' @param root the root node of the XML
#' @param flowEnv the environment that stores the information parsed by 'read.GatingML'.
#' @importFrom  XML xmlAttrs getNodeSet addChildren xmlAttrs<-
#' @importFrom flowWorkspace pData
#' @return XML root node
addCustomInfo <- function(root, gs, flowEnv, cytobank.default.scale = TRUE, showHidden){
  quad.pattern.cytobank <- c("++", "-+", "--","+-")
  pd <- pData(gs)
  # fcs_names <- pd[["name"]]
  fcs_guids <- rownames(pd)
  translist <- getTransformations(gs[[1]], only.function = FALSE)
  transNames <- names(translist)
  rng <- range(gs[[1]], raw.scale = TRUE)
  #retrieve the prefix for latter trans matching
  cmp <- flowWorkspace:::.cpp_getCompensation(gs@pointer, sampleNames(gs)[[1]])
  prefix <- cmp$prefix
  suffix <- cmp$suffix
  id <- 0
  for(i in 1:length(root)){

    curNode <- root[[i]]
    guid <- as.vector(xmlAttrs(curNode)[["id"]])
    if(!is.null(guid)&&grepl("gate_", guid)){
        #parse pop and fcs info from guid
        fields <- strsplit(guid, "_")[[1]]
        gate_id <- as.integer(fields[[2]])
        fcs_id <- as.integer(fields[[3]])


        # fcs_name <- fcs_names[fcs_id]
        fcs_guid <- fcs_guids[fcs_id]
        fcs_name <- basename(keyword(gs[[fcs_guid]], "FILENAME"))#cytobank uses the actual filename instead of keyword to match tailor gates
        # browser()

        gate <- flowEnv[[guid]]
        nodePath <- attr(gate, "nodePath")
        pop_name<- basename(nodePath)
        gate_type <- class(gate)
        if(gate_type == "rectangleGate"){
          if(length(parameters(gate)) == 1)
            gate_type <- "RangeGate"
          else
            gate_type <- "RectangleGate"
        }else if(gate_type == "polygonGate")
          gate_type <- "PolygonGate"
        else if(gate_type == "ellipsoidGate")
          gate_type <- "EllipseGate"
        else if(gate_type == "quadGate")
          gate_type <- "QuadrantGate"
        else
          stop("unsupported gate: ", gate_type)
        # browser()
        # message(guid)
        #parse scale info from gate parameter
        scale <- lapply(gate@parameters@.Data, function(param){
          # browser()
          if(class(param) == "compensatedParameter"){
            if(cytobank.default.scale&&!is.cytof(gs)){
             thisRng <- c(1, 262144.0)
            }else{
                chnl <- as.vector(parameters(param))
                thisRng <- rng[, chnl]
              }

            flag <- 1
            argument <- "1"
          }else if(is(param, "singleParameterTransform")){

            chnl <- as.vector(parameters(param@parameters))
            chnl <- sub("(^Comp_)(.*)", "\\2", chnl) #strip the new prefix and add the original one before matching
            chnl <- paste0(prefix, chnl, suffix)
            # ind <- grepl(chnl, names(rng))
            ind <- names(rng) == chnl
            nMatched <- sum(ind)
            if(nMatched == 1){
              if(cytobank.default.scale){
                if(is.cytof(gs))
                  thisRng <- c(-5, 12000.0)
                else
                  thisRng <- c(-200, 262144.0)
              }else
                thisRng <- rng[, ind]
            }else if(nMatched == 0)
              stop(chnl , " not found in range info")
            else
              stop(chnl , " has multiple matches in range info")

            if(is(param, "asinhtGml2")){
              flag <- 4
              argument <- as.character(round(param@T/sinh(1)))
            }else if(is(param, "logicletGml2")){
             flag <- 4
             argument <- as.character(round(param@T/sinh(1)))
            }else if(is(param, "logtGml2")){
              flag <- 2
              argument <- as.character(1)
            }else
              stop("unsupported transform: ", class(param))

          }else
            stop("unsupported transform: ", class(param))

          thisRng <- round(thisRng, 2)
          list(flag = flag, argument = argument, min = thisRng[1], max = thisRng[2], bins = 256, size = 256)
        })
        if(length(scale) == 1){
          scale <- unlist(scale, recursive = FALSE)
        }else{
          names(scale) <- c("x", "y")
        }
        definition <- list(scale = scale)
        #quadgate requires the json definition
        if(gate_type == "QuadrantGate")
        {
          id <- id + 5 #reserve 4 ids for quadrants
          definition[["labels"]] <- matrix(c(7.015343642234706,7.620918572637606
                                             ,0.33115968153503017,7.620918572637606
                                             ,0.33115968153503017,-0.3809554276552931
                                             ,7.015343642234706,-0.3809554276552931)
                                           , nrow = 4, byrow = TRUE)
          definition[["quadrant"]] <- list(x = gate@boundary[1], y = gate@boundary[2]
                                           , UR = 1, UL = 2, LL = 3, LR = 4)
        }else
          id <- id + 1
        definition <- toJSON(definition, auto_unbox = TRUE)
        #insert custom info
        customNode <- customInfoNodeForGate(id, gate_id, pop_name, fcs_id, fcs_name, gate_type, definition)
        newNode <- addChildren(curNode, kids = list(customNode), at = 0)
        #modify gate id so that cytobank can parse it
        #also must use id since tailored gates shared the same gate_id and can't be used in the final version of gatingML node
        guid.new <- paste("Gate", id, base64encode_cytobank(pop_name), sep = "_")
        xmlAttrs(newNode)[["id"]] = guid.new



        #special treatment for quadGate
        #modify the id of each divider and quadrant
        if(gate_type == "QuadrantGate")
        {
          quad_id <- id - 5
          quad.pattern <- attr(gate, "quad.pattern")
          for(j in seq_len(length(newNode)))
          {
            subNode <- newNode[[j]]
            nodeName <- xmlName(subNode)
            if(nodeName == "divider"){#divider use the same id as parent quadgate node
              old.id <- xmlAttrs(subNode)[["id"]]
              div.id <- substr(old.id, nchar(old.id), nchar(old.id))
              div.guid.new <- paste0(guid.new, "divider_", div.id)
              xmlAttrs(subNode)[["id"]] = div.guid.new
            }else if(nodeName == "Quadrant")
            {
              #update id
              #extract and convert flowUtils quad pattern to +-
              old.id <- xmlAttrs(subNode)[["id"]]
              this.pattern <- substr(old.id, nchar(old.id)-1, nchar(old.id))
              this.pattern <- gsub("N", "-", gsub("P", "+", this.pattern))
              pat.ind <- match(this.pattern, quad.pattern)
              # quad.ord <- match(this.pattern, quad.pattern.cytobank)
              quad_id <- quad_id + 1
              nodePath <- attr(gate, "quad.pop.name")[pat.ind]
              pop_name <- basename(nodePath)
              quad.guid.new <- paste("Gate", quad_id, base64encode_cytobank(pop_name), sep = "_")
              xmlAttrs(subNode)[["id"]] <- quad.guid.new
              #update divider ref id
              xmlAttrs(subNode[[2]])[["divider_ref"]] <- paste0(guid.new, "divider_1")
              xmlAttrs(subNode[[4]])[["divider_ref"]] <- paste0(guid.new, "divider_2")
              if(fcs_id == 1)#record the mapping between gate_id and guid.new for the refs of GateSets
                flowEnv[["guid_mapping"]][[nodePath]] <- quad.guid.new
            }
            newNode[[j]] <- subNode
          }

        }else
        {
          if(fcs_id == 1)#record the mapping between gate_id and guid.new for the refs of GateSets
            flowEnv[["guid_mapping"]][[nodePath]] <- guid.new
        }
        #update the tree
        root[[i]] <- newNode

    }
  }
  root

}

#' @importFrom  XML newXMLNode
customInfoNodeForGate <- function(id, gate_id, pop_name, fcs_id, fcs_name, type, definition)
{
    if(fcs_id == 1){
      fcs_id <- fcs_name <- ""
    }

 #avoid using newXMLNode since it is not segfault-free.
  xmlNode("data-type:custom_info"
      , xmlNode("cytobank"
          , xmlNode("name", pop_name)
          , xmlNode("id", id) #unique for each gate node (include tailored gates)
          , xmlNode("gate_id", gate_id) #unique for each gate path (tailored gates share the same gate_id)
          , xmlNode("type", type)
          , xmlNode("version", 1)
          , xmlNode("fcs_file_id", fcs_id)
          , xmlNode("fcs_file_filename", fcs_name)
          , xmlNode("definition", I(definition))
          )
      )
}

addExperimentInfo <- function(root, experiment_number = ""){

   customNode <- root[["custom_info"]]
   customNode <- addChildren(customNode, xmlNode("flowWorkspace-version", packageVersion("flowWorkspace")))
   customNode <- addChildren(customNode, xmlNode("CytoML-version", packageVersion("CytoML")))
   newNode <- xmlNode("cytobank"
                      , xmlNode("experiment_number", experiment_number)
   )
   customNode <- addChildren(customNode, newNode, at = 0)

   root[["custom_info"]] <- customNode
   root
}

