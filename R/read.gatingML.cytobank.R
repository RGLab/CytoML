#' A graph object returned by 'read.gatingML.cytobank' function.
#'
#' Each node corresponds to a population(or GateSet) defined in gatingML file. The actual gate object (both global and tailored gates) is
#' associated with each node as nodeData. Compensation and transformations are stored in graphData slot.
#'
#' The class simply extends the graphNEL class and exists for the purpose of method dispatching.
#'
#' @importClassesFrom graph graphNEL graphBase graph
#' @importClassesFrom Biobase AssayData
#' @export
setClass("graphGML", contains = "graphNEL")

#' Parser for gatingML exported by Cytobank
#'
#' The Default parser (flowUtils::read.gatingML) does not  parse the population tree as well as
#' the custom information from cytobank. (e.g. gate name, fcs filename).
#'
#' @param file Gating-ML XML file
#' @param ... additional arguments passed to the handlers of 'xmlTreeParse'
#' @export
#' @importFrom flowUtils read.gatingML
#' @importFrom flowCore parameters parameters<-
#' @return a graphGML that represents the population tree.
#' The gate and population name are stored in nodeData of each node.
#' Compensation and transformations are stored in graphData.
#' @examples
#' xml <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "CytoML")
#' g <- read.gatingML.cytobank(xml) #parse the population tree
#' #plot(g) #visualize it
read.gatingML.cytobank <- function(file, ...){

  #parse all the elements:gate, GateSets, comp, trans
  flowEnv <- new.env()
  suppressWarnings(read.gatingML(file, flowEnv))#suppress the deprecated warnings from XML

  #parse gate info (id vs fcs and pop name)
  gateInfo <- parse.gateInfo(file)
  if(nrow(gateInfo) == 0)
    stop("No gates defined in gatingML file!")

  #restore the original gate parameter names
  #because flowUtils add comp and tran names as prefix which is really not neccessary (even problematic) here.
  for(objID in ls(flowEnv)){
    obj <- flowEnv[[objID]]
    if(is(obj, "parameterFilter")){

      # message(class(obj))

      sb <- gateInfo[id == objID, ]
      if(nrow(sb)>0){
        orig_params <- sb[, params]
        orig_params <- strsplit(split = ":", orig_params)[[1]]
        params <- parameters(obj)
        ind <- unlist(sapply(orig_params, function(orig_param)grep(paste0("\\Q", orig_param, "\\E$"), params)))
        if(length(ind)==0)
          stop("orig_param : '", paste(orig_params, collapse = ","), "'\n not matched to the prefixed params of the gate: \n'", paste(params, collapse = ","), "'")
        orig_param <- orig_params[ind]

        #cytobank add Comp_ prefix when the custom compensation is specified and referred by gates
        #We always strip it to ensure the bare channel names are used for gates (so that flow data does not need changes)
        orig_param <- sub("(^Comp_)(.*)", "\\2", orig_param)

        parameters(obj) <- orig_param
        flowEnv[[objID]] <-  obj
      }

    }

  }


  #construct tree from GateSets
  g <- constructTree(flowEnv, gateInfo)

  #determine comps and trans to be used

  comp_refs <- gateInfo[, comp_ref]
  comp_refs <- unique(comp_refs[comp_refs!=""])
  comp_refs <- comp_refs[comp_refs != "uncompensated"]
  if(length(comp_refs) == 0)
    comps <- "NONE"
  else if(length(comp_refs) > 1)
    stop("More than one compensation referred in gates!")
  else{
    if(comp_refs == "FCS")
      comps <- "FCS"
    else{
      comps <- flowEnv[[comp_refs]]
      #strip Comp_ prefix that was added by cytobank
#       mat <- comps@spillover
#       comp_param <- colnames(mat)
#       comp_param <- sapply(comp_param, function(i)sub("(^Comp_)(.*)", "\\2", i), USE.NAMES = FALSE)
#       colnames(mat) <- comp_param
#       comps <- compensation(mat)

    }

  }
  g@graphData[["compensation"]] <- comps


  trans <-  sapply(ls(flowEnv), function(i){

                                obj <- flowEnv[[i]]
                                #exclude the compensatedParameter since it also inherits transformation class
                                if(is(obj, "transformation")&&!is(obj, "compensatedParameter"))
                                  obj
                              }, USE.NAMES = FALSE)

  trans <- compact(trans)
  chnl <- sapply(trans, function(tran)unname(parameters(tran@parameters)), USE.NAMES = FALSE)

  ind <- chnl != "any"

  chnl <- chnl[ind]
  trans <- trans[ind]
  names(trans) <- chnl
  if(length(trans) > 0)
    g@graphData[["transformations"]] <- trans

  as(g, "graphGML")

}

#' Parse the cytobank custom_info for each gate
#'
#' Fcs filename and gate name stored in 'custom_info' element are beyong the scope of
#' the gatingML standard and thus not covered by the default 'read.gatingML'.
#'
#' @param file xml file path
#' @param ... additional arguments passed to the handlers of 'xmlTreeParse'
#' @return a data.frame that contains three columns: id (gateId), name (gate name), fcs (fcs_file_filename).
#' @importFrom XML xmlRoot xmlName xmlGetAttr xmlValue xmlElementsByTagName xmlChildren
#' @import data.table
parse.gateInfo <- function(file, ...)
{

  root <- xmlRoot(flowUtils:::smartTreeParse(file,...))

  rbindlist(
    lapply(xmlChildren(root), function(node){
            nodeName <- xmlName(node)

            if(grepl("*Gate", nodeName)){


                id <- xmlGetAttr(node, "id")

                name <- getCustomNodeInfo(node, "name")
                fcs_file_filename <- getCustomNodeInfo(node, "fcs_file_filename")
                gate_id <- getCustomNodeInfo(node, "gate_id")#used to find tailored gate
                if(nodeName %in% c("BooleanGate"))
                {
                  gate_def <- comp_ref <- trans_ref <- params <- ""
                }else
                {

                  gate_def <- getCustomNodeInfo(node, "definition")

                  if(nodeName == "QuadrantGate"){
                    dimTag <- "divider"
                    #update id with quardant definition
                    quadrantList <- xmlElementsByTagName(node, "Quadrant")
                    id <- unname(sapply(quadrantList, function(i)xmlGetAttr(i, "id")))

                  }else
                    dimTag <- "dimension"

                  dimNode <- xmlElementsByTagName(node, dimTag)
                  comp_ref <- unique(sapply(dimNode, function(i)xmlGetAttr(i, "compensation-ref")))
                  trans_ref <- unique(unname(unlist(compact(sapply(dimNode, function(i)xmlGetAttr(i, "transformation-ref"))))))
                  trans_ref <- ifelse(is.null(trans_ref), "", trans_ref)
                  params <- paste(sapply(dimNode, function(i)xmlGetAttr(i[["fcs-dimension"]], "name")), collapse = ":")
                }
                # message(name)
                # browser()
                data.table(id = id, name = name, gate_id = gate_id, fcs = fcs_file_filename
                  , comp_ref = comp_ref, trans_ref = trans_ref, params = params, gate_def = gate_def
                  )


            }

  })
  )
}

getCustomNodeInfo <- function(node, nodeName){
  custom_node <- xmlElementsByTagName(node, nodeName, recursive = TRUE)
  if(length(custom_node) >0 ){
    value <- xmlValue(custom_node[[1]])
    if(length(value)==0)
      value <- ""
  }
  else
    value <- ""

  value
}

#' Given the leaf node, try to find out if a collection of nodes can be matched to a path in a graph(tree) by the bottom-up searching
#' @param g graphNEL
#' @param leaf the name of leaf(terminal) node
#' @param nodeSet a set of node names
#' @return TRUE if path is found, FALSE if not path is matched.
#' @importFrom graph inEdges
matchPath <- function(g, leaf, nodeSet){

    if(leaf %in% nodeSet){
      nodeSet <- nodeSet[-match(leaf, nodeSet)] #pop it out
      leaf <- inEdges(leaf, g)[[1]]#update it with parent node
      if(length(nodeSet) == 0){ #nodeSet emptied
        if(length(leaf) == 0)  #and path reach to the top
          return (TRUE)
        else
          return(FALSE) #path has not reached to the top
      }else
      {
        if(length(leaf) == 0) #reach the top before nodeSet is emptied
          return(FALSE)
        else
          matchPath(g, leaf, nodeSet)  #continue match the path against nodeSet
      }
    }else
      return(FALSE)

}

#' Reconstruct the population tree from the GateSets
#' @param flowEnv the enivornment contains the elements parsed by read.gatingML function
#' @param gateInfo the data.frame contains the gate name, fcs filename parsed by parse.gateInfo function
#' @return a graphNEL represent the population tree. The gate and population name are stored as nodeData in each node.
#' @importFrom graph graphNEL nodeDataDefaults<- nodeData<- addEdge edges removeNode addNode
#' @importFrom jsonlite fromJSON
constructTree <- function(flowEnv, gateInfo){

  #parse the references from boolean gates
  #currently we use refs (which point to the gate id defined by the standard)
  #we don't want to use gate_id from custom_info used by cytobank internally
  #because quadrantGate seems do not have this id for each of its quadrant elements
  gateSets <- sapply(ls(flowEnv), function(i){
    obj <- flowEnv[[i]]
    if(class(obj) == "intersectFilter"){
      refs <- obj@filters
      refs <- sapply(refs, slot, "name")
      unique(refs) #make root node depth of 1
    }
  })

  gateSets <- compact(gateSets)
  if(length(gateSets) == 0)
    stop("No GateSets defined in gatingML file!")
  popIds <- names(gateSets)
  #sort by the depths of path
  counts <- sapply(gateSets, function(i)length(i))
  counts <- sort(counts)

  #init the graph with all gates that have been referred
  # gateIds <- unique(unlist(gateSets))
  g <- graphNEL(nodes = popIds, edgemode = "directed")

  nodeDataDefaults(g, "popName") <- ""
  nodeDataDefaults(g, "gateInfo") <- list()

  # add edges (from nDepths 2 to n)
  for(popId in names(counts)){
    thisGateSet <- gateSets[[popId]]
    nDepth <- length(thisGateSet)

    popName <- gateInfo[id == popId, name]
    #add pop name
    nodeData(g, popId, "popName") <- popName
    message(popName)

    if(nDepth == 1){#root node
      gateID <- thisGateSet[1]
      g <- addGate(gateInfo,flowEnv, g, popId, gateID)
    }else{
      #traverse all the potential parent GateSets(i.e. subset)
      #must go all the way up to the root
      #because the difference between parent and current node could be more than level 1
      for(pDepth in (nDepth - 1):1){
        parents_popIds <- names(counts[counts == pDepth])
        matchRes <- findParent(gateSets, thisGateSet, parents_popIds)
        if(!is.null(matchRes))
          break
      }
      parentID <- matchRes[["parentID"]]
      gateIDs <- matchRes[["gateIDs"]]
      #when length(gateIDs) > 1, then we need to add each gate as a separate pop
      #even though some of these gates do not have GateSet defined in gatingML
      for(i in 1:length(gateIDs)){
        gateID <- gateIDs[i]
        if(i < length(gateIDs)){
          #need to create the new pop that is not currently defined in gatingML GateSets
          thisPopId <- gateID #use gateID as popID, which can not be found in gate_info

          #add new pop
          g <- addNode(thisPopId, g)
          thisGateName <- gateInfo[id == gateID, name]
          nodeData(g, thisPopId, "popName") <- paste0(thisGateName, "_0")

          g <- addEdge(parentID, thisPopId, g)
          g <- addGate(gateInfo,flowEnv, g, thisPopId, gateID)
          parentID <- thisPopId #update the parent
        }else{
          thisPopId <- popId
          g <- addEdge(parentID, thisPopId, g)
          g <- addGate(gateInfo,flowEnv, g, thisPopId, gateID)
        }

      }

    }


  }

  #prune the tree by removing the orphan nodes
#   egs <- edges(g)
#   leaves <- names(egs[sapply(egs, length) == 0])
#   for(node in leaves){
#     if(length(inEdges(node, g)[[1]]) == 0)
#       g <- removeNode(node, g)
#   }
  g
}

findParent <- function(gateSets,thisGateSet, parents_popIds){
  isFound <- FALSE
  for(parentID in parents_popIds)
  {
    pGateSet <- gateSets[[parentID]]
    #check the overlaps between two sets
    if(setequal(intersect(pGateSet, thisGateSet), pGateSet))
    {
      isFound <- TRUE
      #if it is indeed a subset of the current gateSet
      gateIDs <- setdiff(thisGateSet, pGateSet) #parse out the gateID

      # There might be multiple parents due to the duplicated GateSets allowed in gatingML
      # it is not clear which one should be picked and
      # we pick the first one for now

      break
    }
  }
  if(isFound)
    return(list(parentID = parentID, gateIDs = gateIDs))
  else
    return(NULL)

}


addGate <- function(gateInfo,flowEnv, g, popId, gateID){
  #add gate
  sb <- gateInfo[id == gateID, ]
  nGates <- nrow(sb)
  if(nGates > 1)
    stop("multiple gates found for ", gateID)
  #try to find the tailored gate
  tg_sb <- gateInfo[gate_id == sb[, gate_id] & fcs != "", ]


  tg <- sapply(tg_sb[, id], function(gateID){
    flowEnv[[gateID]]
  }, simplify = FALSE)
  names(tg) <- tg_sb[, fcs]
  #     message(popName)
  gate <- flowEnv[[gateID]]
  #parse bounding info
  cytobank_gate_def <- fromJSON(sb[, gate_def])

  #     if(gateID == "Gate_2095590_UEQtMShIaXN0byk.")
  #       browser()

  pname <- parameters(gate)
  if(length(pname) == 1){
    bound <- cytobank_gate_def[["scale"]][c("min", "max")]
    bound <- t(as.matrix(bound))
    rownames(bound) <- pname
  }
  else
    bound <- t(sapply(c("x", "y"), function(axis){
      cytobank_gate_def[["scale"]][[axis]][c("min", "max")]
    }))

  rownames(bound) <- as.vector(parameters(gate))
  nodeData(g, popId, "gateInfo") <- list(list(gate = gate
                                              , gateName = sb[, name]
                                              , fcs = sb[, fcs]
                                              , tailored_gate = tg
                                              , bound = bound
  )
  )
  g
}

#' extend the gate to the minimum and maximum limit of both dimensions based on the bounding information.
#'
#' It is equivalent to the behavior of shifting the off-scale boundary events into the gate boundary that is describled in
#' bounding transformation section of gatingML standard.
#'
#' The advantage of extending gates instead of shifting data are two folds:
#' 1. Avoid the extra computation each time applying or plotting the gates
#' 2. Avoid changing the data distribution caused by adding the gates
#'
#' Normally this function is not used directly by user but invoked when parsing GatingML file exported from Cytobank.
#'
#' @export
#' @param gate a flowCore filter/gate
#' @param bound numeric matrix representing the bouding information parsed from gatingML. Each row corresponds to a channel.
#'        rownames should be the channel names. colnames should be c("min", "max")
#' @param data.range numeric matrix specifying the data limits of each channel. It is used to set the extended value of vertices and must has the same structure as 'bound'.
#'        when it is not supplied, c(-.Machine$integer.max, - .Machine$integer.max) is used.
#' @param limits character whether to plot in "extended" or "original" gate limits. Default is "original".
#' @param ... other arguments
#' @return a flowCore filter/gate
#' @examples
#' library(flowCore)
#' sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol=2,nrow=4)
#' colnames(sqrcut) <- c("FSC-H","SSC-H")
#' pg <- polygonGate(filterId="nonDebris", sqrcut)
#' pg
#' bound <- matrix(c(100,3e3,100,3e3), byrow = TRUE, nrow = 2, dimnames = list(c("FSC-H", "SSC-H"), c("min", "max")))
#' bound
#' pg.extened <- extend(pg, bound, plot = TRUE)
extend <- function(gate, bound, data.range = NULL, plot = FALSE, limits = c("original", "extended"))UseMethod("extend")

#' @export
#' @S3method extend polygonGate
#' @rdname extend
#' @param plot whether to plot the extended polygon.
#' @importFrom flowCore polygonGate
#' @importFrom graphics abline polygon text
extend.polygonGate <- function(gate, bound, data.range = NULL, plot = FALSE, limits = c("original", "extended")){
  limits <- match.arg(limits)
  #linear functions
  f.solve <- list(x = function(x, slope, x1, y1){

                                res <- slope * (x -  x1) + y1
                                names(res) <- "y"
                                res
                              }
            ,y =  function(y, slope, x1, y1){

                                res <- (y -  y1) / slope + x1
                                names(res) <- "x"
                                res
                            }
            )

  #update chnnl names with generic axis names
  chnls <- as.vector(rownames(bound))

  axis.names <- c("x", "y")
  rownames(bound) <- axis.names

  verts.orig <- gate@boundaries
  #fix Inf (from rectangleGate)
  verts.orig[verts.orig==Inf] <- .Machine$integer.max
  verts.orig[verts.orig==-Inf] <- -.Machine$integer.max

  #this is a hack: We assume the polygon is convex
  #if not we turn the convcave into convex
  #to avoid the error
  # verts.orig <- verts.orig[chull(verts.orig),]

  verts <- data.table(verts.orig)
  pname <- as.vector(parameters(gate))
  setnames(verts, colnames(verts), pname)
  ind <- match(pname, chnls)
  new.name <- axis.names[ind]
  setnames(verts, pname, new.name)

  if(is.null(data.range)){
    data.range <- data.frame(min = c(-.Machine$integer.max, - .Machine$integer.max)
                          , max = c(.Machine$integer.max, .Machine$integer.max)
                          , row.names = c("x", "y")
                          )
  }else{
    rname <- rownames(data.range)
    ind <- match(rname, chnls)
    rownames(data.range) <- axis.names[ind]
  }



  #accumulatively update verts by detecting and inserting the intersection points
  #removing off-bound vertex points and inserting extended points
  #for each boundary line
  for(dim in axis.names){ #loop from x to y

    for(bn in colnames(bound)){# loop from min to max

      #extend data.range with gate range in case the later is larger (which will break the assumption of extension logic)
      # if(bn=="min")
      #   data.range[dim, bn] <- min(data.range[dim, bn], min(verts[, ..dim]))
      # else
      #   data.range[dim, bn] <- max(data.range[dim, bn], max(verts[, ..dim]))



      intersect.coord <- bound[dim, bn][[1]]
      names(intersect.coord) <- dim
      nVerts <- nrow(verts)


      # centroid <- colMeans(verts)
      this.intersect <- lapply(1:nVerts, function(i){ #loop through each edge
        #get two vertices
        j <- ifelse(i == nVerts, 1, i + 1)
        #get coordinates
        y2 <- verts[j, y]
        x2 <- verts[j, x]

        y1 <- verts[i, y]
        x1 <- verts[i, x]
        #compute the slope
        slope <- (y2 - y1) / (x2 - x1)
        #get ranges
        rg <- list(x = range(c(x1,x2)), y = range(c(y1,y2)))
        this.rg <- rg[[dim]]
        #       points(x1, y1, col = "green")
        #       points(x2, y2, col = "blue")
        #find inersect point
        # message(i)
#         if(i == 3)

        if(intersect.coord <= this.rg[2]&&intersect.coord >= this.rg[1]){

          #take care of horizontal/vertical edges
          if(is.infinite(slope)){#vertical edge
            if(dim == "x")
              return(NULL) #parallel to vertical bound, no intersect
            else
              point <- c(x = x1, intersect.coord)
          }else if(slope == 0){ #horizontal edge
            if(dim == "y")
              return(NULL)
            else
              point <- c(intersect.coord, y = y1)
          }else{
            #otherwise use linear function to compute the x or y coordinate
            thisFun <- f.solve[[dim]]
            # get the y axis of the intersected point
            res <- thisFun(intersect.coord, slope, x1, y1)
            # browser()
            names(intersect.coord) <- dim
            point <- c(intersect.coord, res)[axis.names]
          }


          c(point, id = i + 0.1)#jitter a positive ammount to break the tie with vert1

        }

      })




      this.intersect.df <- do.call(rbind, this.intersect)

      dim.flip <- ifelse(dim == "x", "y", "x")
#       this.intersect <- this.intersect[order(this.intersect[[dim.flip]]),] #bottom to top/or left to right
      nCount <- ifelse(is.null(this.intersect.df), 0, nrow(this.intersect.df))
      if(nCount > 0){
        if(nCount %% 2 != 0)
          stop("uneven number of intersected points with ", bn, " boundary on ", dim, " axis: ", nCount)
      verts[, id := 1:nVerts]#record the id before insertion
      verts[, key := paste0(x,",", y)] # generate key for the comparsion to intersected points later on

      #reset id for the duplicated intersects so that they won't cause chaos for the ordering later on
      this.intersect.df <- as.data.table(this.intersect.df)
      this.intersect.df[, key := paste0(x,",", y)]
      this.intersect.df[, id := min(id), by = key]
      #order by dim so that extension to pairs follows the order
      setorderv(this.intersect.df, dim.flip)
      inserted.ids <- NULL
      #loop through each pair of extended points
      for(i in seq_len(nCount/2)){
          j <- 2 * i
          # browser()
          this.intersect <- this.intersect.df[(j -1):j, ]

          # this.intersect <- as.data.table(this.intersect)

          #remove the original edge points that overlap with intersect points
          #but keep the intersected points inserted previously
          #nchar(id) : length of significant digits
          #== 1: original points
          #== 2: intersected
          #== 4: extended
          # verts <- verts[!(key %in% this.intersect[, key] && nchar(id) == 1), ]
          verts <- verts[!(key %in% this.intersect[, key]), ]



          #keep track of id that has been inserted repeatly
          #so that the duplicated intersects can be removed later
          #otherwise, it could mess up the already removed off-bound points
          dup.id <- this.intersect[id %in% inserted.ids, id]
          inserted.ids <- unique(c(inserted.ids, this.intersect[, id]))
          #insert the intersect points
          verts <- rbindlist(list(verts, this.intersect))
          verts <- verts[order(id),]

          #remove off-bound points
          #(no longer do it:between the two intersected points
          #because sometime the points fall outside of the range of dim.flip of intersected points
          #breaks the ordering assumption of two extended points)
          vals.dim <- verts[, dim, with = FALSE]
          vals.dim.flip <- verts[, dim.flip, with = FALSE]
          rng.dim.flip  <- range(this.intersect[[dim.flip]])
          # ind.between <- vals.dim.flip <= rng.dim.flip[2] & vals.dim.flip >= rng.dim.flip[1]
          if(bn == "min")
          {
            ind <- vals.dim < intersect.coord #& ind.between
          }else
          {
            ind <- vals.dim > intersect.coord #& ind.between
          }


          ind <- as.vector(ind)
          verts <- verts[!ind, ]



          #add extended points
          this.extend <- this.intersect
          this.extend[, dim] <- data.range[dim, bn]
          this.extend[, key := paste0(x,",", y)]#update key
          #sort by the Id
          this.extend[, is.smaller:= order(id) == 1]
          #check if head-tail node situation
          # by cal the distance between two points
          this.extend[, order := match(id, verts[,id])]
          dist <- abs(diff(this.extend[, order]))
          if(dist == 1){#two consecutive points
            this.extend[is.smaller == TRUE, id := id + 0.01]
            this.extend[is.smaller == FALSE, id := this.extend[is.smaller == TRUE, id] + 0.01]
          }else{
            #deal with head-tail points
            this.extend[is.smaller == TRUE, id := id - 0.01]
            this.extend[is.smaller == FALSE, id := this.extend[is.smaller == TRUE, id] - 0.01]
          }
          # browser()
          this.extend[, is.smaller := NULL]
          this.extend[, order := NULL]

          #remove dup id
          verts <- verts[!id%in%dup.id, ]

          verts <- rbindlist(list(verts, this.extend))
          verts <- verts[order(id),]

        }#end loop for each pair of intersects

        #also make sure to remove the off-bound points outside of any intersected points range
        vals.dim <- verts[, dim, with = FALSE]
        vals.dim.flip <- verts[, dim.flip, with = FALSE]
        rng.dim.flip  <- range(this.intersect.df[[dim.flip]])
        ind.between <- vals.dim.flip <= rng.dim.flip[2] & vals.dim.flip >= rng.dim.flip[1]
        if(bn == "min")
          ind <- vals.dim < intersect.coord & !ind.between
        else
          ind <- vals.dim > intersect.coord & !ind.between
        ind <- as.vector(ind)
        verts <- verts[!ind, ]
      }#end if of ncount>0

    }#end of inner for loop for boundary (from min to max)


  }#end of outer for loop for dim (from x to y)



# if(isTRUE(parent.frame(1)[["popName"]] == "T-helpers"))
#   browser()
  if(plot){
    if(limits == "extended")
      plot(type = "n", x = verts[[1]], y = verts[[2]])
    else
      plot(type = "n", x = verts.orig[,1], y = verts.orig[,2])
    polygon(verts.orig, lwd =  4, border = rgb(0, 0, 0,0.5))
    # points(verts.orig, col = "red")
    # points(t(as.data.frame(colMeans(verts.orig))), col = "red")
    # abline(v = bound[1,], lty = "dashed", col = "red")
    # abline(h = bound[2,], lty = "dashed", col = "red")
    text(verts, labels = verts[, id], col = "red")
    # points(intersect.points[, c(axis.names)], col = "blue")
    polygon(verts, lty = "dotted", border = "green", lwd = 3)
  }


  verts <- verts[, new.name, with = FALSE]
  setnames(verts, new.name, chnls)

  polygonGate(verts)


}

#' @export
#' @S3method extend rectangleGate
#' @rdname extend
#' @importFrom flowCore rectangleGate
extend.rectangleGate <- function(gate, ...){
  pnames <- parameters(gate)
  nParam <- length(pnames)

  if(nParam == 2){
    gate <- fix.rectangleGate(gate)

    extend(as(gate, "polygonGate"), ...)
  }else{
    extend.rectangleGate1d(gate, ...)
  }

}

extend.rectangleGate1d <- function(gate, bound, data.range = NULL)
{

    if(is.null(data.range))
      data.range <- c(min = -.Machine$integer.max, max= .Machine$integer.max)


    if(gate@min <= bound[, "min"][[1]])
      gate@min <- data.range["min"]

    if(gate@max >= bound[, "max"][[1]])
      gate@max <- data.range["max"]



  gate

}
#' @export
#' @S3method extend ellipsoidGate
#' @rdname extend
#' @importFrom flowCore ellipsoidGate
extend.ellipsoidGate <- function(gate, ...){
  #fix the ellipsoidGate parsed from gatingML
  gate <- fix.ellipsoidGate(gate)
  extend(as(gate, "polygonGate"), ...)
}

fix.ellipsoidGate <- function(gate){
  pnames <- as.vector(parameters(gate))
  dimnames(gate@cov) <- list(pnames,pnames)
  names(gate@mean) <- pnames
  gate
}
fix.rectangleGate <- function(gate){
  pnames <- parameters(gate)
  names(gate@min) <- pnames
  names(gate@max) <- pnames
  gate
}
