xmlElementsByTagName <- function(...)suppressWarnings(XML::xmlElementsByTagName(...))
compact <- flowWorkspace:::compact

#' the parameter range from the flow data associated with GatingHierarchy
#' @param ... GatingHierarchy object
#' @param na.rm not used
#' @param type character of "instrument" or "data" indicating whether to retrieve the instrument or the actual data range
#' @param raw.scale logical whether convert the range from transformed scale to raw scale
#' @return matrix
#' @examples
#' \dontrun{
#'  range(gh, type = "data")#return data range
#'  range(gh) #return instrument range
#'  range(gh, raw.scale = TRUE) #inverse transform the range to the raw scale
#' }
range.GatingHierarchy <- function(..., na.rm = FALSE, type = c("instrument", "data"), raw.scale = FALSE){
  type <- match.arg(type)
  gh <- list(...)[[1]]
  fr <- getData(gh, use.exprs = type == "data")
  rng <- range(fr, type = type)
  if(raw.scale)
  {
    chnls <- names(rng)
    translist <- getTransformations(gh, only.function = FALSE)
    for(chnl in chnls)
    {
      trans <- translist[[chnl]]
      if(!is.null(trans))
      {
        rng[, chnl] <- trans$inverse(rng[, chnl])
      }
    }
  }
  rng

}

is.cytof <- function(gs){
  any(grepl("Event_length", colnames(gs)))
}
######################################
#common APIs related to processing comp, trans, gates
#to prepare the GatingML output that can be shared by both cytobank and flowJo modules
######################################
#' @importFrom flowWorkspace getData
#' @importFrom flowCore compensation identifier identifier<- compensatedParameter asinhtGml2 logicletGml2
export_comp_trans <- function(gs, flowEnv, cytobank.default.scale = FALSE, type = c("cytobank", "flowJo"))
{
  type <- match.arg(type)
  #parse comp and channel names
  comp <- getCompensationMatrices(gs[[1]])#assuming the comp is identical across samples
  if(is.null(comp)){
    #no compensation and channel names in transformation are not prefixed
    chnls <- colnames(getData(gs))
    prefix_chnls_orig <- chnls
  }else{
    chnls <- as.vector(parameters(comp))
    #retrieve the prefix for latter trans matching
    cmp <- flowWorkspace:::.cpp_getCompensation(gs@pointer, sampleNames(gs)[[1]])
    prefix <- cmp$prefix
    suffix <- cmp$suffix
    prefix_chnls_orig <- paste0(prefix, chnls, suffix)

  }


  #add comp
  if(!is.null(comp)){

    if(type == "cytobank"){
      #prefix the channels since cytobank expect that
      prefix_chnls <- paste0("Comp_", chnls)
      rownames(comp@spillover) <- prefix_chnls #change fluorochromes and leave detector(colnames) unchanged
    }else{
      #prefix the channels
      prefix_chnls <- paste0("Comp-", chnls)
      # prefix_chnls <- chnls #flowJo use the raw channel name
      # rownames(comp@spillover) <- prefix_chnls #change fluorochromes
      # markers <- markernames(gs)
      # rownames(comp@spillover) <- markers #change detector to marker names because flowJo seems to expect that
    }


    comp <- compensation(comp@spillover)

    compId <- identifier(comp)
    compId <- paste("Spill", compId, sep = "_")
    identifier(comp) <- compId

    flowEnv[[compId]] <- comp
  }else{
    compId <- "FCS"
    prefix_chnls <- chnls
  }




  #add trans (assume it is identical across samples)
  trans <- getTransformations(gs[[1]], only.function = FALSE)

  if(length(trans) == 0)
    stop("no transformation is found in GatingSet!")
  trans.Gm2objs <- list()
  rescale.gate <- FALSE
  for(transName in names(trans)){
    trans.obj <- trans[[transName]]
    type <- trans.obj[["name"]]
    # ind <- sapply(chnls, grepl, x = transName, USE.NAMES = FALSE)
    ind <- prefix_chnls_orig == transName#do strict match due to the possible mismatch for cases like CD3 vs CD33
    nMatched <- sum(ind)
    if(nMatched > 1)
      stop("More than one channels matched to transformation: ", transName)
    else if(nMatched == 1){
      trans.func <- trans.obj[["transform"]]
      chnl <- chnls[ind]
      prefix_chnl <- prefix_chnls[ind]
      prefix_chnl_orig <- prefix_chnls_orig[ind]

      # if(is.null(comp))
      #   param.obj <- prefix_chnl
      # else
        param.obj <- compensatedParameter(prefix_chnl
                           , spillRefId = compId
                           , searchEnv = flowEnv
                           , transformationId = prefix_chnl)

      if(type == "asinhtGml2"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_Arcsinh_", prefix_chnl)


        flowEnv[[transID]] <- asinhtGml2(parameters = param.obj
                                     , M = env[["m"]]
                                     , T = env[["t"]]
                                     , A = env[["a"]]
                                     , transformationId = transID
                                    )
      }else if(type == "logicleGml2"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_logicleGml2_", prefix_chnl)
        flowEnv[[transID]] <- logicletGml2(parameters = param.obj
                                     , M = env[["M"]]
                                     , T = env[["T"]]
                                     , A = env[["A"]]
                                     , W = env[["W"]]
                                     , transformationId = transID
                                    )
      }else if(type == "logicle"){
        #extract parameters
        env <- environment(trans.func)
        transID <- paste0("Tr_logicle_", prefix_chnl)
        flowEnv[[transID]] <- logicletGml2(parameters = param.obj
                                        , M = env[["m"]]
                                        , T = env[["t"]]
                                        , A = env[["a"]]
                                        , W = as.vector(env[["w"]])
                                        , transformationId = transID
                                        )
        rescale.gate <- TRUE
      }else if(type %in% c("flowJo_biexp", "flowJo_fasinh")){
        transID <- paste0("Tr_logicle_", prefix_chnl)
        param <-  attr(trans.func,"parameters")

        flowEnv[[transID]] <- logicletGml2(parameters = param.obj
                                           , M = param[["pos"]]
                                           , T = param[["maxValue"]]
                                           , A = param[["neg"]]
                                           , W = log10(-param[["widthBasis"]]) / 2
                                           , transformationId = transID
        )

        rescale.gate <- TRUE
      }else if(type == "flowJo_flog"){
        env <- environment(trans.func)
        transID <- paste0("Tr_flog_", prefix_chnl)
        flowEnv[[transID]] <- logtGml2(parameters = param.obj
                                           , M = 1
                                           , T = 1
                                           , transformationId = transID
                                      )
        rescale.gate <- TRUE
      }else{
        # browser()
        stop("unsupported trans: ", type)
      }




    if(cytobank.default.scale){
      rescale.gate <- TRUE
      #overwrite the customed scale with default one
      flowEnv[[transID]] <- asinhtGml2(parameters = param.obj
                                       , M = 0.43429448190325176
                                       , T = ifelse(is.cytof(gs), 5.8760059682190064, 176.2801790465702)
                                       , A = 0.0
                                       , transformationId = transID)
      }

    #save another copy of trans.obj in the list
    trans.Gm2objs[[prefix_chnl_orig]] <- flowEnv[[transID]]
    }
  }
  return(list(trans.Gm2objs = trans.Gm2objs, trans = trans, compId = compId))
}

#' @importFrom flowCore eval
processGate <- function(gate, gml2.trans, compId, flowEnv, rescale.gate = FALSE, orig.trans){

  params <- as.vector(parameters(gate))
  prefixed_chnls_orig <- names(gml2.trans)

  for(i in seq_along(params)){
    param <- params[i]
    # ind <- sapply(chnls, function(chnl)grepl(chnl, param), USE.NAMES = FALSE)
    ind <- prefixed_chnls_orig == param
    nMatched <- sum(ind)
    if(nMatched == 0){

      chnl <- gate@parameters[[i]]@parameters
      #can't use "uncompensated" because it will cause multiple entries when paring gate back into openCyto through cytobank2GatingSet
      gate@parameters[[i]] <- compensatedParameter(chnl
                                                   , spillRefId = compId #"uncompensated"
                                                   , searchEnv = flowEnv
                                                   , transformationId = chnl
                                                   )
    }else if(nMatched == 1){
      prefixed_chnl_orig <- prefixed_chnls_orig[ind]
      orig.trans.obj <- orig.trans[[which(ind)]]
      gml2.trans.obj <- gml2.trans[[prefixed_chnl_orig]]
      if(rescale.gate){
        inv.fun <- orig.trans.obj[["inverse"]]
        trans.fun <- eval(gml2.trans.obj)
        #rescale
        gate <- transform_gate(gate, inv.fun, param)
        gate <- transform_gate(gate, trans.fun, param)
      }


      #attach trans reference
      gate@parameters[[i]] <- gml2.trans.obj
    }else if(nMatched > 1)
      stop("multiple trans matched to :", param)
  }

  # round
  gate

}


inverse <- function(gate, boundary){
  UseMethod("inverse")
}

inverse.polygonGate <- function(gate, ...){
  gate@boundaries <- inverse.polygon(gate@boundaries, ...)
  gate
}
inverse.ellipsoidGate <- function(gate, ...){
  inverse(as(gate, "polygonGate"), ...)
}
inverse.rectangleGate <- function(gate, ...){
  param <- as.vector(parameters(gate))
  if(length(param) == 1){
    stop("to be implemented!")
  }else
    inverse(as(gate, "polygonGate"), ...)
}

inverse.polygon <- function(mat, boundary){


  dims <- colnames(mat)
  stopifnot(all(dims %in% colnames(boundary)))

  #enlarge the boundary to ensure the negated polygon includes all events outside of original polygon
  boundary["min", ] <- boundary["min", ] - 1e4
  boundary["max", ] <- boundary["max", ] + 1e4

  x <- dims[1]
  y <- dims[2]

  #find the top most point as starting point
  ind <- which.max(mat[, y])
  #reorder mat starting from top most
  nRow <- nrow(mat)
  orig.seq <- seq_len(nRow)
  seq1 <- ind:nRow
  seq2 <- setdiff(orig.seq, seq1)
  new.seq <- c(seq1, seq2)
  new.seq
  mat.new <- mat[new.seq, ]


  mat.inv <- mat.new[1, ]
  #extend to upper bound
  pt1 <- c(mat.new[1, x], boundary["max", y])
  mat.inv <- rbind(mat.inv, pt1, deparse.level = 0)
  #left top
  mat.inv <- rbind(mat.inv, c(boundary["min", x], boundary["max",y]))
  #left bottom
  mat.inv <- rbind(mat.inv, c(boundary["min", x], boundary["min",y]))
  #right bottom
  mat.inv <- rbind(mat.inv, c(boundary["max", x], boundary["min",y]))
  #right top
  mat.inv <- rbind(mat.inv, c(boundary["max", x], boundary["max",y]))
  #back to first extended point
  mat.inv <- rbind(mat.inv, pt1, deparse.level = 0)
  #connect to the remain mat.new in reverse order
  mat.inv <- rbind(mat.inv, mat.new[rev(seq_len(nRow)[-1]), ])

#   plot(mat.inv, type = "n")
#   polygon(mat.new, col = "red")
#   polygon(mat.inv, col = "blue")
  mat.inv
}
