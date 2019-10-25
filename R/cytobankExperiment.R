#' Construct cytobank_experiment object from ACS file
#' @param acs ACS file exported from Cytobank
#' @param exdir he directory to extract files to
#' @return cytobank_experiment object
#' @importFrom yaml read_yaml
#' @rdname open_cytobank_experiment
#' @export
cytobankExperiment <- function(...){
  .Deprecated("open_cytobank_experiment")
  open_cytobank_experiment(...)
}
#' @rdname open_cytobank_experiment
#' @export
open_cytobank_experiment <- function(acs, exdir = tempfile()){
  message("Unpacking ACS file...")
  unzip(acs, exdir = exdir)
  path <- file.path(exdir, "experiments")
  
  yamlpath<- list.files(path, pattern = "\\.yaml", recursive = TRUE, full.names = TRUE)
  experiment_yaml <- read_yaml(yamlpath)
  expdir <- file.path(path, experiment_yaml$id)
  #rename fcs from id to the actual fcs name
  fcsdir <- file.path(expdir, "fcs_files")
  for(fcs in experiment_yaml$fcsFiles)
  {
    originalId <- fcs$originalId
    from <- file.path(fcsdir, originalId)
    if(!file.exists(from))
      stop(originalId, " missing from ", fcsdir)
    file.rename(from, file.path(fcsdir, fcs$filename))
  }
  #unzip attachment
  attachdir <- file.path(expdir, "attachments")
  att <- list.files(attachdir, full.names = TRUE)
  for(z in att)
  {
    unzip(z, exdir = attachdir)
    file.remove(z)
  }
  gml <- list.files(expdir, pattern = "\\.xml", full.names = TRUE)
  if(length(gml) > 1)
    stop("More than one gatingML files found within ACS")
  if(length(gml) == 0)
    stop("No gatingML files found within ACS")
  structure(list(experiment = experiment_yaml
                        , gatingML = gml
                        , fcsdir = fcsdir
                        , attachments = list.files(attachdir, pattern = "\\.csv$", full.names = TRUE)
                        )
                   , class = "cytobank_experiment")
}
#' @rdname cytobank_to_gatingset
#' @export
cytobank2GatingSet <- function(...)
{
  .Deprecated("cytobank_to_gatingset")
  cytobank_to_gatingset(...)
}
#' @rdname cytobank_to_gatingset
#' @export
cytobank_to_gatingset <- function(x, ...)UseMethod("cytobank_to_gatingset")
#' @importFrom flowWorkspace markernames<-
#' @param ... other arguments
#' @export
#' @method cytobank_to_gatingset cytobank_experiment
#' @rdname cytobank_to_gatingset
cytobank_to_gatingset.cytobank_experiment <- function(x, ...){
  gs <- cytobank_to_gatingset(x$gatingML, list.files(x$fcsdir, full.names = TRUE), ...)
  ce <- x
  pData(gs) <- pData(ce)
  #update markers 
  markers.ce <- markernames(ce)
  cols.ce <- colnames(ce)
  
  for(sn in names(markers.ce))
  {
    #prepare chnl vs marker map for update
    markers.ce <- markers.ce[[sn]]
    names(markers.ce) <- cols.ce
    #filter out NA markers
    gh <- gs[[sn]]
    cols.nonNA <- subset(pData(parameters(gh_pop_get_data(gh, use.exprs = FALSE))), !is.na(desc), "name", drop = TRUE)
    
    markernames(gh) <- markers.ce[cols.nonNA]
  }
  gs
}
setOldClass("cytobank_experiment")

#' @param x cytobank_experiment object
#' @rdname cytobank_experiment
#' @param ... not used
#' @export
#' @method print cytobank_experiment
print.cytobank_experiment <- function(x, ...){
  exp <- x[["experiment"]]
  cat("cytobank Experiment: ", exp[["name"]],"\n");
  cat("gatingML File: ",x[["gatingML"]], "\n");
  
  cat("compensations: ", length(exp$compensations), "\n")
  cat("fcsFiles: ", length(exp$fcsFiles), "\n");
  cat("panels: ", length(exp$panels), "\n");
  cat("scales: ", length(exp$scales), "\n");
}


#' @rdname cytobank_experiment
#' @export
ce_get_cmpensations <- function(x){
  comps <- x[["experiment"]][["compensations"]]
  comp_names <- sapply(comps,`[[`, "name")
  res <- lapply(comps, function(comp.i){
    mat.i<- do.call(rbind, comp.i$matrix)
    colnames(mat.i)   <- comp.i$channelShortNames
    rownames(mat.i) <- comp.i$channelShortNames
    mat.i
  })
  names(res) <- comp_names
  res
}

#' @rdname cytobank_experiment
#' @importFrom flowWorkspace markernames
#' @export
setMethod("markernames",
          signature=signature(object="cytobank_experiment"),
          definition=function(object){
            
            panels <- get_panel_per_file(object)
            res <- lapply(panels, `[[`, "markers")
            if(length(unique(res)) > 1)
            {
              warning("markers are not consistent across samples!")
            }
            res
            
          })

#' @rdname cytobank_experiment
#' @param do.NULL,prefix not used
#' @export
setMethod("colnames",
          signature=signature(x="cytobank_experiment"),
          definition=function(x, do.NULL="missing", prefix="missing"){
            panels <- get_panel_per_file(x)
            res <- lapply(panels, `[[`, "channels")
            if(length(unique(res)) > 1)
              stop("colnames are not consistent across samples!")
            res[[1]]
          })

get_panel_per_file <- function(ce){
  res <- lapply(ce$experiment$fcsFiles, function(sample){
                pairs <- sample[["panel"]][["channels"]]
                list(channels = unlist(lapply(pairs, `[[`, "shortName"))
                     , markers = unlist(lapply(pairs, `[[`, "longName"))
                    )
      
  })
  names(res) <- sampleNames(ce)
  res
}
#' @rdname cytobank_experiment
#' @export
ce_get_transformations <- function(x, ...){
  chnls <- colnames(x)
  low.chnls <- tolower(chnls)
  scales <- x$experiment$scales
  res <- list()
  
  for(scale in scales)
  {
      pname <- scale$channelShortName
      ind <- match(pname, low.chnls)
      if(is.na(ind))
        stop(pname, " not found in colnames of the panel!")
      pname <- chnls[ind]
      stype <- scale$scaleType
      cofactor <- scale$cofactor
      if(stype == "Log")
        res[[pname]] <- logtGml2_trans()
      else if(stype == "Arcsinh")
      {
        T <- sinh(1) * cofactor
        res[[pname]] <- asinhtGml2_trans(T = T)
      }else if(stype != "Linear")
        stop("Unknown scale type: ", stype)
      
  }
  transformerList(names(res), res)
}

#' @rdname cytobank_experiment
#' @export
setMethod("sampleNames","cytobank_experiment",function(object){
  rownames(pData(object))
})

#' @param object cytobank_experiment object
#' @rdname cytobank_experiment
#' @export
setMethod("pData","cytobank_experiment",function(object){
  get_pd(object)
})

#' @importFrom plyr name_rows
get_pd <- function(ce){
  res <- ldply(ce$experiment$fcsFiles, function(sample){
                                c(name = sample[["filename"]]
                                      , unlist(sample[["tags"]])
                                      , .rownames = sample[["filename"]]#sample[["sampleName"]]
                                    )
                      })
  res <- name_rows(res)
  res
}
