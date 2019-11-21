#' Methods for interacting with cytobank_experiment objects
#' 
#' These methods mirror similar accessor methods for the \code{GatingSet}
#' class.
#' 
#' @name cytobank_experiment-methods
#' @aliases print.cytobank_experiment
NULL


#' @export
cytobankExperiment <- function(...){
  .Deprecated("open_cytobank_experiment")
  open_cytobank_experiment(...)
}
#' Construct a \code{cytobank_experiment} object from ACS file
#' 
#' @name open_cytobank_experiment
#' @aliases cytobankExperiment
#' @param acs ACS file exported from Cytobank
#' @param exdir he directory to extract files to
#' @return cytobank_experiment object
#' @importFrom yaml read_yaml
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

#' @export
cytobank2GatingSet <- function(...)
{
  .Deprecated("cytobank_to_gatingset")
  cytobank_to_gatingset(...)
}

#' @export
cytobank_to_gatingset <- function(x, ...)UseMethod("cytobank_to_gatingset")


#' @name cytobank_to_gatingset
#' @param panel_id select panel to parse
#' @param ... other arguments
#' @method cytobank_to_gatingset cytobank_experiment
#' @importFrom dplyr filter
#' @importFrom flowWorkspace markernames<-
#' @export
cytobank_to_gatingset.cytobank_experiment <- function(x, panel_id = 1, ...){
  #filter by panel
  ce <- x
  panel <- ce_get_panels(ce)
  pname <- panel[panel_id][["panel"]]
  samples <- ce_get_samples(ce) %>% filter(panel == pname)
  samples <- samples[["sample"]]
  gs <- cytobank_to_gatingset(ce$gatingML, file.path(ce$fcsdir, samples), ...)
  pData(gs) <- pData(ce)[samples, ]
  #update markers 
  markers.ce <- markernames(ce)
  cols.ce <- colnames(ce)
  
  for(sn in names(markers.ce))
  {
    #prepare chnl vs marker map for update
    marker.ce <- markers.ce[[sn]]
    names(marker.ce) <- cols.ce
    #filter out NA markers
    gh <- gs[[sn]]
    cols.nonNA <- subset(pData(parameters(gh_pop_get_data(gh, use.exprs = FALSE))), !is.na(desc), "name", drop = TRUE)
    
    markernames(gh) <- marker.ce[cols.nonNA]
  }
  gs
}
setOldClass("cytobank_experiment")

#' @export
#' @method print cytobank_experiment
print.cytobank_experiment <- function(x, ...){
  exp <- x[["experiment"]]
  cat("cytobank Experiment: ", exp[["name"]],"\n");
  cat("gatingML File: ",x[["gatingML"]], "\n");
  
  # cat("compensations: ", length(exp$compensations), "\n")
  # cat("fcsFiles: ", length(exp$fcsFiles), "\n");
  # cat("panels: ", length(exp$panels), "\n");
  panels <- ce_get_panels(x)
  # cat("scales: ", length(exp$scales), "\n");
  print(as.data.frame(panels))
}

#' Obtain the spillover matrices for the samples in a Cytobank experiment
#' 
#' @name ce_get_compensations
#' @param x A cytobank_experiment object
#' @return A named list of spillover matrices
#' @export
ce_get_compensations <- function(x){
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

#' Obtain counts of the number of samples associated with each marker panel
#' in a Cytobank experiment
#' 
#' @name ce_get_panels
#' @param x \code{cytobank_experiment} object
#' @return A \code{tibble} of panels with sample counts
#' @importFrom dplyr rename count %>%
#' @export
ce_get_panels <- function(x){
  ce_get_samples(x) %>% count(panel) %>% rename(samples = n)
}

#' Obtain a mapping between the samples and marker panels in a Cytobank
#' experiment
#' 
#' @name ce_get_samples
#' @param x A \code{cytobank_experiment} object
#' @return A \code{tibble} with rows containing sample names and their associated
#' panel names
#' @importFrom tibble tibble
#' @export
ce_get_samples <- function(x){
  x <- get_panel_per_file(x)
  x <- unlist(sapply(x, function(i)i[["panel"]]))
  tibble(panel = x, sample = names(x))
}
#' @rdname cytobank_experiment-methods
#' @usage markernames(object)
#' @param object A \code{cytobank_experiment} object
#' @importFrom flowWorkspace markernames
#' @export
setMethod("markernames",
          signature=signature(object="cytobank_experiment"),
          definition=function(object){
            
            panels <- get_panel_per_file(object)
            res <- lapply(panels, `[[`, "markers")
            res <- Filter(Negate(is.null), res)
            
            if(length(unique(res)) > 1)
            {
              warning("markers are not consistent across samples!")
            }
            res
            
          })

#' @rdname cytobank_experiment-methods
#' @usage colnames(object)
#' @export
setMethod("colnames",
          signature=signature(x="cytobank_experiment"),
          definition=function(x, do.NULL="missing", prefix="missing"){
            panels <- get_panel_per_file(x)
            res <- lapply(panels, `[[`, "channels")
            res <- Filter(Negate(is.null), res)
            if(length(unique(res)) > 1)
              stop("colnames are not consistent across samples!")
            res[[1]]
          })

get_panel_per_file <- function(ce){
  res <- lapply(ce$experiment$fcsFiles, function(sample){
                pairs <- sample[["panel"]][["channels"]]
                list( panel = sample[["panel"]][["name"]]
                    , channels = unlist(lapply(pairs, `[[`, "shortName"))
                     , markers = unlist(lapply(pairs, `[[`, "longName"))
                    )
      
  })
  names(res) <- sampleNames(ce)
  res
}

#' Obtain the transformations associated with each channel in a Cytobank experiment
#' 
#' @name ce_get_transformations
#' @param x A \code{cytobank_experiment} object
#' @return A \code{transformerList} object containing \code{transformation} objects for each
#' transformed channel
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

#' @rdname cytobank_experiment-methods
#' @usage sampleNames(object)
#' @export
setMethod("sampleNames","cytobank_experiment",function(object){
  rownames(pData(object))
})

#' @rdname cytobank_experiment-methods
#' @usage pData(object)
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
