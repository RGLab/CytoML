% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cytobank2GatingSet.R, R/cytobankExperiment.R
\name{cytobank_to_gatingset.default}
\alias{cytobank_to_gatingset.default}
\alias{cytobank2GatingSet}
\alias{cytobank_to_gatingset}
\alias{cytobank_to_gatingset.cytobank_experiment}
\title{A wrapper that parse the gatingML and FCS files (or cytobank_experiment object) into GatingSet}
\usage{
\method{cytobank_to_gatingset}{default}(x, FCS, ...)

cytobank2GatingSet(...)

cytobank_to_gatingset(x, ...)

\method{cytobank_to_gatingset}{cytobank_experiment}(x, ...)
}
\arguments{
\item{x}{the cytobank_experiment object or the full path of gatingML file}

\item{FCS}{FCS files to be loaded}

\item{...}{other arguments}
}
\value{
a GatingSet
}
\description{
A wrapper that parse the gatingML and FCS files (or cytobank_experiment object) into GatingSet
}
\examples{
\dontrun{
acsfile <- system.file("extdata/cytobank_experiment.acs", package = "CytoML")
ce <- open_cytobank_experiment(acsfile)
xmlfile <- ce$gatingML
fcsFiles <- list.files(ce$fcsdir, full.names = TRUE)
gs <<- cytobank_to_gatingset(xmlfile, fcsFiles)
library(ggcyto)
autoplot(gs[[1]])
}
}