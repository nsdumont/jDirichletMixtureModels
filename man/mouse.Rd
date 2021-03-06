% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmm_data.R
\docType{data}
\name{mouse}
\alias{mouse}
\title{Mouse Data}
\format{An object of class \code{matrix} with 500 rows and 2 columns.}
\source{
\href{https://github.com/elki-project/elki/blob/master/data/synthetic/Vorlesung/mouse.csv}{mouse.csv}
}
\usage{
data(mouse, package = "jDirichletMixtureModels")
}
\description{
Clusters that look like Mickey mouse's head.
}
\examples{
\dontrun{
Xdata <- data(mouse, package = "jDirichletMixtureModels")
states <- dmm.cluster(model, Xdata)
dmm.plot(states[[1]]$labeledData)
}
}
\keyword{datasets}
