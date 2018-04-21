#' Synthetic Gaussian Cluster Data 1
#'
#' Synthetic 2-d data with N=5000 vectors and k=15 Gaussian clusters with different degree of cluster overlapping
#'
#' @docType data
#'
#' @usage data("syntheticGaussian1", package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references P. Fränti and O. Virmajoki, "Iterative shrinking method for clustering problems", Pattern Recognition, 39 (5), 761-765, May 2006.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(syntheticGaussian1)
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"syntheticGaussian1"

#' Synthetic Gaussian Cluster Data 2
#'
#' Synthetic 2-d data with N=5000 vectors and k=15 Gaussian clusters with different degree of cluster overlapping
#'
#' @docType data
#'
#' @usage data("syntheticGaussian2", package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references P. Fränti and O. Virmajoki, "Iterative shrinking method for clustering problems", Pattern Recognition, 39 (5), 761-765, May 2006.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(syntheticGaussian2)
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"syntheticGaussian2"

#' Synthetic Unbalanced Gaussian Cluster Data
#'
#' Synthetic 2-d data with N=6500 vectors and k=8 Gaussian clusters
#'
#' @docType data
#'
#' @usage data("unbalancedClusters", package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references M. Rezaei and P. Fränti, "Set-matching methods for external cluster validity", IEEE Trans. on Knowledge and Data Engineering, 28 (8), 2173-2186, August 2016.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(unbalancedClusters)
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"unbalancedClusters"
