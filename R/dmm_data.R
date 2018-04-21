#' Synthetic Gaussian Cluster Data 1
#'
#' Synthetic 2-d data with N=5000 vectors and k=15 Gaussian clusters with different degree of cluster overlapping
#'
#' @docType data
#'
#' @usage data(syntheticGaussian1, package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references P. Fränti and O. Virmajoki, "Iterative shrinking method for clustering problems", Pattern Recognition, 39 (5), 761-765, May 2006.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(syntheticGaussian1, package = "jDirichletMixtureModels")
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
#' @usage data(syntheticGaussian2, package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references P. Fränti and O. Virmajoki, "Iterative shrinking method for clustering problems", Pattern Recognition, 39 (5), 761-765, May 2006.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(syntheticGaussian2, package = "jDirichletMixtureModels")
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"syntheticGaussian2"


#' Aggregation Data
#'
#' N=788, k=7, D=2
#'
#' @docType data
#'
#' @usage data(aggregationData, package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references A. Gionis, H. Mannila, and P. Tsaparas, Clustering aggregation. ACM Transactions on Knowledge Discovery from Data (TKDD), 2007. 1(1): p. 1-30.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(aggregationData, package = "jDirichletMixtureModels")
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"aggregationData"

#' Birch3 Data
#'
#' Synthetic 2-d data with N=100,000 vectors and k=100 clusters. Random sized clusters in random locations.
#'
#' @docType data
#'
#' @usage data(birch3Data, package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#'
#' @references Zhang et al., "BIRCH: A new data clustering algorithm and its applications", Data Mining and Knowledge Discovery, 1 (2), 141-182, 1997.
#'
#' @source \href{https://cs.joensuu.fi/sipu/datasets/}{Clustering basic benchmark}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(birch3Data, package = "jDirichletMixtureModels")
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"birch3Data"

#' Mouse Data
#'
#' Clusters that look like Mickey mouse's head.
#'
#' @docType data
#'
#' @usage data(mouse, package = "jDirichletMixtureModels")
#'
#' @keywords datasets
#' 
#' @source \href{https://github.com/elki-project/elki/blob/master/data/synthetic/Vorlesung/mouse.csv}{mouse.csv}
#'
#' @examples
#' \dontrun{
#' Xdata <- data(mouse, package = "jDirichletMixtureModels")
#' states <- dmm.cluster(model, Xdata)
#' dmm.plot(states[[1]]$labeledData)
#' }
"mouse"
