## Display functions (summarize and plot) for the jDirichletMixtureModels Package

#' Summerize given a single state's cluster info
#'
#' @param clusterInfo A list or data.table. Given states <- dmm.cluster(...), this input is states$clusterInfo
#'
#' @export
dmm.summarize <- function(clusterInfo){
  UseMethod("dmm.summarize")
}

#' @export
dmm.summarize.data.table <- function(clusterInfo){
  print(sprintf("%d clusters were found.", nrow(clusterInfo)))
  for (i in clusterInfo$cluster){
    print(sprintf("   Cluster %d: ", i))
    print(sprintf("      Contains %d data points. ", clusterInfo$population[i]))
    for (j in 3:ncol(clusterInfo)){
      print(sprintf("      %s: ", colnames(clusterInfo)[j]))
      print(clusterInfo[,j,with=FALSE][i])
    }
  }
}

#' @export
dmm.summarize.list <- function(clusterInfo){
  print(sprintf("%d clusters were found.", length(clusterInfo)))
  for (i in 1:length(clusterInfo)){
    print(sprintf("   Cluster %d: ", i))
    print(sprintf("      Contains %d data points. ", clusterInfo[[i]]$population))
    for (j in 1:length(clusterInfo[[i]]$params)){
        print(sprintf("      %s: ", names(clusterInfo[[i]]$params)[j]))
        print(clusterInfo[[i]]$params[[j]])
    }
  }
}

#' Plot labeledData for a state returned by dmm
#'
#' @usage \code{ states <- dmm.cluster(model,Xdata,...)  }
#' \code{ dmm.plot(states[[1]]$data)  }
#'
#' Given the data from a single state returned by dmm.cluster(...), plot it. Can do 2D, 1D, or 3D plots.
#' \code{ggplot2} recommanded for 2D plots. \code{scatterplot3d} requried for 3D plots.
#'
#' @param labeledData The data from a single state. A single state is one item of the list of states deturned by mm.cluster(...).
#'
#' @export
dmm.plot <- function(labeledData){
  if (ncol(labeledData) == 3){
    # 2D plot
    # If user has ggplot2, use it
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot( labeledData, aes(x=x.1, y=x.2 ) ) +
        geom_point(aes(colour = factor(cluster)),alpha=0.6,size = 0.1, stroke = 2, shape = 16) +
        xlab(expression(x[1])) +
        ylab(expression(x[2])) +
        theme(legend.position="none")
      p
      # Otherwise a normal plot
    } else {
      plot(labeledData[,2:3], col=labeledData$cluster, pch=16,
           ylab=expression(x[2]),
           xlab=expression(x[1]), bty="L")
    }
  } else if (ncol(labeledData) == 2){
    # 1D plot
    xplot <- data.frame(labeledData[,2],1)
    stripchart(xplot, col=labeledData$cluster, xlab=expression(x[1]),pch=19, ylab = '',yaxt='n',bty="n")
  } else if (ncol(labeledData) == 4){
    # 3D plot
    if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
      scatterplot3d(labeledData[,2:4], pch = 16, color=labeledData$cluster,
                    xlab = expression(x[1]),
                    ylab = expression(x[2]),
                    zlab = expression(x[3]))
    } else {
      stop("Need package scatterplot3d for 3d plots.")
    }
  } else {
    stop("Dimensionality of data too high to plot.")
  }
}
