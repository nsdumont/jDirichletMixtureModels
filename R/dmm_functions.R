## Functions for Dirichlet Mixture Model Julia Package in R

.dmm <- new.env(parent = emptyenv())

#' Do initial setup for jDirichletMixtureModels package.
#'
#' \code{dmm.setup} does the initial setup for jDirichletMixtureModels package.
#'
#' @param ... arguments passed to \code{JuliaCall::julia_setup}.
#'
#' @examples
#' \dontrun{
#' DMM.setup()
#' }
#'
#' @import JuliaCall
#'
#' @export
dmm.setup <- function(...) {
  # If user did not pass in julia path, try usually macOSx Julia path, if that doesn't work give error
  arguments <- list(...)
  if (length(arguments) != 0){
    .dmm$julia <- JuliaCall::julia_setup(...)
  } else {
    .dmm$julia <- JuliaCall::julia_setup("/Applications/Julia-0.6.app/Contents/Resources/julia/bin/")
  }
  .dmm$julia$install_package_if_needed("DirichletMixtureModels") 
  .dmm$julia$library("DirichletMixtureModels") 
  .dmm$julia$command("using DirichletMixtureModels")
  .dmm$julia$command("importall DirichletMixtureModels")
}


#' Create the model used in the dmm.cluster function
#'
#' If not passed any argurments a multivariate normal conjugate prior will be used as default.
#'
#' @param typename A string. The name of the predefined conjugate prior you wish to use. Options listed below.
#'                 These do not require any \code{params} unless otherwise stated. "MultivariateNormal" is the default.
#'
#' @param pdf_func A function. The probability density function likelihood
#' @param sample_fct A function. The sample posterior function
#' @param marg_fct A function. The marginal likelihood
#'
#' @param filename A string. The name of the .jl file in which Julia functions are located
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood.
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function.
#' @param marg_name  A string. The name of the Julia function in \code{filename} that returns the marginal likelihood.
#'
#' @param params A list of the hyperparameter values used by \code{pdf_func}, \code{sample_func}, and \code{marg_func}.
#'
#' @param isconjugate A logical variable. \code{TRUE} if the user specfied model is conjugate, \code{FALSE} (default) if not.
#'
#' @details Either just the name of one of the availble conjugate priors should be passed as \code{typename} (along with any hyperparamters if needed),
#' or the user can specify their own model.
#'
#' Bulit-in models avaible are:
#'                        "MultivariateNormal" (default),
#'                        "UnivariateNormal",
#'                        "UnivariateNormalKnowSigma",
#'                        "UnivariateExponential".
#'
#' User specified models require three R functions: \code{pdf_fct}, \code{sample_fct}, and \code{marg_fct}
#' (and hyperparameters needed for each of these in \code{params}).
#'
#' Users can specify these functions as Julia functions if desired. To do this pass the filename of the .jl file in which
#' the functions are saved, along with the names of the three functions in the given Julia code.
#'
#' @return A model object which can be passed to \code{dmm.cluster}.
#'
#' @examples
#' \dontrun{
#' # Example of using a multivariate normal conjugate prior (the default)
#' model <- dmm.model()
#'
#' # Example of using a bulit-in conjugate prior
#' model <- dmm.model(typename = "UnivariateNormalKnowSigma", params = list(sigma))
#'
#' # Using a user specified R functions
#'model <- dmm.model(pdf_fct, sample_fct, marg_fct, params, uv=TRUE, isconjugate=TRUE)
#'
#' # Using a user specified Julia functions
#'model <- dmm.model(filename, pdf_name, sample_name, marg_name, params, uv=TRUE, isconjugate=TRUE)
#' }
#'
#' @export
dmm.model <- function(...){
  UseMethod("dmm.model")
}

#' @export 
dmm.model.NULL <- function(...){
  return(.BaseModel())
}

#' Create a model object to be used for dmm.cluster
#' 
#' If a function is passed to dmm.model it will create and return an Rmodel
#' @export  
dmm.model.function <- function(...){
  arguments <- list(...)
  return(.RModel(arguments))
}

#' Create a model object to be used for dmm.cluster
#' 
#' If a string is passed to dmm.model it will create and return either a BaseModel (if its one string),
#' or a JModel for 4 strings
#' @export
dmm.model.character <- function(...){
  arguments <- list(...)
  
  if (length(arguments) == 1){
    return(.BaseModel(arguments))
  } 
  else if (length(arguments) == 4){
    .add_file(arguments[1])
    return(.JModel(arguments[2:4]))
  } 
  else {
    stop("Improper inputs to dmm.model. Either 1 or 4 strings must be passed.")
  }

}

#' Add a Julia file to be accessible.
#' @param filename The name of the Julia file
#' @export
add_file <- function(filename){
  .dmm$julia$command(paste("include(",filename,")"))
}

#' Constructing RModel object: a model based on user specifed R function
.RModel <- function(pdf_func, sample_func, marg_func, params, isconjugate=TRUE){
  model <- list(pdf_likelihood=pdf_func, sample_posterior=sample_func,
                marginal_likelihood=marg_func, params = params, isconjugate=isconjugate)
  attr(model, "class") <- "RModel"
  return(model)
}

#' Constructing JModel object: a model based on user specifed Julia functions
.JModel <- function(pdf_name, sample_name, marg_name, params, isconjugate=TRUE){
  model <- list(pdf_likelihood=pdf_name, sample_posterior=sample_name,
                marginal_likelihood=marg_name, params = params, isconjugate=isconjugate)
  attr(model, "class") <- "JModel"
  return(model)
}

#' Constructing BaseModel object: a conjugate model given by typename
.BaseModel <- function(typename = "MultivariateNormal", params=NULL){
  model <- list(model_type=typename, params = params)
  attr(model, "class") <- "BaseModel"
  return(model)
}


#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size d-by-N (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffle A logical. Whether or not to shuffle the data. Default is true.
#'
#' @details Performs \code{iters} iterations of Algorithm 2 (in conjugate case) or Algorithm 8 (in non-conjugate case) from Neal(2000) to generate possible
#' clusters for the data in \code{Xdata}, using the model in \code{model}, with concentration
#' parameter \code{alpha}. In the 1D case, \code{Xdata} is assumed to be a 1D array of floats. In
#' the 2D case, \code{Xdata} is assumed to be a dxN array of floats, where the data is
#' d-dimensional and N is the number of datapoints.
#' Returns a list of states. The elements of the list are all states 
#' post-burnin iteration, with the default being a \code{burnin} of 200. By default, this
#' array is shuffled so that it may be used to approximate I.I.D draws from the
#' posterior.
#' 
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusterInfo)} function. 
#' 
#'@return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#' 
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#' 
#' If clusterInfo is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#' 
#' If clusterInfo is a list, each element of the list refers to a clsuter,  clusterInfo[[i]] is a list containing of the above information for 
#' cluster i as elements. E.g. clusterInfo[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#' 
#' 
#' @import JuliaCall
#' @export
dmm.cluster <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffle=TRUE){
  UseMethod("dmm.cluster", model)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#' 
#' If using a user specifed model via R functions.
#' @export
dmm.cluster.RModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffle=TRUE){
  # Converting all model functions to julia objects
  .dmm$julia$assign("pdf_func", JuliaObject(model$pdf_likelihood))
  .dmm$julia$assign("sample_func", JuliaObject(model$sample_posterior))
  .dmm$julia$assign("marg_func", JuliaObject(model$marginal_likelihood))
  .dmm$julia$assign("params", JuliaObject(model$params))
  
  if (model$isconjugate){
    .dmm$julia$command("rmodel=GeneralConjugateModel(pdf_func,sample_func,marg_func,params);")
  } else {
    .dmm$julia$command("rmodel=GeneralNonConjugateModel(pdf_func,sample_func,marg_func,params);")
  }
  # Converting all inputs to julia objects
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("Int64(iters)")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("Int64(burnin)")
  .dmm$julia$assign("shuffle", shuffle)
  # Run cluster code
  if (model$isconjugate) {
    juliastates <- .dmm$julia$eval("dp_cluster(Y, rmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffle);")
  } else {
    .dmm$julia$assign("m_prior", m_prior)
    .dmm$julia$command("Int64(m_prior)")
    .dmm$julia$assign("m_post", m_post)
    .dmm$julia$command("Int64(m_post)")
    juliastates <- .dmm$julia$eval("dp_cluster(Y, rmodel, alpha, m_prior=m_prior, m_post=m_post, iters=iters, burnin=burnin, shuffled=shuffle);")
  }
  dmmstates <- dmm.states(juliastates,paramnames)
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#' 
#'If using a user specifed model via Julia functions.
#'@export
dmm.cluster.JModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffle=TRUE){
  # Converting all model functions to julia objects
  .dmm$julia$assign("pdf_func", model$pdf_name)
  .dmm$julia$assign("sample_func", model$sample_name)
  .dmm$julia$assign("marg_func", model$marginal_name)
  .dmm$julia$assign("params", JuliaObject(model$params))
  if (model$isconjugate == TRUE) {
    .dmm$julia$command("jmodel=GeneralConjugateModel(pdf_func,sample_func,marg_func,params);")
  } else {
    .dmm$julia$command("jmodel=GeneralNonConjugateModel(pdf_func,sample_func,marg_func,params);")
  }
  # Converting all inputs to julia objects
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("Int64(iters)")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("Int64(burnin)")
  .dmm$julia$assign("shuffle", shuffle)
  # Run cluster code
  if (model$isconjugate) {
    juliastates <- .dmm$julia$eval("dp_cluster(Y, jmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffle);")
  } else {
    .dmm$julia$assign("m_prior", m_prior)
    .dmm$julia$command("Int64(m_prior)")
    .dmm$julia$assign("m_post", m_post)
    .dmm$julia$command("Int64(m_post)")
    juliastates <- .dmm$julia$eval("dp_cluster(Y, jmodel, alpha, m_prior=m_prior, m_post=m_post, iters=iters, burnin=burnin, shuffled=shuffle);")
  }
  dmmstates <- dmm.states(juliastates,paramnames)
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#' 
#' If using one of the avaible bulit-in models.
#' 
#' 
#' @export
dmm.cluster.BaseModel <- function(model, Xdata, alpha=1.0, iters=5000, burnin=200, shuffle=TRUE){
  # Create julia model object given name
  # Case: model without params, ie using default parameters
  if (is.null(model$params)){
    strcommand <- paste0("basemodel=",model$model_type,"Model();")
  # Case: model with params, ie using user defined parameters
  } else {
    .dmm$julia$assign("params", model$params)
    strcommand <- paste0("basemodel=",model$model_type,"Model(params);")
  }
  .dmm$julia$command(strcommand)
  # Converting all inputs to julia objects
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("iters = Int64(iters);")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("burnin = Int64(burnin);")
  .dmm$julia$assign("shuffle", shuffle)
  
  # Run cluster code
  juliastates <- .dmm$julia$eval("dp_cluster(Y, basemodel, alpha, iters=iters, burnin=burnin, shuffled=shuffle);")
  # Get labels/names of parameters if they exist
  paramnames <- unlist(.dmm$julia$eval("parameter_names(basemodel);"))
  
  dmmstates <- dmm.states(juliastates,paramnames)
  return(dmmstates)
}

#' Constructing list of all states from dmm.cluster run (excluding burnin).
#' 
#' @param juliastates The object returned by Julia code
#' @param paramnames Optionally. A list of the parameter names. Returned by Julia code for most bulit-in models.
#' 
#' @details Each item in the list (i.e. \code{state = states[[i]]}) is a state. A state is also a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#' 
#' \code{labeledData} is a data.frame of the data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#' 
#' If clusterInfo is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#' 
#' If clusterInfo is a list, each element of the list refers to a clsuter,  clusterInfo[[i]] is a list containing of the above information for 
#' cluster i as elements. E.g. clusterInfo[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#' 
#' 
#' @import JuliaCall
dmm.states <- function(juliastates, paramnames=NULL){
  states <- list()
  # If user is not using data.table package
  if (!requireNamespace("data.table", quietly = TRUE)) {
    for (i in 1:length(juliastates)){
      states[[i]] <- dmm.state(juliastates[i], paramnames)
    }
  } else {
    # Otherwise use nice data.tables
    for (i in 1:length(juliastates)){
      states[[i]] <- dmm.stateAsTable(juliastates[i], paramnames)
    }
  }
  return(states)
}

#' Formats a single DMM OutputState Julia object into a list of labeled data and list of cluster info
#' @import JuliaCall
dmm.state <- function(juliastate, paramnames=NULL){
  paramlen <- length(field(juliastate,"phi")[1])
  nums <- length(field(juliastate,"n"))
  
  # If any paramters names don't exist (and instead paramter_names returns NULL, which will not appear when
  # unlisted and this paramnames won't be full size) then don't use them
  if (length(paramnames) != paramlen){
    paramnames <- sapply(1:nums, function(x) paste0("param.",toString(x)))
  }
  
  labeledX <- data.frame("x" = field(juliastate,"data"), 
                         "cluster" = field(juliastate,"labels"))
  
  parameters <- list()
  clusterInfo <- list()
  for (k in 1:nums){
    parameters[[k]] <- field(juliastate,"phi")[k]
    attr(parameters[[k]], "class") <- NULL
    names(parameters[[k]]) <- paramnames
    clusterInfo[[k]] <- list("cluster"=k, "population"=field(juliastate,"n")[k],
                             "params"=parameters[[k]])
  }
  
  state <- list(labeledData=labeledX, clusterInfo = clusterInfo)
  return(state)
}

#' Formats a single DMM OutputState Julia object into a list of labeled data and data.table of cluster info
#' @import JuliaCall
dmm.stateAsTable <- function(juliastate, paramnames=NULL){
    paramlen <- length(field(juliastate,"phi")[1])
    nums <- length(field(juliastate,"n"))
    
    # If any paramters names don't exist (and instead paramter_names returns NULL, which will not appear when
    # unlisted and this paramnames won't be full size) then don't use them
    if (length(paramnames) != paramlen){
      paramnames <- sapply(1:nums, function(x) paste0("param.",toString(x)))
    }
    
    labeledX <- data.frame("x" = field(juliastate,"data"), 
                            "cluster" = field(juliastate,"labels"))
  
    parameters <- list()
    for (j in 1:paramlen){
      parameters[[j]] <- field(juliastate,"phi")[1:nums][j]
      attr(parameters[[j]], "class") <- NULL
    }
    clusterInfo <- data.table("cluster" = 1:nums, "population" = field(juliastate,"n"))
    clusterInfo[,(paramnames) := parameters]
    
    state <- list(labeledData=labeledX, clusterInfo = clusterInfo)
    return(state)
}


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
      if (is.atomic(unlist(clusterInfo[,j,with=FALSE][i]))){
        print(sprintf("      %s: %f ", colnames(clusterInfo)[j], unlist(clusterInfo[,j,with=FALSE][i])))
      } else {
        print(sprintf("      %s: ", colnames(clusterInfo)[j]))
        print(clusterInfo[,j,with=FALSE][i])
      }
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
      if (is.atomic(clusterInfo[[i]]$params[[j]])){
        print(sprintf("      %s: %f ", names(clusterInfo[[i]]$params)[j], clusterInfo[[i]]$params[[j]]))
      } else {
        print(sprintf("      %s: ", names(clusterInfo[[i]]$params)[j]))
        print(clusterInfo[[i]]$params[[j]])
      }
    }
  }
} 

#' 
#' @export
dmmm.plot <- function(labeledData){
  if (ncol(labeledData) == 3){
    # 2D plot
    # If user has ggplot2, use it
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot( labeledData, aes(x=x.1, y=x.2 ) ) +
        geom_point(aes(colour = factor(cluster)),alpha=0.8) +
        xlab(expression(x[1])) + 
        ylab(expression(x[2])) 
      p$labels$colour <- "Cluster"
      p
    # Otherwise a normal plot
    } else {
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      plot(labeledData[,1:2], col=labeledData$cluster, pch=19,
           ylab=expression(x[2]),
           xlab=expression(x[1]), bty="L")
      legend("right",inset=c(-1,0),legend = as.character(1:length(unique(labeledData$cluster))),
             title = "Cluster",
             col=1:length(unique(labeledData$cluster)),pch=19)
    }
  } else if (ncol(labeledData) == 1){
    # 1D plot
    par(yaxt = "n", bty = "n")
    stripchart(labeledData[,1], col=labeledData$cluster, xlab=expression(x[1]))
  } else if (ncol(labeledData) == 4){
    # 3D plot
    if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
      scatterplot3d(labeledData[,1:3], pch = 16, color=labeledData$cluster,
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

