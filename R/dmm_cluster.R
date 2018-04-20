## Cluster and state conversion functions for the jDirichletMixtureModels Package

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size N-by-d (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffled A logical. Whether or not to shuffle the data. Default is true.
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
#' A single state from the returned list of states has fields \code{data} and \code{clusters}. \code{data} is a dataframe consisting of the \code{Xdata} and their cluster labels.
#' \code{clusters} is a data.table (is the user has the data.table package loaded) or a list.
#'
#' If clusters is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#'
#' If clusters is a list, each element of the list refers to a clsuter,  clusters[[i]] is a list containing of the above information for
#' cluster i as elements. Each single item in clusters is a list with fields \code{cluster}, \code{population}, and \code{params}. E.g. clusters[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#'
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusters)} function.
#'
#' To see a plot of the labled data in a given state use the \code{dmm.plot(data)} function.
#'
#' @return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#'
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#'
#'
#' @import JuliaCall
#' @export
dmm.cluster <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffled=TRUE){
  UseMethod("dmm.cluster", model)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size N-by-d (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffled A logical. Whether or not to shuffle the data. Default is true.
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
#' A single state from the returned list of states has fields \code{data} and \code{clusters}. \code{data} is a dataframe consisting of the \code{Xdata} and their cluster labels.
#' \code{clusters} is a data.table (is the user has the data.table package loaded) or a list.
#'
#' If clusters is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#'
#' If clusters is a list, each element of the list refers to a clsuter,  clusters[[i]] is a list containing of the above information for
#' cluster i as elements. Each single item in clusters is a list with fields \code{cluster}, \code{population}, and \code{params}. E.g. clusters[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#'
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusters)} function.
#'
#' To see a plot of the labled data in a given state use the \code{dmm.plot(data)} function.
#'
#' @return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#'
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#'
#'
#' @import JuliaCall
#' @export
dmm.cluster.RModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffled=TRUE){
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
  if(is.matrix(Xdata)){
    Xdata=t(Xdata)
  }
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$command("Y = Array{Float64}(Y);")
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("Int64(iters)")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("Int64(burnin)")
  .dmm$julia$assign("shuffled", shuffled)
  # Run cluster code
  if (model$isconjugate) {
    juliastates <- .dmm$julia$eval("dp_cluster(Y, rmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled);")
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
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size N-by-d (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffled A logical. Whether or not to shuffle the data. Default is true.
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
#' A single state from the returned list of states has fields \code{data} and \code{clusters}. \code{data} is a dataframe consisting of the \code{Xdata} and their cluster labels.
#' \code{clusters} is a data.table (is the user has the data.table package loaded) or a list.
#'
#' If clusters is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#'
#' If clusters is a list, each element of the list refers to a clsuter,  clusters[[i]] is a list containing of the above information for
#' cluster i as elements. Each single item in clusters is a list with fields \code{cluster}, \code{population}, and \code{params}. E.g. clusters[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#'
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusters)} function.
#'
#' To see a plot of the labled data in a given state use the \code{dmm.plot(data)} function.
#'
#' @return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#'
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#'
#'
#' @import JuliaCall
#' @export
dmm.cluster.JConjugateModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffled=TRUE){
  # Converting all model functions to julia objects
  .dmm$julia$assign("params",model$params)
  .dmm$julia$command("params=(params...);")
  .dmm$julia$command(paste0("pdf_func=",model$pdf_likelihood,";"))
  .dmm$julia$command(paste0("sample_func=",model$sample_posterior,";"))
  .dmm$julia$command(paste0("marg_func=",model$marginal_likelihood,";"))
  #argstring=paste(model$pdf_likelihood,model$sample_posterior,model$marginal_likelihood,sep=",")
  .dmm$julia$command(paste0("jmodel=GeneralConjugateModel(pdf_func,sample_func,marg_func, params);"))

  # Converting all inputs to julia objects
  if(is.matrix(Xdata)){
    Xdata=t(Xdata)
  }
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$command("Y = Array{Float64}(Y);")
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("iters = Int64(iters);")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("burnin = Int64(burnin);")
  .dmm$julia$assign("shuffled", shuffled)
  # Run cluster code
  juliastates <- .dmm$julia$eval("export_r_all(Y,jmodel,
                                 dp_cluster(Y, jmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
  paramnames <- unlist(.dmm$julia$eval("parameter_names(jmodel);"))
  dmmstates <- dmm.states(juliastates,paramnames)
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size N-by-d (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffled A logical. Whether or not to shuffled the data. Default is true.
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
#' A single state from the returned list of states has fields \code{data} and \code{clusters}. \code{data} is a dataframe consisting of the \code{Xdata} and their cluster labels.
#' \code{clusters} is a data.table (is the user has the data.table package loaded) or a list.
#'
#' If clusters is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#'
#' If clusters is a list, each element of the list refers to a clsuter,  clusters[[i]] is a list containing of the above information for
#' cluster i as elements. Each single item in clusters is a list with fields \code{cluster}, \code{population}, and \code{params}. E.g. clusters[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#'
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusters)} function.
#'
#' To see a plot of the labled data in a given state use the \code{dmm.plot(data)} function.
#'
#' @return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#'
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#'
#'
#' @import JuliaCall
#' @export
dmm.cluster.JNonConjugateModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=4, iters=5000, burnin=200, shuffled=TRUE){
  # Converting all model functions to julia objects
  .dmm$julia$assign("params",model$params)
  .dmm$julia$command("params=(params...);")
  .dmm$julia$command(paste0("pdf_func=",model$pdf_likelihood,";"))
  .dmm$julia$command(paste0("sample_func=",model$sample_prior,";"))
  .dmm$julia$command(paste0("jmodel=NonConjugateModel(pdf_func,sample_func, params);"))

  # Converting all inputs to julia objects
  if(is.matrix(Xdata)){
    Xdata=t(Xdata)
  }
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$command("Y = Array{Float64}(Y);")
  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("iters = Int64(iters);")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("burnin = Int64(burnin);")
  .dmm$julia$assign("shuffled", shuffled)
  .dmm$julia$assign("m_prior", m_prior)
  .dmm$julia$command("m_prior = Int64(m_prior)")
  .dmm$julia$assign("m_post", m_post)
  .dmm$julia$command("m_post = Int64(m_post)")
  juliastates <- .dmm$julia$eval("export_r_all(Y,jmodel,
                                 dp_cluster(Y, jmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
  # Run cluster code
  paramnames <- unlist(.dmm$julia$eval("parameter_names(jmodel);"))
  dmmstates <- dmm.states(juliastates,paramnames)
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param Xdata A 1D array of length N (univariate case) or 2D array of size N-by-d (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha A float. The concentration parameter. Default is 1.0.
#' @param m_prior An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param m_post An integer. Optionally paramter only used in non-conjugate case. Default is 3.
#' @param iters An integer. Number of iterations. Default is 5000.
#' @param burnin An integer. Amount of burn-in. Default is 200.
#' @param shuffled A logical. Whether or not to shuffled the data. Default is true.
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
#' A single state from the returned list of states has fields \code{data} and \code{clusters}. \code{data} is a dataframe consisting of the \code{Xdata} and their cluster labels.
#' \code{clusters} is a data.table (is the user has the data.table package loaded) or a list.
#'
#' If clusters is a data.table, each row refers to a cluster. Columns are the cluster label, the population, and the rest of the columns are parameters.
#'
#' If clusters is a list, each element of the list refers to a clsuter,  clusters[[i]] is a list containing of the above information for
#' cluster i as elements. Each single item in clusters is a list with fields \code{cluster}, \code{population}, and \code{params}. E.g. clusters[[1]]$population is the population of cluster 1. The params field (clusterInfo[[i]]$params)
#' is itself a list of each of the parameters
#'
#' To see a formatted summary of all the clusters in a given state use the \code{dmm.summarize(clusters)} function.
#'
#' To see a plot of the labled data in a given state use the \code{dmm.plot(data)} function.
#'
#' @return A list of states (i.e. \code{state = states[[i]]}). A state is itself a list.
#' A state has two fields: \code{labeledData} and \code{clusterInfo}.
#'
#' \code{labeledData} is a data.frame of the \code{Xdata} data points and their cluster labels.
#' \code{clusterInfo} is either a list or a data.table (if the data.table package is loaded by the user). It conatins
#' (1) cluster labels, (2) the number of data points (i.e. population) of each cluster, and (3) all of the parameters for each cluster.
#'
#'
#' @import JuliaCall
#' @export
dmm.cluster.BaseModel <- function(model, Xdata, alpha=1.0, iters=5000, burnin=200, shuffled=TRUE){
  # Pass data to Julia
  if(is.matrix(Xdata)){
    Xdata=t(Xdata)
  }
  .dmm$julia$assign("Y", Xdata)
  .dmm$julia$command("Y = Array{Float64}(Y);")

  # Create julia model object given name
  # Case: model without params, ie using default parameters
  if (is.null(model$params) & is.null(model$data)){
    strcommand <- paste0("basemodel=",model$model_type,"();")
    # Case: model with params, ie using user defined parameters
  } else if (is.null(model$params)){
    strcommand <- paste0("basemodel=",model$model_type,"(Y);")
  } else {
    .dmm$julia$assign("params", model$params)
    strcommand <- paste0("basemodel=",model$model_type,"(params...);")
  }
  .dmm$julia$command(strcommand)
  # Converting all inputs to julia objects

  .dmm$julia$assign("alpha", alpha)
  .dmm$julia$assign("iters", iters)
  .dmm$julia$command("iters = Int64(iters);")
  .dmm$julia$assign("burnin", burnin)
  .dmm$julia$command("burnin = Int64(burnin);")
  .dmm$julia$assign("shuffled", shuffled)

  # Run cluster code
  juliastates <- .dmm$julia$eval("export_r_all(Y,basemodel,
                                 dp_cluster(Y, basemodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
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
  paramlen=length(juliastates[1][2][1])
  if (length(paramnames) != paramlen){
    paramnames <- sapply(1:paramlen, function(x) paste0("param.",toString(x)))
  }
  # If user is not using data.table package
  if (TRUE){#(!requireNamespace("data.table", quietly = TRUE)) {
    for (i in 1:length(juliastates)){
      states[[i]] <- dmm.state(juliastates[i], paramnames)
    }
  } else {
    # Otherwise use nice data.tables
    for (i in 1:length(juliastates)){
      states[[i]] <- dmm.state.astable(juliastates[i], paramnames)
    }
  }
  return(states)
}

dmm.state <- function(juliastate,paramnames){
  state<-list()
  data=juliastate[1]
  state$data<- data.frame("cluster" = data[,1],"x" = data[,2:ncol(data)])

  clusterinfo=list()
  nclusters=length(juliastate[2])
  for(i in 1:nclusters){
    params=juliastate[2][i]
    names(params) <- paramnames
    attr(params, "class") <- NULL
    clusterinfo[[i]] <- list("cluster"=i, "population"=juliastate[3][i],
                             "params"=params)
  }
  state$clusters <- clusterinfo
  state
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
