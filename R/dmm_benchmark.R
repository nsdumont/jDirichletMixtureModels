#' To get MCMC computation times
#' 
#' This function is the same as \code{dmm.cluster} except instead of returning the states it returns the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
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
#' Returns a dataframe of the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @return A dataframe of the time in seconds it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @import JuliaCall
#' @import tictoc
#' @export
dmm.benchmark <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffled=TRUE){
  UseMethod("dmm.benchmark", model)
}




#' To get MCMC computation times
#' 
#' This function is the same as \code{dmm.cluster} except instead of returning the states it returns the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
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
#' Returns a dataframe of the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @return A dataframe of the time in seconds it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @import JuliaCall
#' @import tictoc
#' @export
dmm.benchmark.JConjugateModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=3, iters=5000, burnin=200, shuffled=TRUE){
  tic()
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
  pre=toc()
  tic()
  # Run cluster code
  juliastates <- .dmm$julia$eval("export_r_all(Y,jmodel,
                                 dp_cluster(Y, jmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
  comp=toc()
  tic()
  paramnames <- unlist(.dmm$julia$eval("parameter_names(jmodel);"))
  dmmstates <- dmm.states(juliastates,paramnames)
  post=toc()
  out=list()
  out$pre=(pre$toc-pre$tic)
  out$comp=(comp$toc-comp$tic)
  out$post=(post$toc-post$tic)
  out
}


#' To get MCMC computation times
#' 
#' This function is the same as \code{dmm.cluster} except instead of returning the states it returns the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
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
#' Returns a dataframe of the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @return A dataframe of the time in seconds it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @import JuliaCall
#' @import tictoc
#' @export
dmm.benchmark.JNonConjugateModel <- function(model, Xdata, alpha=1.0, m_prior=3, m_post=4, iters=5000, burnin=200, shuffled=TRUE){
  tic()
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
  pre=toc()
  tic()
  juliastates <- .dmm$julia$eval("export_r_all(Y,jmodel,
                                 dp_cluster(Y, jmodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
  comp=toc()
  tic()
  # Run cluster code
  paramnames <- unlist(.dmm$julia$eval("parameter_names(jmodel);"))
  dmmstates <- dmm.states(juliastates,paramnames)
  post=toc()
  out=list()
  out$pre=(pre$toc-pre$tic)
  out$comp=(comp$toc-comp$tic)
  out$post=(post$toc-post$tic)
  out
}


#' To get MCMC computation times
#' 
#' This function is the same as \code{dmm.cluster} except instead of returning the states it returns the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
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
#' Returns a dataframe of the time it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @return A dataframe of the time in seconds it took to do 
#' preprocessing computations, the MCMC computation, and the postprocessing computations.
#' 
#' @import JuliaCall
#' @import tictoc
#' @export
dmm.benchmark.BaseModel <- function(model, Xdata, alpha=1.0, iters=5000, burnin=200, shuffled=TRUE){
  tic()
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
  pre = toc()
  tic()
  # Run cluster code
  juliastates <- .dmm$julia$eval("export_r_all(Y,basemodel,
                                 dp_cluster(Y, basemodel, alpha, iters=iters, burnin=burnin, shuffled=shuffled));")
  comp=toc()
  tic()
   # Get labels/names of parameters if they exist
  paramnames <- unlist(.dmm$julia$eval("parameter_names(basemodel);"))

  dmmstates <- dmm.states(juliastates,paramnames)
  post=toc()
  out=data.frame("pre-computation time"=(pre$toc-pre$tic),
                 "computation time"=(comp$toc-comp$tic),
                 "post-computation time"=(post$toc-post$tic))
}
