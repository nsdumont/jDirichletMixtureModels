## Functions for Dirichlet Mixture Model Julia Package in R

.dmm <- new.env(parent = emptyenv())

#' Do initial setup for jDirichletMixture package.
#'
#' \code{dmm.setup} does the initial setup for jDirichletMixture package.
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
  .dmm$julia <- try(JuliaCall::julia_setup(...),silent=FALSE)
   if (class(.dmm$julia) == "try-error"){
     .dmm$julia <- JuliaCall::julia_setup("/Applications/Julia-0.6.app/Contents/Resources/julia/bin/")
   }
  .dmm$julia$install_package_if_needed("DirichletMixtureModels")
  .dmm$julia$library("DirichletMixtureModels")
  #.dmm$julia$source(system.file("julia/DirichletMixtureModels.jl", package = "jDirichletMixture"))
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
#' @param uv A logical variable. \code{TRUE} (default) if model is univariate,\code{FALSE} if not.
#' @param isconjugate A logical variable. \code{TRUE} if the user specfied model is conjugate, \code{FALSE} (default) if not.
#'                    
#' @details Either just the name of one of the availble conjugate priors should be passed as \code{typename} (along with any hyperparamters if needed),
#' or the user can specify their own model.
#' 
#' Bulit-in models avaible are:
#'                        "MultivariateNormal" (default),
#'                        "UnivariateNormal",
#'                        "UnivariateNormalKnowSigma" (requires \code{params} = sigma),
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

#' Create a model object to be used for dmm.cluster
#' If a function is passed to dmm.model it will create and return an Rmodel 
dmm.model.function <- function(...){
  return(.RModel(...))
}

#' Create a model object to be used for dmm.cluster
#' If a string is passed to dmm.model it will create and return either a BaseModel (if its one string),
#' or a JModel for 4 strings
dmm.model.character <- function(...){
  argvals <- list(...)
  
  if (length(argvals) == 1){
    return(.BaseModel(...))
  } 
  else if (length(argvals) == 4){
    .add_file(argvals[1])
    return(.JModel(argvals[2:4]))
  } 
  else {
    stop("Improper inputs to dmm.model. Either 1 or 4 strings must be passed.")
  }
  
}

#' Including a user given file of Julia functions 
#' @param filename The name of the Julia file
.add_file <- function(filename){
  julia_command(paste("include(",filename,")"))
}

#' Constructing RModel object: a model based on user specifed R function
.RModel <- function(pdf_func, sample_func, marg_func, params, uv=TRUE, isconjugate=TRUE){
  model <- list(pdf_likelihood=pdf_func, sample_posterior=sample_func, 
                marginal_likelihood=marg_func, params = params, univariate=uv, isconjugate=isconjugate)
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
.BaseModel <- function(typename, params=NULL, uv=TRUE){
  model <- list(model_type=typename, params = params, univariate=uv)
  attr(model, "class") <- "BaseModel"
  return(model)
}


#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'
#' @param model An object returned by \code{dmm.model()}.
#' @param data A 1D array of length N (univariate case) or 2D array of size d-by-N (mulitvariate case),
#'             where d is the dimensionailty of the data and N is the number of observations.
#' @param alpha 
#' @param iters An integer. Number of iterations.
#' @param burnin An integer. Amount of burn-in.
#' @param shuffle A logical. Whether or not to shuffle the data.
#'
#' @export
dmm.cluster <- function(model, data, alpha=1.0, iters=5000, burnin=200, shuffle=TRUE){
  UseMethod("dmm.cluster", model)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#' If using a user specifed model via R functions.
dmm.cluster.RModel <- function(model, data, alpha=1.0, iters=5000, burnin=200, shuffle=TRUE){
  julia$assign("pdf_func", JuliaObject(model$pdf_likelihood))
  julia$assign("sample_func", JuliaObject(model$sample_posterior))
  julia$assign("marg_func", JuliaObject(model$marginal_likelihood))
  julia$assign("params", JuliaObject(model$params))
  if (model$univariate == TRUE) {
    julia$command("rmodel=GeneralUnivariateConjugateModel(pdf_func,sample_func,marg_func,params)")
  }else{
    julia$command("rmodel=GeneralMultivariateConjugateModel(pdf_func,sample_func,marg_func,params)")
  }
  julia$assign("Y", data)
  julia$assign("alpha", alpha)
  julia$assign("iters", iters)
  julia$assign("shuffle", shuffle)
  dmmstates <- julia$eval("dp_cluster(Y, rmodel, alpha, iters, shuffle)")
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#'If using a user specifed model via Julia functions.
dmm.cluster.JModel <- function(model, data, alpha=1.0, iters=5000, burnin=200, shuffle=TRUE){
  julia$assign("pdf_func", model$pdf_name)
  julia$assign("sample_func", model$sample_name)
  julia$assign("marg_func", model$marginal_name)
  julia$assign("params", JuliaObject(model$params))
  if (model$univariate == TRUE) {
    julia$command("rmodel=GeneralUnivariateConjugateModel(pdf_func,sample_func,marg_func,params)")
  }else{
    julia$command("rmodel=GeneralMultivariateConjugateModel(pdf_func,sample_func,marg_func,params)")
  }
  julia$assign("Y", data)
  julia$assign("alpha", alpha)
  julia$assign("iters", iters)
  julia$assign("shuffle", shuffle)
  dmmstates <- julia$eval("dp_cluster(Y, rmodel, alpha, iters, shuffle)")
  return(dmmstates)
}

#' Use a Dirichlet Mixture Model on data to get cluster labels and cluster parameter values.
#' If using one of the avaible bulit-in models.
dmm.cluster.BaseModel <- function(model, data, alpha=1.0, iters=5000, burnin=200, shuffle=TRUE){
  julia$assign("params", model$params)
  strcommand <- paste0("basemodel=",model$model_type,"(params)")
  julia$command(strcommand)
  
  julia$assign("Y", data)
  julia$assign("alpha", alpha)
  julia$assign("iters", iters)
  julia$assign("shuffle", shuffle)
  dmmstates <- julia$eval("dp_cluster(Y, basemodel, alpha, iters, shuffle)")
  return(dmmstates)
}

dmm.summarize <- function(){
  
}


