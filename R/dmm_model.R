## Model functions for the jDirichletMixtureModels Package

#' Create a model using R fucntions
#' 
#'  Create an model object to be used in the \code{dmm.cluster} function, using user given R functions.
#'  
#'  @usage \code{model <- dmm.model(pdf_fct, sample_fct, marg_fct, params, isconjugate=TRUE)}
#'  
#' @param pdf_func A function that takes as input \code{(data, likelihoodparams, params)}. It returns the value of the probability density function likelihood at \code{(data,likelihoodparams)} given \code{params}. 
#' @param sample_fct Optional for nonconjugate models. A function takes as input \code{(data, params)}. It returns the value of the sample posterior function at \code{data} given \code{params}.
#' @param marg_fct Only needed for conjugate models. A function takes as input \code{{(data, params)}. It returns the value of the marginal likelihood funciton at \code{data} given \code{params}.. 
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'  
#' @return A model object of type RModel which can be passed to \code{dmm.cluster}.
#' 
#' @export
dmm.model.RModel <- function(pdf_func, sample_func=NULL, marg_func=NULL, params, isconjugate=TRUE){
  if (isconjugate & (is.null(marg_fct) | is.null(sample_func)) ){
    stop("Error: A marg_fct and sample_func are requried for conjugate models.")
  }
  model <- list(pdf_likelihood=pdf_func, sample_posterior=sample_func,
                marginal_likelihood=marg_func, params = params, isconjugate=isconjugate)
  attr(model, "class") <- "RModel"
  return(model)
}

#' Create a model using Julia fucntions
#' 
#' Create an model object to be used in the \code{dmm.cluster} function, using user given Julia functions. Must call \code{dmm.addfile} to import files, in which the Julia functions are stored, before using \code{dmm.cluster} on a JModel.
#' 
#' @usage \code{dmm.addfile(filename)}
#' \code{model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)}
#'  
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, θ::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, θ::Tuple, params::Tuple)}. 
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param marg_name  A string. The name of the Julia function in \code{filename} that returns the marginal likelihood. The function should be of the form \code{marg_name(y::Float64, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @details \code{marg_name} is only requried for conjugate models. \code{sample_name} is optional for nonconjugate models.
#'
#' @return A model object of type JModel which can be passed to \code{dmm.cluster}.
#'
#' @export
dmm.model.JModel <- function(pdf_name, sample_name=NULL, marg_name=NULL, params, isconjugate=TRUE){

  if (isconjugate & (is.null(marg_name) | is.null(sample_name)) ){
    stop("Error: A marg_name and sample_name are requried for conjugate models.")
  }
  model <- list(pdf_likelihood=pdf_name, sample_posterior=sample_name,
                marginal_likelihood=marg_name, params = params, isconjugate=isconjugate)
  attr(model, "class") <- "JModel"
  return(model)
}

#' Add a Julia file to be accessible.
#' 
#' This function must be called before runnning the \code{dmm.cluster} function using a JModel (see \code{dmm.model.JModel}).
#' 
#' @param filename The name of the Julia file.
#' 
#' @export
dmm.addfile <- function(filename){
  .dmm$julia$command(paste0("include(\"",filename,"\")"))
}

#' Create a model using bulit-in conjugate models
#' 
#' Create an model object to be used in the \code{dmm.cluster} function, using the packages bulit-in conjugate models.
#' 
#' @usage \code{model <- dmm.model(typename, params)}
#'  
#' @param typename A string. The name of the predefined conjugate prior you wish to use. Options listed under details.
#'                  "MultivariateNormalModel" is the default.
#' @param params A list of the hyperparameter values for the likelihood functions. Many model have default \code{params} values and thus can be made without passing any \code{params}.
#'               See documentation for what parameters a given model may take.
#'
#' @details Bulit-in models avaible are:
#'                        "MultivariateNormalModel" (default),
#'                        "UnivariateNormalModel",
#'                        "UnivariateNormalKnowSigma",
#'                        "UnivariateExponentialModel".
#'
#' @return A model object of type BaseModel which can be passed to \code{dmm.cluster}.
#'
#' @export
dmm.model.BaseModel <- function(typename = "MultivariateNormalModel()", params=NULL){
  model <- list(model_type=typename, params = params)
  attr(model, "class") <- "BaseModel"
  return(model)
}


#' Create a model 
#'
#' Make a model object to be passed to \code{dmm.cluster}. If not passed any argurments a conjugate multivariate normal likelihood model with default parameters will be used.
#'
#' @usage \code{dmm.model(...)}
#'
#' @details Depending on what argurments are passed to \code{dmm.model()} either a BaseModel (see \code{dmm.model.BaseModel}),
#' a RModel (see \code{dmm.model.RModel}), or JModel (see \code{dmm.model.JModel}) will be made.
#' 
#' @return A model object which can be passed to \code{dmm.cluster}.
#'
#' @examples
#' \dontrun{
#' # Example of using a multivariate normal conjugate prior (the default)
#' model <- dmm.model()
#'
#' # Example of using a bulit-in conjugate prior
#' model <- dmm.model(typename, params)
#'
#' # Using a user specified R functions
#'model <- dmm.model(pdf_fct, sample_fct, marg_fct, params, isconjugate=TRUE)
#'
#' # Using a user specified Julia functions located in a .jl file called filename
#' dmm.addfile(filename)
#' model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#' }
#'
#' @export
dmm.model <- function(...){
  UseMethod("dmm.model")
}

#' @export
dmm.model.NULL <- function(...){
  return(dmm.model.BaseModel())
}

#' Create a model object
#'
#' If a function is passed as the first input to \code{dmm.model} it will create and return an Rmodel (see \code{dmm.model.RModel}).
#' 
#' @export
dmm.model.function <- function(...){
  arguments <- list(...)
  return(dmm.model.RModel(arguments))
}

#' Create a model object
#'
#' If a string is passed as the first input to \code{dmm.model} it will create and return either a BaseModel (if passed one string, plus an optional \code{params}) (see \code{dmm.model.BaseModel}),
#' or a JModel (see \code{dmm.model.JModel})
#' 
#' @export
dmm.model.character <- function(...){
  arguments <- list(...)
  
  if (length(arguments) < 3){
    return(dmm.model.BaseModel(arguments))
  }
  else {
    return(dmm.model.JModel(arguments))
  }
}


