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
#' @return A model object of type Rmodel which can be passed to \code{dmm.cluster}.
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
#' Create an model object to be used in the \code{dmm.cluster} function, using user given Julia functions.
#' 
#' @usage \code{model <- dmm.model(filename, pdf_name, sample_name, marg_name, params, isconjugate=TRUE)}
#'  
#' @param filename A string. The name of the .jl file in which Julia functions are located
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, θ::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, θ::Tuple, params::Tuple)}. 
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param marg_name  A string. The name of the Julia function in \code{filename} that returns the marginal likelihood. The function should be of the form \code{marg_name(y::Float64, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @details \code{marg_name} is only requried for conjugate models. \code{sample_name} is optional for nonconjugate models.
#'
#' @return A model object of type Jmodel which can be passed to \code{dmm.cluster}.
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
#' @param filename The name of the Julia file
add_file <- function(filename){
  .dmm$julia$command(paste0("include(\"",filename,"\")"))
}

#' Create a model using bulit-in conjugate models
#' 
#' Create an model object to be used in the \code{dmm.cluster} function, using the packages bulit-in conjugate models.
#' 
#' @usage \code{model <- dmm.model(filename, pdf_name, sample_name, marg_name, params, isconjugate=TRUE)}
#'  
#' @param filename A string. The name of the .jl file in which Julia functions are located
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, θ::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, θ::Tuple, params::Tuple)}. 
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param marg_name  A string. The name of the Julia function in \code{filename} that returns the marginal likelihood. The function should be of the form \code{marg_name(y::Float64, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @details \code{marg_name} is only requried for conjugate models. \code{sample_name} is optional for nonconjugate models.
#'
#' @return A model object of type Jmodel which can be passed to \code{dmm.cluster}.
#'
#' @export
dmm.model.BaseModel <- function(typename = "MultivariateNormalModel()", params=NULL){
  model <- list(model_type=typename, params = params)
  attr(model, "class") <- "BaseModel"
  return(model)
}



#' Create the model used in the dmm.cluster function
#'
#' If not passed any argurments a multivariate normal conjugate prior will be used as default.
#'
#' @param typename A string. The name of the predefined conjugate prior you wish to use. Options listed below.
#'                 These do not require any \code{params} unless otherwise stated. "MultivariateNormal" is the default.
#'
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
  else if (length(arguments) == 5){
    add_file(arguments[1])
    return(.JModel(arguments[2], arguments[3], arguments[4], arguments[5]))
  }
  else {
    stop("Improper inputs to dmm.model. Either 1 or 4 strings must be passed.")
  }
  
}



