## Model functions for the jDirichletMixtureModels Package

#' Create a model using R fucntions
#'
#' *NOTE: This function is not avaible in the current version of jDirichletMixtureModels.*
#'  Create an model object to be used in the \code{dmm.cluster} function, using user given R functions.
#'
#'  @usage model <- dmm.Rmodel(pdf_fct, sample_fct, marg_fct, params, isconjugate=TRUE)
#'
#' @param pdf_func A function that takes as input \code{(data, likelihoodparams, params)}. It returns the value of the probability density function likelihood at \code{(data,likelihoodparams)} given \code{params}.
#' @param sample_fct Optional for nonconjugate models. A function takes as input \code{(data, params)}. It returns the value of the sample posterior function at \code{data} given \code{params}.
#' @param marg_fct Only needed for conjugate models. A function takes as input \code{(data, params)}. It returns the value of the marginal likelihood funciton at \code{data} given \code{params}..
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @return A model object of type RModel which can be passed to \code{dmm.cluster}.
#'
#' #@export
dmm.RModel <- function(pdf_func, sample_func=NULL, marg_func=NULL, params, isconjugate=TRUE){
  stop("This function is not avaible in the current version of jDirichletMixtureModels")
  
  if (isconjugate & (is.null(marg_fct) | is.null(sample_func)) ){
    stop("Error: A marg_fct and sample_func are requried for conjugate models.")
  }
  if (isconjugate){
    model <- list(pdf_likelihood=pdf_func, sample_posterior=sample_func,
                marginal_likelihood=marg_func, params = params, isconjugate=isconjugate)
  }
  attr(model, "class") <- "RModel"
  return(model)
}

#' Create a model using Julia fucntions
#'
#' Create an model object to be used in the \code{dmm.cluster} function, using user given Julia functions. Must call \code{dmm.addfile} to import files, in which the Julia functions are stored, before using \code{dmm.cluster} on a JModel.
#' Functions \code{dmm.JConjugateModel} and \code{dmm.JNonConjugateModel} are alternatives.
#'
#' @usage \code{dmm.addfile(filename)}
#' \code{model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)}
#' \code{model <- dmm.Jmodel(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)}
#' \code{model <- dmm.JConjugateModel(pdf_name, sample_name, marg_name, params)}
#' \code{model <- dmm.JNonConjugateModel(pdf_name, sample_name, params)}
#'
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, theta::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, theta::Tuple, params::Tuple)}.
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function for conjugate case or the sample prior for nonconjugate case. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param marg_name  A string. For conjugate case only. The name of the Julia function in \code{filename} that returns the marginal likelihood. The function should be of the form \code{marg_name(y::Float64, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @details \code{marg_name} is only requried for conjugate models.
#'
#' @return A model object of type JModel which can be passed to \code{dmm.cluster}.
#'
#' @examples dmm.addfile(filename)
#' # The following all make models using Julia functions
#' model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#' model <- dmm.Jmodel(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#' model <- dmm.JConjugateModel(pdf_name, sample_name, marg_name, params)
#' model <- dmm.JNonConjugateModel(pdf_name, sample_name, params)
#'
#' @export
dmm.JModel <- function(pdf_name, sample_name=NULL, marg_name=NULL, params, isconjugate=TRUE){
  if (isconjugate) {
    model <- list(pdf_likelihood=pdf_name, sample_posterior=sample_name,
                  marginal_likelihood=marg_name, params = params)
    attr(model, "class") <- "JConjugateModel"
  } else {
    model <- list(pdf_likelihood=pdf_name, sample_prior=sample_name,
                  params = params)
    attr(model, "class") <- "JNonConjugateModel"
  }
  if (isconjugate & is.null(marg_name)){
    stop("Error: A marg_name and sample_name are requried for conjugate models.")
  }
  return(model)
}

#' Create a conjugate model using Julia fucntions
#'
#' Create an model object to be used in the \code{dmm.cluster} function, using user given Julia functions. Must call \code{dmm.addfile} to import files, in which the Julia functions are stored, before using \code{dmm.cluster} on a JModel.
#' Functions \code{dmm.JModel} and \code{dmm.model} are alternatives.
#'
#' @usage dmm.addfile(filename)
#' model <- dmm.JConjugateModel(pdf_name, sample_name, marg_name, params)
#'
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, theta::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, theta::Tuple, params::Tuple)}.
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample posterior function. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param marg_name  A string. The name of the Julia function in \code{filename} that returns the marginal likelihood. The function should be of the form \code{marg_name(y::Float64, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#' @param isconjugate A logical. \code{TRUE} (default) if the user specfied model is conjugate, \code{FALSE} if not.
#'
#' @return A model object of type JModel which can be passed to \code{dmm.cluster}.
#'
#' @examples dmm.addfile(filename)
#' # The following all make conjugate models using Julia functions
#' model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#' model <- dmm.Jmodel(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#' model <- dmm.JConjugateModel(pdf_name, sample_name, marg_name, params)
#'
#' @export
dmm.JConjugateModel <- function(pdf_name, sample_name, marg_name, params){
  model <- list(pdf_likelihood=pdf_name, sample_posterior=sample_name,
                marginal_likelihood=marg_name, params = params)
  attr(model, "class") <- "JConjugateModel"
  return(model)
}


#' Create a nonconjugate model using Julia fucntions
#'
#' Create an model object to be used in the \code{dmm.cluster} function, using user given Julia functions. Must call \code{dmm.addfile} to import files, in which the Julia functions are stored, before using \code{dmm.cluster} on a JModel.
#' Functions \code{dmm.JModel} and \code{dmm.model} are alternatives..
#'
#' @usage dmm.addfile(filename)
#' model <- dmm.model(pdf_name, sample_name, marg_name, params, isconjugate=TRUE)
#'
#' @param pdf_name A string. The name of the Julia function in \code{filename} that returns the probability density function likelihood. The function should be of the form \code{pdf_name(y::Float64, theta::Tuple, params::Tuple)} or \code{pdf_name(y::Array{Float64,1}, theta::Tuple, params::Tuple)}.
#' @param sample_name  A string. The name of the Julia function in \code{filename} that returns the sample prior. The function should be of the form \code{sample_name(y::Float64, params::Tuple)}, \code{sample_name(y::Array{Float64,1}, params::Tuple)} or \code{sample_name(y::Array{Float64,2}, params::Tuple)}.
#' @param params A list of all hyperparameters needed for the above three functions.
#'
#' @return A model object of type JModel which can be passed to \code{dmm.cluster}.
#'
#' @examples dmm.addfile(filename)
#' # The following all make nonconjugate models using Julia functions
#' model <- dmm.model(pdf_name, sample_name, params, isconjugate=FALSE)
#' model <- dmm.Jmodel(pdf_name, sample_name, params, isconjugate=FALSE)
#' model <- dmm.JNonConjugateModel(pdf_name, sample_name, params)
#'
#' @export
dmm.JNonConjugateModel <- function(pdf_name, sample_name, params){
  model <- list(pdf_likelihood=pdf_name, sample_prior=sample_name,
                params = params)
  attr(model, "class") <- "JNonConjugateModel"
  return(model)
}

#' Add a Julia file to be accessible.
#'
#' This function must be called before runnning the \code{dmm.cluster} function using a JModel (see \code{dmm.JModel}).
#'
#' @usage dmm.addfile(filename)
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
#' Function \code{dmm.model} is an alternative method.
#'
#' @usage model <- dmm.BaseModel(typename, params)
#'
#' @param typename A string. The name of the predefined conjugate prior you wish to use. Options listed under details.
#'                  "MultivariateNormalModel" is the default.
#' @param params A list of the hyperparameter values for the likelihood functions. Many model have default \code{params} values and thus can be made without passing any \code{params}.
#'               See documentation for what parameters a given model may take.
#' @param data In lieu of explicit hyperparameter values, some models can infer good hyperparameter values from given data.
#'               This option is supported for MultivariateNormalModel and UnivariateNormalModel, as well as UnivariateNormalKnownSigma.

#'
#' @details Bulit-in models avaible are:
#'                        "MultivariateNormalModel" (default),
#'                        "UnivariateNormalModel",
#'                        "UnivariateNormalKnownSigma",
#'                        "UnivariateExponentialModel".
#'
#' @return A model object of type BaseModel which can be passed to \code{dmm.cluster}.
#'
#' @export
dmm.BaseModel <- function(typename = "MultivariateNormalModel", params=NULL, data=NULL){
  if (!is.null(data)){
    if(!(typename=="MultivariateNormalModel" || typename=="UnivariateNormalModel" || typename=="UniivariateNormalKnownSigma")){
      stop("Error: this option is only supported for the MultivariateNormalModel, UnivariateNormalModel and UnivariateNormalKnownSigma")
    }
  }
  if (!is.null(params) & !is.null(data)){
    stop("Error: Provide either hyperparameters or a dataset, not both")
  }
  model <- list(model_type=typename, params=params, data=data)
  attr(model, "class") <- "BaseModel"
  return(model)
}


#' Create a model
#'
#' Make a model object to be passed to \code{dmm.cluster}. If not passed any argurments a conjugate multivariate normal likelihood model with default parameters will be used.
#'
#' @usage dmm.model(...)
#'
#' @details Depending on what argurments are passed to \code{dmm.model()} either a BaseModel (see \code{dmm.BaseModel}),
#' a RModel (see \code{dmm.RModel}), or JModel (see \code{dmm.JModel}) will be made.
#' 
#' *NOTE: The use of RModels is not avaible in the current version of jDirichletMixtureModels*
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

#' Create a model object
#'
#' If nothing is passed to \code{dmm.model} it will create and return a BaseModel using a multivariate normal conjugate model with default parameters (see \code{dmm.BaseModel}).
#'
#' @export
dmm.model.NULL <- function(...){
  return(dmm.BaseModel())
}

#' Create a model object
#'
#' If a function is passed as the first input to \code{dmm.model} it will create and return an Rmodel (see \code{dmm.RModel}).
#'
#' @export
dmm.model.function <- function(...){
  arguments <- list(...)
  return(dmm.RModel(arguments))
}

#' Create a model object
#'
#' If a string is passed as the first input to \code{dmm.model} it will create and return either a BaseModel (if passed one string, plus an optional \code{params}) (see \code{dmm.BaseModel}),
#' or a JModel (see \code{dmm.JModel})
#'
#' @export
dmm.model.character <- function(...){
  arguments <- list(...)

  if (length(arguments) < 3){
    return(dmm.BaseModel(arguments))
  }
  else {
    return(dmm.JModel(arguments))
  }
}
