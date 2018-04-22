## Setup functions for the jDirichletMixtureModels Package

.dmm <- new.env(parent = emptyenv())

#' Do initial setup for jDirichletMixtureModels package.
#'
#' \code{dmm.setup} does the initial setup for jDirichletMixtureModels package.
#' 
#' @usage dmm.setup()
#'
#' @param ... arguments passed to \code{JuliaCall::julia_setup}. Most noteable:the first input should be the path to the folder in which the Julia bin file is located on the user's machine. 
#' It will default to using the macOSX path for the most recent Julia version. 
#'
#' @examples
#' \dontrun{
#' dmm.setup()
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
