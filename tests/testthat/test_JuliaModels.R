# Using JuliaCall to run unit tests for model distribution code from Julia
context("Julia model code")

library(jDirichletMixtureModels)
library(JuliaCall)

julia <- julia_setup("/Applications/Julia-0.6.app/Contents/Resources/julia/bin/")
julia$library("DirichletMixtureModels")
julia$command("using DirichletMixtureModels")
julia$command("importall DirichletMixtureModels")

test_that("UnivariateNormalModel tests", {
  julia$command("model = UnivariateNormalModel();")
  julia$assign("y",3.67)
  julia$assign("theta",c(0.2,1.08))
  jointOverMarg <- julia$eval("pdf_joint(model,y,theta)/marginal_likelihood(model,y)")
  conditional <- julia$eval("pdf_posterior(model,y,theta")
  expect_equal( jointOverMarg, conditional)
})