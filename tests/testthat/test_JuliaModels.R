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
  julia$command("theta = (theta...);")
  jointOverMarg <- julia$eval("DirichletMixtureModels.pdf_joint(model,y,theta)/DirichletMixtureModels.marginal_likelihood(model,y)")
  conditional <- julia$eval("DirichletMixtureModels.pdf_posterior(model,y,theta)")
  expect_equal( jointOverMarg, conditional)
})

test_that("UnivariateNormalModel tests", {
  julia$command("model = UnivariateNormalKnownSigma(4.5, 6.8, 1.2);")
  julia$assign("y",3.67)
  julia$assign("theta",0.2)
  julia$command("theta = (theta...);")
  jointOverMarg <- julia$eval("DirichletMixtureModels.pdf_joint(model,y,theta)/DirichletMixtureModels.marginal_likelihood(model,y)")
  conditional <- julia$eval("DirichletMixtureModels.pdf_posterior(model,y,theta)")
  expect_equal( jointOverMarg, conditional)
})

test_that("UnivariateExponentialModel tests", {
  julia$command("model = UnivariateExponentialModel(3.2, 1.6);")
  julia$assign("y",3.67)
  julia$assign("theta",0.2)
  julia$command("theta = (theta...);")
  jointOverMarg <- julia$eval("DirichletMixtureModels.pdf_joint(model,y,theta)/DirichletMixtureModels.marginal_likelihood(model,y)")
  conditional <- julia$eval("DirichletMixtureModels.pdf_posterior(model,y,theta)")
  expect_equal( jointOverMarg, conditional)
})

test_that("MultivariateNormalModel tests", {
  julia$command("model = MultivariateNormalModel();")
  julia$assign("y",c(3.67,4.21))
  julia$assign("theta",list(c(0.2,1.08), diag(2.7,nrow=2)))
  julia$command("theta = (theta...);")
  jointOverMarg <- julia$eval("DirichletMixtureModels.pdf_joint(model,y,theta)/DirichletMixtureModels.marginal_likelihood(model,y)")
  conditional <- julia$eval("DirichletMixtureModels.pdf_posterior(model,y,theta)")
  expect_equal( jointOverMarg, conditional)
})