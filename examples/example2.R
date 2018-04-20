library(jDirichletMixtureModels)

dmm.setup()
X=rnorm(100)
dmm.addfile("examples/example2.jl")
m=dmm.JConjugateModel("example_pdf", "example_post", "example_marg", list(0.0,1.0,1.0,1.0))

o=dmm.cluster(m,X)
