library(jDirichletMixtureModels)

dmm.setup()
X=rnorm(100)
dmm.addfile("examples/example2.jl")
#m=dmm.JConjugateModel("example_pdf", "example_post", "example_marg", list(0.0,0.1,1.0,1.0))

#o=dmm.cluster(m,X)

m2=dmm.JNonConjugateModel("example_pdf", "example_pri", list(0.0,1.0,2.0,0.5))
o=dmm.cluster(m2,X)
