
library(jDirichletMixtureModels)

dmm.setup()
X=rnorm(100)

m=dmm.BaseModel("UnivariateNormalModel", c(0.,1.,1.,1.))

o=dmm.cluster(m,X)

state=o[[1]]
dmm.summarize(state$clusters)

X2=matrix(rnorm(200), ncol=2)
m=dmm.BaseModel("MultivariateNormalModel")

o=dmm.cluster(m,X2,iters=1000)

state=o[[1]]
dmm.summarize(state$clusters)
