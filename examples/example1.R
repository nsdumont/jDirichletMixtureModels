
library(jDirichletMixtureModels)

dmm.setup()


X=rnorm(100)

m=dmm.BaseModel("UnivariateNormalModel", c(0.,1.,1.,1.))

o=dmm.cluster(m,X)

state=o[[1]]
dmm.summarize(state$clusters)

Xdata=rbind(matrix(rnorm(100),ncol=2),10+matrix(rnorm(100),ncol=2)/2)
m=dmm.BaseModel("MultivariateNormalModel")

o=dmm.cluster(m,Xdata,iters=1000)

state=o[[1]]
dmm.summarize(state$clusters)
