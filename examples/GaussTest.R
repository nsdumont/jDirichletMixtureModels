library(jDirichletMixtureModels)
library(data.table)

Xdata <- as.matrix(syntheticGaussian1)
attr(Xdata, "scaled:center") <- NULL
attr(Xdata, "scaled:scale") <- NULL
inds <- sample(nrow(Xdata), size = 1000)
Xdata <- Xdata[inds,]

dmm.setup()
model <- dmm.BaseModel(data=Xdata)

states <- dmm.cluster(model, Xdata, alpha = 10.0, iters = 1500, burnin = 1000, shuffled = TRUE)

astate <- states[[1]]
dmm.summarize(astate$clusters)
dmm.plot(astate$data)

