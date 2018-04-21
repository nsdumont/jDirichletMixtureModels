library(tictoc)
library(jDirichletMixtureModels)

dmm.setup()

data=rnorm(100)
model=dmm.BaseModel("UnivariateNormalModel")

t=dmm.benchmark(model, data)

print(t)