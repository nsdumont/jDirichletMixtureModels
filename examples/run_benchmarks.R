library(tictoc)
library(jDirichletMixtureModels)

dmm.setup()

model <- dmm.BaseModel("UnivariateNormalModel")

nums <- c(50,100,200,400)
times <- data.frame()
for (i in 1:length(nums)) {
  data <- rnorm(nums[i])
  t <- dmm.benchmark(model, data)
  times <- rbind(times, data.frame("N"=nums[i],t))
}

times





print(t)