# jDirichletMixtureModels
- Currently in active development! 

Package `jDirichletMixtureModels` provides utilities for clustering using Dirichlet Process mixture models in R. It supports a number of existing conjugate distribution pairs, as well as user-specified distributions.

Currently, the package only supports clustering using conjugate distributions using the methods in Markov Chain Sampling Methods for Dirichlet Process Mixture Models by Radford Neal. Clustering using non-conjugate distributions is under active development.

This package is an R wrapper of the `Julia` package [DirechletMixtureModels](https://github.com/krylea/DirichletMixtureModels.jl), which it is being co-develped with.
[Julia](https://julialang.org) is a high-level, high-performance dynamic programming language for numerical computing.

## Getting Started

### Installation
This package can be installed from `Github` by using `devtools`:
```{r, eval=FALSE}
devtools::install_github("nsdumont/jDirichletMixtureModels")
```

### Basic Usage
To use the package, the first thing you need to do is initial setup. Call the `dmm.setup()` function after loading the package. Input to `dmm.setup()` should be the path name of the folder in which your Julia binary executable is located (inside the Julia application bundle).
Mac OSX users using the latest Julia version do not need to pass any input to `dmm.setup()`. Since by default, it will use the correct path  `/Applications/Julia-0.6.app/Contents/Resources/julia/bin`.

```{r, eval=FALSE}
library(jDirichletMixtureModels)
dmm.setup()
```

Next you must define a model. A number of conjugate models are provided in the package (see section [Bulit-In Models](#bulit-in-models) for more information).
To use a predefined model you must pass it's name and any requried parameters.
```{r, eval=FALSE}
model <- dmm.model("UnivariateNormal")     # Using the default parameters
model <- dmm.model("MultivariateNormal", params=c(mu,kappa,T,nu))     # Using user defined parameters: they are the paramters for the Normal Wishart distribution
```

The rest of this section refers to package feautures in active development and not fully tested.

User defined models (both conjugate and non-conjugate) can also be used. Users must supply the following funtions:
- The probability density likelihood
- The sample posterior function
- The marginal likelihood
 
If these functions are R functions, models are defined as follows:
 ```{r, eval=FALSE}
model <- dmm.model(pdf_func, sample_func, marg_func, params, isconjugate)
```
where `params` is a list of all hyperparameters. The functions `pdf_func`, `sample_func`, and  `marg_func` must all take these parameters. The option `isconjugate` is either TRUE or FALSE.
 
Users can also define these functions as Julia functions with names `pdf_name`, `sample_name`, `marg_name` stored in file `filename`: 
```{r, eval=FALSE}
model <- dmm.model(filename, pdf_name, sample_name, marg_name, params, isconjugate)
```
All of these inputs must be strings


### Example

Suppose you have a dataset of 2D N-by-2 data you wish to find clusters over, with no prior knowledge of the distribution. If your data is stored as a 2D array called 'Xdata', you could run the following:

```{r, eval=FALSE}
  library(jDirichletMixtureModels)
  dmm.setup()

  model <- dmm.model()

  states <- dmm.cluster(model, Xdata)
```

This will define a conjugate multivariate Normal model (multivariate Normal likelihood with Normal-Wishart prior) over your data, with default hyper-parameters. It will then perform a run of the clustering algorithm over the data, returning a list of cluster states.
This is a list of (effectively) I.I.D draws from the posterior over the clusters (default 5000 iterations with a burnin of 200).

A cluster state, `states[[i]]`, consists of labeled data, `states[[i]]$labeledData`, and cluster information, `states[[i]]$clusterInfo`.

Suppose I wanted to take a random draw and see a summary of the clusters for that draw. I would then run the following:
```
  dmm.summarize(states[[1]]$clusterInfo)
```
This will print a summary of the clusters in the first state in the list. Note that by default the states are randomized so that they may be used as IID draws.
To see a plot of the data colored by cluster I would run:
```
  dmm.plot(states[[1]]$labeledData)
```

## Bulit-In Models
The avaible bulit-in models are:
- "MultivariateNormal" (default): A multivariate Normal likelihood with Normal-Wishart prior. Requries paramters: prior mean, \lambda, scale matrix, \nu (see https://en.wikipedia.org/wiki/Normal-Wishart_distribution)

- "UnivariateNormal": A univariate Normal likelihood with Normal-Gamma prior. Requries paramters: prior mean, precision scale, shape, rate

- "UnivariateNormalKnowSigma": A univariate Normal likelihood, with likelihood variance known, with Normal prior. Requries paramters: prior mean, variance and fixed likelihood variance.
 
- "UnivariateExponential": A univariate Exponential likelihood with Gamma prior. Requries paramters: prior shape, rate

Note: parameters must be passed in the order they are listed in above.

## Authors

* **Nicole Dumont** - *Initial work* - [NSDumont](https://github.com/nsdumont)
* **Kira Selby** - *Initial work* - [Krylea](https://github.com/krylea)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Dr. Radford M. Neal for his wonderful paper on Dirichlet Process mixture models.
* Dr. Martin Lysy for teaching a lovely course on Computational Inference that motivated this project.
* [PurpleBooth](https://github.com/PurpleBooth) for creating the template used to make this readme!

