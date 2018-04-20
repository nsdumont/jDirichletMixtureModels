
using DirichletMixtureModels

importall DirichletMixtureModels

import Distributions:
        UnivariateDistribution,
        MultivariateDistribution,
        Exponential,
        Normal,
        NormalCanon,
        MvNormal,
        NormalStats,
        rand,
        suffstats,
        pdf

import ConjugatePriors:
    NormalGamma,
    NormalWishart,
    posterior_canon,
    rand



function example_pdf(y::Float64,μ,λ,μ0,n0,α0,β0)
  pdf(NormalCanon(λ*μ, λ), y)
end
function example_pdf(y::Array{Float64},μ,λ,μ0,n0,α0,β0)
  pdf.(NormalCanon(λ*μ, λ), y)
end
function example_post(Y::Array{Float64,1}, μ0,n0,α0,β0)
  p=posterior_canon(NormalGamma(μ0,n0,α0,β0),suffstats(Normal,Y))
  rand(p)
end
function example_post(y::Float64, μ0,n0,α0,β0)
  p=posterior_canon(NormalGamma(μ0,n0,α0,β0),suffstats(Normal,[y]))
  rand(p)
end
function example_pri(μ0,n0,α0,β0)
  rand(NormalGamma(μ0,n0,α0,β0))
end
function example_marg(y::Float64,μ0,n0,α0,β0)
  gamma(α0+1./2)/gamma(α0) * sqrt(n0/(n0+1)) * 1/sqrt(2*π) * β0^α0 /
    (β0+n0/2/(n0+1)*(y-μ0)^2)^(α0+1./2)
end
