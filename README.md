
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/mohammedfaizan0014/dress/workflows/R-CMD-check/badge.svg)](https://github.com/mohammedfaizan0014/dress/actions)
<!-- badges: end -->

The R package
[`dress`](https://mohammedfaizan0014.github.io/dress/index.html)
provides some measures for disclosure risk associated with the release
of protected data, irrespective of what mechanism was used to protect
it. Key principles of the disclosure framework include distinctness,
accuracy and un-deniability. This method can be applied to any pair of
original and protected data-sets despite a difference in dimensionality
and without assuming any particular joint probability structure between
the original and protected data.

# In the repository

-   **analysis/** : report and presentation
-   **dress/** : the shiny dashboard

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=dress):

``` r
install.packages("dress")
```

You can install the **development** version from
[GitHub](https://github.com/mohammedfaizan0014/dress)

``` r
# install.packages("remotes")
remotes::install_github("mohammedfaizan0014/dress")
```

Installing this software requires a compiler

## Example

``` r
library(svMisc)
library(dress)
library(sdcMicro)


# ##################
# ##all continuous###################
CASC_sample <- CASCrefmicrodata[,c(2,3,4,6)]
CASC_protected <- addNoise(CASC_sample,noise = 100)$xm #Additive Noise protected

DRisk_NN <- drscore(
  Sample = CASC_sample, #Original Sample
  Protected = CASC_protected,
  delta = 0.05,
  kdistinct = 0.05, #k distinct threshold if integer then
                 # probability threshold is k/SS (SS = sample size)
  ldeniable = 5, # l undeniable threshold if integer then
                         # probability threshold is l/SS (SS = sample size)
  neighbourhood = 1,
  #Possible 'neighbourhood' types
  # 1 = Mahalanobis (Based on Mahalanobis Distance)
  # 2 = DSTAR   (Based on Density Based Distance)
  # 3 = StdEuclid (Based on Standardised (by std dev) Euclidean Distance)
  # 4 = RelEuclid (Relative Euclidean Distance sum_k ((Xk-Yk)/Xk)^2)
  neigh_type = 'prob',
  #Possible 'neigh_type' types
  #constant = fixed threshold on distance
  #prob = Nearest Neighbour Probability Neighbourhood used (Worst Case Scenario 1)
  #estprob = = Nearest Neighbour Probability Neighbourhood used based on protected density (Worst Case Scenario 2)
  numeric.vars = 1:4, #Which Variables are continuous?
  outlier.par = list(centre = median,
                     scale = var,
                     thresh = 0.01)
  #Parameters to adjust how MV outliers are determined.
  #Default is that lie 99% (based on Chi-Square n-1 dist) away from median after scale by variance.
)

#Update neighbourhood to fixed threshold definition
DRisk_Fxd <- update(DRisk_NN,neigh_type = 'constant',
                          delta = 1)
```

## Learning the Mathematics

-   *[On the Disclosure Risk Framework for Micro-Level Data]()*
    <!-- the paper  -->

## Getting help

-   Common questions about dress package are often found on
    [stack-overflow](https://stackoverflow.com/).
