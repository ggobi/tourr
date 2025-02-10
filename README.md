tourr: tour methods for multivariate data visualisation
================
Hadley Wickham, Di Cook, Nick Spyrison, Ursula Laa, H. Sherry Zhang,
Stuart Lee
<br> February 11, 2025

<!-- README.md is generated from README.Rmd. Please edit that file -->

# tourr <img src="man/figures/logo.png" align="right" width="150" />

The goal of tourr is to explore shapes of high-dimensional data. This
code also allows new tour methods to be created that utilise geodesic
interpolation and basis generation functions.

## Installation

You can install the released version of tourr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tourr")
```

You can install the development version of tourr from github with:

``` r
# install.packages("remotes")
remotes::install_github("ggobi/tourr")
```

## Example

To run a tour in R, use one of the animate functions. This code will
show a 2D tour displayed as a scatterplot on a 6D data set with three
labelled classes.

``` r
animate_xy(flea[,-7], col=flea$species)
```

## Resources

The best place to get started is the [Journal of Statistical Software
paper](https://www.jstatsoft.org/article/view/v040i02).
