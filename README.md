
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/chadhazlett/sensemakr.svg?branch=master)](https://travis-ci.org/chadhazlett/sensemakr)

sensemakr
=========

The goal of sensemakr is to make a simple sensitivity (to confounding) analysis widely accessible. The package defines a custom 'sensemakr' class that enables plot, summary, and print methods so that the user can make sense of unobserved confounding.

Installation
------------

You can install sensemakr from github with:

``` r
# install.packages("devtools")
devtools::install_github("chadhazlett/sensemakr")
```

The accompanying vignette can be viewed [here](https://github.com/chadhazlett/sensemakr/blob/master/vignettes/sensemakr.md).

Additionally, if you want to install the companion vignette inside R, use the below command instead:

``` r
devtools::install_github("chadhazlett/sensemakr",build_vignettes = TRUE,force=TRUE)
vignette('sensemakr')
```

Example
-------

Below is a basic example of the functionality in the sensemakr package

``` r
library(sensemakr)
lm.out  = lm(peacefactor ~ directlyharmed + age + female + village, data = darfur)
sense.out = sensemakr(model=lm.out, treatment="directlyharmed")
plot(sense.out, showvars = list("age","female"))
```

![](tools/README-example-1.png)
