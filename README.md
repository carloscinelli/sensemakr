
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensemakr: Sensitivity Analysis Tools for OLS <img src="man/figures/sensemakr-logo-small.png" align="right" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sensemakr)](https://CRAN.R-project.org/package=sensemakr)
[![Downloads](https://cranlogs.r-pkg.org/badges/sensemakr)](https://cran.r-project.org/package=sensemakr)
[![R-CMD-check](https://github.com/chadhazlett/sensemakr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chadhazlett/sensemakr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`sensemakr` implements a suite of sensitivity analysis tools that
extends the traditional omitted variable bias framework and makes it
easier to understand the impact of omitted variables in regression
models, as discussed in [Cinelli, C. and Hazlett, C. (2020) “Making
Sense of Sensitivity: Extending Omitted Variable Bias.” Journal of the
Royal Statistical Society, Series B (Statistical
Methodology).](https://doi.org/10.1111/rssb.12348)

# News

- Watch the [useR! 2020
  presentation](https://www.youtube.com/watch?v=p3dfHj6ki68) for a quick
  introduction on sensemakr.

- Check out the [software paper
  preprint](https://www.researchgate.net/publication/340965014_sensemakr_Sensitivity_Analysis_Tools_for_OLS_in_R_and_Stata)!

- Check out the [Stata
  version](https://github.com/resonance1/sensemakr-stata) of the
  package!

- Check out the [Python
  version](https://github.com/nlapier2/PySensemakr) of the package!

- Check out the Robustness Value Shiny App at:
  <https://carloscinelli.shinyapps.io/robustness_value/>

- Check out the [package website](http://carloscinelli.com/sensemakr/)!

# Details

For theoretical details, [please see the JRSS-B
paper](https://www.researchgate.net/publication/322509816_Making_Sense_of_Sensitivity_Extending_Omitted_Variable_Bias).

For a practical introduction, [please see the software
paper](https://www.researchgate.net/publication/340965014_sensemakr_Sensitivity_Analysis_Tools_for_OLS_in_R_and_Stata)
or [see the package
vignettes](http://carloscinelli.com/sensemakr/articles/sensemakr.html).

For a quick start, watch the 15 min tutorial on sensitivity analysis
using sensemakr prepared for useR! 2020:

<iframe width="560" height="315" src="https://www.youtube.com/embed/p3dfHj6ki68" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
</iframe>

# CRAN

To install the current CRAN version run:

``` r
install.packages("sensemakr")
```

# Development version

To install the development version on GitHub make sure you have the
package `devtools` installed.

``` r
# install.packages("devtools") 
devtools::install_github("carloscinelli/sensemakr")
```

# Citation

Please use the following citations:

- [Cinelli, C., & Hazlett, C. (2020). “Making sense of sensitivity:
  Extending omitted variable bias.” Journal of the Royal Statistical
  Society: Series B (Statistical Methodology), 82(1),
  39-67.](https://doi.org/10.1111/rssb.12348)

- [Cinelli, C., & Ferwerda, J., & Hazlett, C. (2020). “sensemakr:
  Sensitivity Analysis Tools for OLS in R and
  Stata.”](https://www.researchgate.net/publication/340965014_sensemakr_Sensitivity_Analysis_Tools_for_OLS_in_R_and_Stata)

# Basic usage

``` r
# loads package
library(sensemakr)
#> See details in:
#> Carlos Cinelli and Chad Hazlett (2020). Making Sense of Sensitivity: Extending Omitted Variable Bias. Journal of the Royal Statistical Society, Series B (Statistical Methodology).

# loads dataset
data("darfur")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                         pastvoted + hhsize_darfur + female + village, data = darfur)

# runs sensemakr for sensitivity analysis
sensitivity <- sensemakr(model = model, 
                         treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)

# short description of results
sensitivity
#> Sensitivity Analysis to Unobserved Confounding
#> 
#> Model Formula: peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + 
#>     pastvoted + hhsize_darfur + female + village
#> 
#> Null hypothesis: q = 1 and reduce = TRUE 
#> 
#> Unadjusted Estimates of ' directlyharmed ':
#>   Coef. estimate: 0.09732 
#>   Standard Error: 0.02326 
#>   t-value: 4.18445 
#> 
#> Sensitivity Statistics:
#>   Partial R2 of treatment with outcome: 0.02187 
#>   Robustness Value, q = 1 : 0.13878 
#>   Robustness Value, q = 1 alpha = 0.05 : 0.07626 
#> 
#> For more information, check summary.

# long description of results
summary(sensitivity)
#> Sensitivity Analysis to Unobserved Confounding
#> 
#> Model Formula: peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + 
#>     pastvoted + hhsize_darfur + female + village
#> 
#> Null hypothesis: q = 1 and reduce = TRUE 
#> -- This means we are considering biases that reduce the absolute value of the current estimate.
#> -- The null hypothesis deemed problematic is H0:tau = 0 
#> 
#> Unadjusted Estimates of 'directlyharmed': 
#>   Coef. estimate: 0.0973 
#>   Standard Error: 0.0233 
#>   t-value (H0:tau = 0): 4.1844 
#> 
#> Sensitivity Statistics:
#>   Partial R2 of treatment with outcome: 0.0219 
#>   Robustness Value, q = 1: 0.1388 
#>   Robustness Value, q = 1, alpha = 0.05: 0.0763 
#> 
#> Verbal interpretation of sensitivity statistics:
#> 
#> -- Partial R2 of the treatment with the outcome: an extreme confounder (orthogonal to the covariates) that explains 100% of the residual variance of the outcome, would need to explain at least 2.19% of the residual variance of the treatment to fully account for the observed estimated effect.
#> 
#> -- Robustness Value, q = 1: unobserved confounders (orthogonal to the covariates) that explain more than 13.88% of the residual variance of both the treatment and the outcome are strong enough to bring the point estimate to 0 (a bias of 100% of the original estimate). Conversely, unobserved confounders that do not explain more than 13.88% of the residual variance of both the treatment and the outcome are not strong enough to bring the point estimate to 0.
#> 
#> -- Robustness Value, q = 1, alpha = 0.05: unobserved confounders (orthogonal to the covariates) that explain more than 7.63% of the residual variance of both the treatment and the outcome are strong enough to bring the estimate to a range where it is no longer 'statistically different' from 0 (a bias of 100% of the original estimate), at the significance level of alpha = 0.05. Conversely, unobserved confounders that do not explain more than 7.63% of the residual variance of both the treatment and the outcome are not strong enough to bring the estimate to a range where it is no longer 'statistically different' from 0, at the significance level of alpha = 0.05.
#> 
#> Bounds on omitted variable bias:
#> 
#> --The table below shows the maximum strength of unobserved confounders with association with the treatment and the outcome bounded by a multiple of the observed explanatory power of the chosen benchmark covariate(s).
#> 
#>  Bound Label R2dz.x R2yz.dx      Treatment Adjusted Estimate Adjusted Se
#>    1x female 0.0092  0.1246 directlyharmed            0.0752      0.0219
#>    2x female 0.0183  0.2493 directlyharmed            0.0529      0.0204
#>    3x female 0.0275  0.3741 directlyharmed            0.0304      0.0187
#>  Adjusted T Adjusted Lower CI Adjusted Upper CI
#>      3.4389            0.0323            0.1182
#>      2.6002            0.0130            0.0929
#>      1.6281           -0.0063            0.0670

# plot bias contour of point estimate
plot(sensitivity)
```

<img src="man/figures/figures-basic-usage-1.png" style="display: block; margin: auto;" />

``` r

# plot bias contour of t-value
plot(sensitivity, sensitivity.of = "t-value")
```

<img src="man/figures/figures-basic-usage-2.png" style="display: block; margin: auto;" />

``` r

# plot bias contour of lower limit of confidence interval
plot(sensitivity, sensitivity.of = "lwr")
```

<img src="man/figures/figures-basic-usage-3.png" style="display: block; margin: auto;" />

``` r

# plot extreme scenario
plot(sensitivity, type = "extreme")
```

<img src="man/figures/figures-basic-usage-4.png" style="display: block; margin: auto;" />

``` r

# latex code for sensitivity table
ovb_minimal_reporting(sensitivity)
#> \begin{table}[!h]
#> \centering
#> \begin{tabular}{lrrrrrr}
#> \multicolumn{7}{c}{Outcome: \textit{peacefactor}} \\
#> \hline \hline 
#> Treatment: & Est. & S.E. & t-value & $R^2_{Y \sim D |{\bf X}}$ & $RV_{q = 1}$ & $RV_{q = 1, \alpha = 0.05}$  \\ 
#> \hline 
#> \textit{directlyharmed} & 0.097 & 0.023 & 4.184 & 2.2\% & 13.9\% & 7.6\% \\ 
#> \hline 
#> df = 783 & & \multicolumn{5}{r}{ \small \textit{Bound (1x female)}: $R^2_{Y\sim Z| {\bf X}, D}$ = 12.5\%, $R^2_{D\sim Z| {\bf X} }$ = 0.9\%} \\
#> \end{tabular}
#> \end{table}
```

``` r
# html code for sensitivity table
ovb_minimal_reporting(sensitivity, format = "pure_html")
```

<table style="align:center">
<thead>
<tr>
<th style="text-align:left;border-bottom: 1px solid transparent;border-top: 1px solid black">
</th>
<th colspan="6" style="text-align:center;border-bottom: 1px solid black;border-top: 1px solid black">
Outcome: peacefactor
</th>
</tr>
<tr>
<th style="text-align:left;border-top: 1px solid black">
Treatment
</th>
<th style="text-align:right;border-top: 1px solid black">
Est.
</th>
<th style="text-align:right;border-top: 1px solid black">
S.E.
</th>
<th style="text-align:right;border-top: 1px solid black">
t-value
</th>
<th style="text-align:right;border-top: 1px solid black">
R<sup>2</sup><sub>Y~D\|X</sub>
</th>
<th style="text-align:right;border-top: 1px solid black">
RV<sub>q = 1</sub>
</th>
<th style="text-align:right;border-top: 1px solid black">
RV<sub>q = 1, α = 0.05</sub>
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left; border-bottom: 1px solid black">
<i>directlyharmed</i>
</td>
<td style="text-align:right;border-bottom: 1px solid black">
0.097
</td>
<td style="text-align:right;border-bottom: 1px solid black">
0.023
</td>
<td style="text-align:right;border-bottom: 1px solid black">
4.184
</td>
<td style="text-align:right;border-bottom: 1px solid black">
2.2%
</td>
<td style="text-align:right;border-bottom: 1px solid black">
13.9%
</td>
<td style="text-align:right;border-bottom: 1px solid black">
7.6%
</td>
</tr>
</tbody>
<tr>
<td colspan="7" style="text-align:right;border-bottom: 1px solid transparent;font-size:11px">
Note: df = 783; Bound ( 1x female ): R<sup>2</sup><sub>Y~Z\|X,D</sub> =
12.5%, R<sup>2</sup><sub>D~Z\|X</sub> = 0.9%
</td>
</tr>
</table>
