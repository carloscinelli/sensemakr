# The `Sensemakr` Vignette
Michael Tzen, Chad Hazlett, Carlos Cinelli  
`r Sys.Date()`  


# Background
Welcome to the vignette for `sensemakr`.  This document briefly describes the basic idea and method for the procedure, then moves on to basic usage, followed by more customized usage in the following sections. 

## Introduction to `sensemakr`

The `sensemakr` package simply builds upon and makes more accessible a brand of sensitivity analysis closesly related to well known concepts of \textiT{omitted variable bias.} One has a linear outcome moel, and imagines that certain important confounders might be missing from it.  Given what \textit{is} in the model, what would need to be the properties of an omitted confounder to have biased your estimate by a certain amount (or as we put it, to imply an adjusted estimate that differs from the actual by that amount)?  In addition to changing the point estimate, such a confounder would change the standard errors, t-values, p-values, etc., making the adjusted versions of these quantities of interest as well.  

The challenge is to characterize a hypothesized troublesome confounder in such a way that enables the user to ask "is this an unobserved confounder that could likely exist in my circumstance?"  This depends heavily on the research design and information about the treatment assignment process. Sensitivity analysis is only a tool for elaborating our discussion of what might have gone wrong in a given observational analysis -- not for tell us that nothing has gone wrong. 

Confounders are characterized by their relationship to the treatment and to the outcome. Our main focus is on describing hypothesized confounders by the partial $R^2$ values relating them to the treatment and outcome. This is a more universal value than other parameters, as it does not depend on the scale of the hypothesized confounder, for example. Other advantages of this approach are described in the paper.  

A simple sensitivity analysis can be done that produces a two-dimensional plot in which one axis shows hypothesized values of the partial $R^2$ between a confounder and the treatment, and the other axis shows hypothesized values of the partial $R^2$ between a confounder and the outcome. Contour lines can be drawn in this two-dimensional space and labeled to indicate either the bias on each line, or the "adjusted treatment effect estimate".  While this is technically straightforward, it provides users with little insight: yes, a confounder with certain partial $R^2$ values would change the estimate by a certain amount.  But how can we use this to draw conclusinos about the robustness of real results against such confounders? In short, how can we assess the likelihood that a confounder that has a troubling impact as described by such a plot could have existed in our analysis?

To help investigators make the leap from the sensitivity analysis itself to an understanding of whether a given troublesome type of confounder is likely to exist, we provide two main modes of interpretation: \textit{benchmarking} and \textit{worst-case scenarios}.   In benchmarking we ask: for a given observed variable, what if an unobserved confounder existed with an equally strong relationship to the treatment and the outcome? Would such a confounder be troubling? Note that this is subtly different from merely leaving out the observable in question, since leaving it out would also effect the error variance.  Here we "leave the observable in"  but ask what would happen had unobservables with similar properties existed.  The value of benchmarking is that users may have an idea of how well they have explained the treatment or the outcome, and which covariates are already strong predictors.  If for example one knows that the treatment assignment process was largely random but that where it wasn't, a particular observed covariate was certainly the most important, then it may be possible to rule out unobserved covariates that have 2, 10, or 100 times as strong a relationship to the treatment.  Similarly if the outcome is known to be strongly driven by a few observables with little room for other explanatory factors, then the user may rule out the existence of confounders that are 10 times more strongly related to the outcome than the observables are as a group (`sensemakr` can also treat observables collectively in groups for such purposes.)

Benchmark analyses may be presented numerically or graphically. They may also be used to contemplate not only how point estimates change given a hypothesized confounder, but how $t$-values and $p$-values change, for example.  We demonstrate these below.  `sensemakr` also outputs natural language interpretations of benchmark comparisons. 

The second mode of interpretation we use is "worst-case scenarios". The fundmantal inuition is that the error term in a fitted linear model contains all the variation in $Y$ not already explained by covariates, $X$, and treatment $D$. What if nature is cruel, and all that remaining variation is in fact attribtuable to a confounder? In stating this, we have specified that the partial $R^2$ of the confounder with the outcome, given $X$ and $D$, is $1$. The user may alternatively think this is too harsh, and that the partial $R^2$ of the confounder hiding in the residual could be at most $0.30$, for exampl. We can know ask how such a confounder would need to relate to the treatment, in terms of partial $R^2$ as well, in order to change our substantive conclusions by a given amount.  Users may have a research design such that they know a partial $R^2$ with the treatment of even 0.10 is unlikely, because they have already accounted for all plausible systematic sources of treatment assignment. Or, they may have less substantive knowledge, and can only make the weaker claim that it would be very surprising for any unobservable to explain some large portinon of the treatment assignment, making the sensitivity analysis less optimistic. Thus, users are better off with more information and better research designs in these worst-case scenarios. 

## Running Example: Natural experiment on the effects of violence on attitudes among Darfurian refugees in Chad. 

Before turning to usage, we introduce the data that will be used. Throughout this vingette, we will use the `Darfur` data to explore these operations. The treatment is exposure to violence, `directlyharmed` a binary indicator for those physically injured during attacks in Darfur.  The outcome is a scale measuring pro-peace attitudes, `peacefactor`. 

We load the data:


```r
library(sensemakr)
data("darfur")
```

# Basic Usage

## Installation

The latest version of `sensemakr` can be installed by:


```r
# devtools::install_github("chadhazlett/sensemakr")
# setwd("~/projects/sensemakr_fin/sensemakr/")
# devtools::build_vignettes() 
```

We begin by showing basic functionality with the features we expect most users will use most of the time. 

The basic workflow is as follows: 

1) Fit a linear outcome model via `lm.out=lm()`. This should have your treatment and (pre-treatment) covariates on the right hand side.   

2) Create a sensemakr object, `sense.out=sensemakr(lm.out)`, which contains useful sensitivity quantitites.

3) Explore the results by use of `plot(sense.out)` and `summary(sense.out)`, or directly by functions these methods call upon.

The first step is running the linear outcome model: 


```r
lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = darfur)
```

The key covariates here for identification purposes is `village`, a factor with 690 levels indicating the village participants are from. Only conditionally upon village do we expect that `directlyharmed` is "as-if randomized". The other covariate required for identification is `female`, a binary indicator for women. The other covariates are included as auxilliary controls, but mainly for purposes of the sensitivity analysis. 

The second step is to run `sensemakr` on the `lm` object. At defaults this is simply,


```r
sense.out <- sensemakr(model=lm.out, treatment="directlyharmed")
```

The `sensemakr` object, `sense.out`, contains a variety of useful quantities, including `sensemakr\$benchmarks`, which we will explore below. In common use cases, however, investigators would not inspect this directly but would turn to the `plot` and `summary` methods.  

We turn first to the `summary` method to gain substantive insight into the sensitivity analysis itself.

## Summary and Interpretation

Sensitivity analyses describe the type of confounders that would change our conclusions about an analysis. The challenge is in understanding how credibly such a confounder can be ruled out.  The summary methods provide ample interpretive information, describing the sensitivity results in several ways to increase the odds that investigators can meaningfully evaluate whether the types of confounders describes are likely to exist. A second purpose of provide numerous, verbose descriptions is to provide clear and precise language that investigators can refer to and even cite directly to minimize misunderstandings.

The user simply applies the summary method to the `sensemakr` object,

```r
summary(sense.out)
```

```
## Sensitivity Analysis
## 
## Model: peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + female + village 
## 
## Outcome: peacefactor 
## Treatment: directlyharmed 
## Unadjusted Treatment Effect: 0.097 
## 
## *** SENSITIVITY TO UNOBSERVED CONFOUNDERS ***
## 
## ### Worst Case Scenarios ###
## 
## Considering the extreme scenarios of unobserved confounders explaining 100%, 25% of the residual variance of the outcome, they would have to, respectively, explain at least 2.19%, 8.21% of the treatment assignment to reduce the treatment effect in 100%.
## 
## ### Benchmarking ###
## 
## ---Using the covariate most strongly associated with the treatment assignment as a benchmark---
## 
## An unobserved confounder explaining as much of the treatment as 'female' (0.009)  would be able to cause at most a bias of 0.062 with an adjusted treatment effect of 0.035 in the extreme case where the confouder explains all the residual variance of the outcome (R2y = 1).
## 
## ---Using the covariate most strongly associated with the outcome as a benchmark---
## 
## An unobserved confounder as associated with the outcome as 'female' (R2y = 0.109)  would have to be at least 18.7 times as strongly associated with the treatment (reaching R2d = 0.17) in order to reduce the treatment effect by 100%
```

After recapitulating the model, the outcome variable, the treatment variable, and the (unadjusted) treatment effect estimate, the output goes on to describe and interpret the results in like of (i) worst case scenarios, and (ii) benchmark comparisons.

### Worst-case scenario summary:
The \textit{worst-case scenario} considers the sensitivity of results based on the unexplained variance. The worst case is one in which \textit{all} unexplained variation in the outcome belongs to an omitted confounder. How strongly would such an omitted variable need to be associated with the treatment in order to reduce the treatment effect estimate by some proportion `q`, such as q=0.50 (50\%) or q=1 (100\%)? By default, $q=1$, and so we are asking what would be necessary to imply the effect estimate would be zero had the hypothesized confounder been observed and included.

This worst-case scenario analysis can also be generated by a direct call to `worstcaseinterpret()`. Here we demonstrate this, changing both the value of `q` and the scenarios that are considered:


```r
worstcaseinterpret(sense.out, scenarios=c(.1,.3), q=.5)
```

```
## Considering the extreme scenarios of unobserved confounders explaining 10%, 30% of the residual variance of the outcome, they would have to, respectively, explain at least 5.29%, 1.83% of the treatment assignment to reduce the treatment effect in 50%.
```

### Benchmarking summary
The summary command above, `summary(sense.out)`, also generates interpretational text related to benchark comparisons. While the benchmark plots (below) provide additional information, these summaries describe two conclusions we expect are often of interest. 

The first block of text uses the covariate most strongly associated with the \textit{treatment} (by partial $R^2$) as the point of interest.  It supposes that a confounder exists that is equally associated with the treatment (in it's own partial $R^2$, in a model that still includes the original covariates). It then makes a worst-case assumption that such a confounder explains all of the residual outcome variance (having a partial $R^2$ of 1 with the outcome), and asks what bias would obtain.  It is thus a kind of worst-case analysis as above, but where the hypothesized relationship of the confounder to the treatment is benchmarked, using the value of the covariate most strongly related to treatment as an example. 

The second block of text uses the covariate most strongly associated with the \textit{outcome} as a benchmark. Rather than a worst-case analysis, it instead imagines a confounder with the same relationship to the outcome as this benchmark, and asks how many times more strongly it would have to be related to the outcome than this benchmark is in order to change our adjusted answer by proportion of $q$.

QUESTION: there is a mismatch between these two. Is this what we want?


Note that the benchmark interpretations provided in `summary` come from `interpret()`, which users can also call directly. 

## Basic Plotting: 
There are two plot types: (i) contourplots with benchmarks, which can take the form of point estimates, t-statistics, or lower- and upper-limits of confidence intervals, and (ii) worstcase plots. 

### Contourplots
As contourplots are the default, we begin there: 

```r
plot(sense.out)
```

![](sensemakr_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

As shown, this produces a two-dimensional contour plot. The contour lines give the value of the adjusted treatment effect estimate for each pair of partial $R^2$ parameters of the hypothesized confounder. 

These plot also by definition show each of the covariates included as a benchmark. The majority of these are small red diamonds, which in this case appear tightly clusteres in the lower-left corner.  This indicates that each has a weak relationship with both the treatment and the outcome.  The proper interpretatin of these benchmark points is that "a confounder with the same partial $R^2_D$ and partial $R^2_Y$ as a given covariate would imply an adjusted treatment effect indicated by that covariates benchmark point on the plot."  It is not quite true to say that these benchmarks indicate the adjusted effect "had that covariate been left out", because the quantities are computed as if the covariates included in the model remain in the model.

Note that "village" is cyan rather than red. This is because it represents a whole group of covariates -- the different levels of the factor variable `village`.  As described in the paper, for variables that occupy multiple columns of the design matrix (or any arbitrary grouping of variables treated as one), the sensitivity parameters computed a worst-case bounds. In this case, `village` was treated as a group by default, because it entered the model as a factor.  See advanced usage below for controlling the construction of groups and their display.

Because the clustering of benchmark points in the lower left makes it difficult to see individual points, the user may wish to plot fewer benchmarks. This can be done by specifying which to plot, as in


```r
plot(sense.out, showvars = list("pastvoted","female"))
```

![](sensemakr_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Finally, by default these contourplots show adjusted point estimates. This is equivalent to setting the `countour` argument to its default value of `estimate`. However `contour` can also be set to `t-value`, `lower bound`, or `upper bound` to show results for the t-statistic and the resulting lower and upper limits of 95\% confidence intervals respectively.  For example, we can inspect the t-values and related benchmarks across values of the sensitivity parameters by


```r
plot(sense.out, showvars = list("pastvoted","female"), contour="t-value")
```

![](sensemakr_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

For this plot, the contour line that is emphasized is at $t=1.96$ rather than zero: we are interested in where an estimate would become statistically indistinguishable from zero. Because the standard errors change as the hypothesized confounder changes, this requires more than computing a critical value from the original analysis and seeing when an adjusted estimate falls below it.

### Worst-case plots

The second main plot type is the worst-case plot:

```r
plot(sense.out, type="worst-case")
```

```
## Warning in rug(x = r2d_group, col = "cyan", lwd = 2): some values will be
## clipped
```

![](sensemakr_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Here the dashed lines show different scenarios. The most severe is the solid line, for which the entire residual from the model is hypohtesized to be a confounder (partial $R^2_Y=1$). One can then follow this line to see where it cross a designated value of the adjusted estimate, such as $0$, which we mark for convenience.  The point at which the line crosses zero correspondons to approximately 0.02 on the horizontal axis. This indicated that an unobserved confounder with partial $R^2_Y=1$, i.e. the entire residual, would need only have a partial $R^2_D$ of about 2\% in order to change the sign of the effect estimate. 

The tick marks on the horizontal axis show benchmarks -- the partial $R^2_D$ values of observed covariates, for comparison. In this case, we are fortunate that even assuming a confounder with $R^2_Y=1$, though it's $R^2_D$ would need to be only 2\% to eliminate the effect, that is approximately twice the $R^2_D$ of any observable.  Given the research design, we know that the treatment actually consists of injuries that occured during indiscriminate violence. As argued in XXX, the nature of the violence makes it difficult to imagine unoberved confounders predict the assignment of violence more strongly than observables such as  `female`.  This suggests that the result is fairly robust. 
A more conservative analysis would not assume a confounder with partial $R^2_Y=1$, but perhaps 30\%, or perhaps the same $R^2_Y$ as observed for the covariate most strongly predictive of the outcome (51\% in this case, also shown as a line.)  By worst-case plot will always show a line corresponding to the largest observed $R^2_Y$ (51\% in this case).  The remaining lines are determined by the option `scenarios`, which defaults to 100\% and 30\% (i.e. `scenarios=c(1,.3)`) as shown here.

## Directly accessing results

The quantities most relevant to these analyses can be extracted directly from the `sensemakr` object. 

Within this object, `info` contains information from the `lm` object. `treat.stats` contains information about the treatment effect and its standard error and degrees of freedom, all of which are important to sensitivity calculations,


```r
sense.out$treat.stats
```

```
##            treat   estimate         se  df
## 1 directlyharmed 0.09731582 0.02325654 783
```

More interestingly, `benchmarks` contain sensitivity parameters associated with each benchmark. A vareity of parameter types are given for each benchmark, each stored in a separate matrix


```r
ls(sense.out$benchmarks)
```

```
## [1] "benchmark_dropallvar" "benchmark_eachvar"    "benchmark_group"     
## [4] "benchmark_masked"     "benchmark_natural"
```

The `\_dropallvar` variable contains useful information for computing sensitivity parameters, but the quantities of usual interest will be stored in `benchmark_eachvar` and `benchmark_masked`. Both contain similar information, but when grouped variables (such as `village`) are included in the model, looking at `benchmark\_masked` conveniently masks the grouped variables (620 separate indicators in this case):


```r
round(sense.out$benchmarks$benchmark_masked[,-1],4)
```

```
##                  r2y    r2d bias_r2 adj_est_r2 adj_se_r2 adj_t_r2
## female        0.1090 0.0091  0.0206     0.0767    0.0221   3.4779
## age           0.0080 0.0011  0.0019     0.0954    0.0232   4.1123
## pastvoted     0.0051 0.0015  0.0018     0.0955    0.0232   4.1123
## herder_dar    0.0003 0.0079  0.0009     0.0964    0.0234   4.1263
## hhsize_darfur 0.0005 0.0002  0.0002     0.0971    0.0233   4.1741
## farmer_dar    0.0024 0.0000  0.0001     0.0972    0.0232   4.1838
```

The columns `r2y` and `r2d` give the partial `R^2` with the outcome (`y`) and treatment (`d`) respectively.  The `bias\_r2` gives the bias that would be caused by a confounder with the corresponding `r2y` and `r2d`. The adjusted estimated due to this bias --  simply the original estimate minus the proposed bias -- is `adj\_est\_r2`.  Finally, the adjusted `adj\_se\_r2` and `adj\_t\_r2` gives the standard error and t-statistic after adjusting for a hypothetical confounder with the proposed characteristics.  

Finally, the parameters for the masked (grouped) variables -- the `village` dummies in this case -- are stored in `benchmark\_group`:


```r
sense.out$benchmarks$benchmark_group
```

```
##         covariate       r2y       r2d  bias_r2 adj_est_r2
## village   village 0.4242888 0.4169266 0.358447 -0.2611312
```


QUESTION: is it important that showvars accepts a list? Standard usage would seem to be `c()`. I guess its because we are overloading it by also making "masked" and "all" options?

QUESTION: should we rename `lower bound` to `lower limit` or `lower CI` or something? "bound" is too strong.

# Advanced Usage

Then go through some more customizeable stuff.  


<!--
The benchmarks  

QUESTION: Are there cases where we'd want to use a covariate as a benchmark but not have it in the model? Maybe a variable that we worry is post-tx?

# runs benchmarking etc
sense <- sensemakr(model=model, treatment="directlyharmed", benchmarks=X)

## contour plot
plot1_data <- plot(sense)
plot(sense, lim=.2)
plot2_data <- plot(sense, contour = "t-value")
plot3_data <- plot(sense, contour = "lower bound")
plot4_data <- plot(sense, contour = "upper bound")

## worst-case plot
plot5_data <- plot(sense, type = "worst-case")
# note, let's me the benchmark ticks at the bottom much more visible
# and let's reduce number of lines that show and/or label them better.

# testing verbal outputs
interpret(sense)
interpret(sense, q = 0.5) #throws warning
interpret(sense, q = 0.6) #throws warning.

summary(sense) #needs more output/ to be different from interpret().




We will show 3 examples using the darfur data. The examples sequentially expose increasing amounts of customization that a user can run for their sensitivity analysis. We will provide examples of continuous benchmarks, categorical benchmarks, and user supplied benchmark groups.


# Continuous Benchmarks

## Fit

In the outcome model, there are two continuous (or at least ordered) covariates, `age` and `hhsize_darfur`.


```r
model_cntns = lm(data = darfur,
                 peacefactor ~ directlyharmed +
                   age + hhsize_darfur)
```

## Compute


```r
sense_cntns = sensemakr(model=model_cntns,
                        treatment="directlyharmed")

str(sense_cntns$benchmarks,max.level = 1)
```

```
## List of 5
##  $ benchmark_dropallvar:'data.frame':	1 obs. of  5 variables:
##  $ benchmark_eachvar   :'data.frame':	2 obs. of  7 variables:
##  $ benchmark_group     :'data.frame':	0 obs. of  2 variables:
##  $ benchmark_masked    :'data.frame':	2 obs. of  7 variables:
##  $ benchmark_natural   :'data.frame':	2 obs. of  5 variables:
```

```r
class(sense_cntns)
```

```
## [1] "sensemade"
```

## Plot

```r
plot(sense_cntns,lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

### Show specific benchmark points from an explicit list




```r
plot(sense_cntns,showvars=list('age','hhsize_darfur'),lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
plot(sense_cntns,showvars=list('age'),lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

# Factor Benchmarks

In the outcome model, there are two continuous covariates, `age` and `hhsize_darfur`. Further, there are two categorical covariates, `female` and `village`.

## Fit


```r
model_fctr  = lm(data = darfur,
                 peacefactor ~ directlyharmed + 
                   age + hhsize_darfur +
                   female + village)
```

## Compute



```r
sense_fctr = sensemakr(model=model_fctr,
                       treatment="directlyharmed")
```

notice there is a single observation in the `$benchmark_group` item of the `sensemakr` object


```r
str(sense_fctr$benchmarks,max.level = 1)
```

```
## List of 5
##  $ benchmark_dropallvar:'data.frame':	1 obs. of  5 variables:
##  $ benchmark_eachvar   :'data.frame':	488 obs. of  7 variables:
##  $ benchmark_group     :'data.frame':	1 obs. of  5 variables:
##  $ benchmark_masked    :'data.frame':	3 obs. of  7 variables:
##  $ benchmark_natural   :'data.frame':	488 obs. of  5 variables:
```

```r
sense_fctr$benchmarks$benchmark_group
```

```
##         covariate       r2y       r2d   bias_r2 adj_est_r2
## village   village 0.4292451 0.4177674 0.3607987 -0.2649393
```

```r
row.names(sense_fctr$benchmarks$benchmark_group)
```

```
## [1] "village"
```

Village enters the linear model as a R-side ‘factor’ variable with 480 levels. These 480 levels are dummy variable columns of the design matrix. Numerically, each level is treated as a set of binary covariates (fixed effects), and so the sensemakr() call treats each of the 480 levels of the village factor as a stand alone dummy variable which results in 480 benchmark points.

These 'low-level' quantities are stored in the `$benchmarks$benchmark_eachvar` item of the `sensemakr` object


```r
str(sense_fctr$benchmarks$benchmark_eachvar)
```

```
## 'data.frame':	488 obs. of  7 variables:
##  $ covariate : chr  "female" "villageTukul Tukul" "villageAroom" "villageDamra" ...
##  $ r2y       : num  0.12111 0.00808 0.00825 0.00523 0.00621 ...
##  $ r2d       : num  0.00806 0.00464 0.00407 0.00497 0.00406 ...
##  $ bias_r2   : num  0.02039 0.00399 0.00377 0.00332 0.00327 ...
##  $ adj_est_r2: num  0.0755 0.0919 0.0921 0.0925 0.0926 ...
##  $ adj_se_r2 : num  0.0218 0.0232 0.0232 0.0232 0.0232 ...
##  $ adj_t_r2  : num  3.46 3.97 3.98 3.99 3.99 ...
```

Although we have 480 dummy variable sensitivity estimates for the village factor levels, we would like to understand the sensitivity when benchamrked to the entire single factor variable as a group

notice that we have also grouped all 480 levels into the single `village` term stored in `str(sense_fctr$benchmarks$benchmark_group)`


```r
str(sense_fctr$benchmarks$benchmark_group)
```

```
## 'data.frame':	1 obs. of  5 variables:
##  $ covariate : chr "village"
##  $ r2y       : num 0.429
##  $ r2d       : num 0.418
##  $ bias_r2   : num 0.361
##  $ adj_est_r2: num -0.265
```
We have also masked out the 480 levels from `str(sense_fctr$benchmarks$benchmark_masked)`


```r
str(sense_fctr$benchmarks$benchmark_masked)
```

```
## 'data.frame':	3 obs. of  7 variables:
##  $ covariate : chr  "female" "age" "hhsize_darfur"
##  $ r2y       : num  0.121109 0.008557 0.000691
##  $ r2d       : num  8.06e-03 8.24e-04 5.38e-06
##  $ bias_r2   : num  2.04e-02 1.73e-03 3.96e-05
##  $ adj_est_r2: num  0.0755 0.0941 0.0958
##  $ adj_se_r2 : num  0.0218 0.0231 0.0232
##  $ adj_t_r2  : num  3.46 4.07 4.13
```

## Plot

Show all low level (design matrix) benchmark points



```r
plot(sense_fctr,showvars='all',lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

Above, we immediately see the reason why using many dummy variables (for a single factor) can pose a challenge for interpretation, visually and contextually.

Contextually, binary contrast columns (for each level) should be viewed as a single grouped ‘village’ factor. Visually, we only want to plot the single `village` factor and mask out its associated 480 individual levels.

Therefore, we have designated `showvars='masked'` as the default option in `plot(...,showvars='masked')`


```r
# plot(sense_fctr,showvars='masked',lim=0.02)
# same as

plot(sense_fctr,lim=0.5)
```

![](sensemakr_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

### Show specific benchmark points from an explicit list

Show the three benchmarks:

* the `village` factor group
* the `villageMngao` level of the village factor
* the continuous covariate `age`




```r
par(mfrow=c(2,2))

plot(sense_fctr,showvars=list('village','villageMngao','age'),lim=0.5)


plot(sense_fctr,showvars=list('village','villageMngao','age'),lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-28-1.png)<!-- -->


# User Specified Groups as Benchmarks

In the outcome model, there are two continuous covariates, `age` and `hhsize_darfur`. Further, there are two categorical covariates, `female` and `village`. Lastly, we note that the user specified group will not be supplied here in the `lm()` step, but after in the following `sensemakr()` step.

## Fit


```r
model_cust_grp  = lm(data = darfur,
                     peacefactor ~ directlyharmed + 
                       age + hhsize_darfur +
                       female + village
                     )
```

Notice for this example, we are using the same resulting model fit that was used in the previous example


```r
identical(model_cust_grp,model_fctr)
```

```
## [1] TRUE
```

## Compute

Although the model fit step was similar, if the user wishes to compute sensitvitiy quantities for a custom grouping, the grouping must be specified during the `sensemakr()` call using the optional `group_list` argument. As in `sensemakr(...,group_list = list(c('village','female'),'age'))`.

The example `group_list = list(c('village','female'),'age')` has two groups

1) `village` with `female`
2) `age` by itself

The `group_list` argument expects a list of character vectors. The terms in a character vector comprise a single group. 



```r
sense_cust_grp = sensemakr(model=model_cust_grp,
                           treatment="directlyharmed",
                           group_list = list(c('village','female'),'age')
                           )

str(sense_cust_grp$benchmarks,max.level = 1)
```

```
## List of 5
##  $ benchmark_dropallvar:'data.frame':	1 obs. of  5 variables:
##  $ benchmark_eachvar   :'data.frame':	488 obs. of  7 variables:
##  $ benchmark_group     :'data.frame':	3 obs. of  5 variables:
##  $ benchmark_masked    :'data.frame':	1 obs. of  7 variables:
##  $ benchmark_natural   :'data.frame':	488 obs. of  5 variables:
```

The sensitivity quantities are stored in the `$benchmarks$benchmark_group` item of the `sensemakr` object


```r
names(sense_cust_grp$benchmarks)
```

```
## [1] "benchmark_dropallvar" "benchmark_eachvar"    "benchmark_group"     
## [4] "benchmark_masked"     "benchmark_natural"
```

```r
head(sense_cust_grp$benchmarks$benchmark_masked)
```

```
##                   covariate          r2y          r2d     bias_r2
## hhsize_darfur hhsize_darfur 0.0006910575 5.375787e-06 3.96253e-05
##               adj_est_r2  adj_se_r2 adj_t_r2
## hhsize_darfur 0.09581978 0.02319581 4.130909
```

```r
head(sense_cust_grp$benchmarks$benchmark_group)
```

```
##                     covariate         r2y          r2d     bias_r2
## village,female village,female 0.502009923 0.4188963714 0.391089300
## age                       age 0.008557023 0.0008244527 0.001727494
## village               village 0.429245072 0.4177674440 0.360798722
##                 adj_est_r2
## village,female -0.29522989
## age             0.09413191
## village        -0.26493931
```




Terms, eligible for grouping, must be the same terms present in the initial outcome regression model stored in the `lm` object.


```r
class(model_cust_grp)
```

```
## [1] "lm"
```

```r
colnames(attr(terms(formula(model_cust_grp)),'factor'))
```

```
## [1] "directlyharmed" "age"            "hhsize_darfur"  "female"        
## [5] "village"
```




## Plot

### Plot with showvars=‘all’


```r
plot(sense_cust_grp,showvars='all',lim=0.5)
```

![](sensemakr_files/figure-html/unnamed-chunk-34-1.png)<!-- -->


### Plot with showvars=‘masked’


```r
plot(sense_cust_grp)
```

![](sensemakr_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

From the plot (with default showvars='masked'), notice that:

* ‘village,female’ is plotted 
* ‘female’ is not plotted, 
* ‘village’ (group) is plotted, 
* the 480 factor levels of ‘village’ are not plotted
* 'age' is plotted as cyan

#### Explaining showvars='masked' vs showvars='all'

Depending on the showvars option, the plot methods `plot(...,showvars='masked')` (default) or `plot(...,showvars='all')` will only display two sets of points, 

1) `benchmark_group` (displayed in both showvars options) 
and
2) either of `benchmark_eachvar` or `benchmark_masked`

The terms in `benchmark_eachvar` are plotted if `plot(...,showvars='all')`
The terms in `benchmark_masked` are plotted if `plot(...,showvars='masked')`. 

All sensitivity quantities, related to the columns of the linear model's model matrix, are stored in `benchmark_eachvar`. All terms in `benchmark_eachvar` get plotted if `plot(...,showvars='all')`.

The terms in `benchmark_group` contain any factor variables (the levels of a factor variable grouped together) and any groups specified by the user in the `group_list` argument of `sensemakr()`.

The terms in `benchmark_masked` form a subset of `benchmark_eachvar`. If a term is in `benchmark_eachvar` that is also part of a term in `benchmark_group`, then it does **not** get copied into `benchmark_masked`.
If a term is in `benchmark_group`, it is not eligible to be copied into `benchmark_masked`. That is, `benchmark_masked` is the set complement of `benchmark_eachvar` anti-joined against `benchmark_group`. 

For example, 

1) The `female` term is stored in `benchmark_eachvar` and is also part of the `c("village","female")` group. Therefore `female` is **not** stored in `benchmark_masked` and is not displayed in the default `plot(...,showvars='masked')`.

2) The factor levels of `village` are not stored in `benchmark_masked` since they are in `benchmark_eachvar` but also part of the group `village`. Therefore the factor levels of village are not displayed in the default `plot(...,showvars='masked')`. 

3) The `village` term itself is a standalone group comprised of 480 levels hence present in `benchmarks_group`. In the examples before, we showed that the internals of `sensemakr()` enforced factor levels to be treated as a group. Therefore the single group term `village` is displayed as cyan in both `showvars='masked'` and `showvars='all'` options. 

4) The single term `age` is part of the second user specified group in `group_list = list(c('village','female'),'age')` . Therefore the single group term `age` is displayed as cyan in both `showvars='masked'` and `showvars='all'` options. 


There may be a situation where the user wants to plot `village,female` (a user specified group) but not plot `village` (a sensemakr-enforced group). More generally, If the user wishes to **not** plot specific terms in `benchmark_group` they must do it explicily themselves via the third showvars option, `plot(...,showvars=list())`.


### Plot with specific list items via showvars=list()

When choosing explicit groups to plot with the `showvars` argument of `plot()`, you must supply a `list()` whose entries are single character vectors that represent the specific groups you wish to plot. A single string must concatenate all the terms belonging to a single group (seperated by a spaceless comma).

For example, the plot command `plot(...,showvars=list(‘village,female’))` corresponds with the compute command `sensemakr(...,group_list = list(c('village','female')))`

As a result, only the single grouping `village` with `female` is plotted. Notice how `village,female` is plotted but **not** `village`.



```r
plot(sense_cust_grp,showvars=list('village,female'),lim=0.5)
```

![](sensemakr_files/figure-html/unnamed-chunk-36-1.png)<!-- -->





# Other Helpful Plots

## Other Contours

with default showvars=‘masked’


```r
par(mfrow=c(2,2))

plot(sense_cust_grp, lim=.2)
plot(sense_cust_grp, contour = "t-value")
plot(sense_cust_grp, contour = "lower bound")
plot(sense_cust_grp, contour = "upper bound")
```

![](sensemakr_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

with showvars=‘all’


```r
par(mfrow=c(2,2))

plot(sense_cust_grp, contour = "t-value",showvars='all')
plot(sense_cust_grp, contour = "t-value",showvars='all',lim=0.02)
plot(sense_cust_grp, contour = "lower bound",showvars='all',lim=0.02)
plot(sense_cust_grp, contour = "upper bound",showvars='all',lim=0.02)
```

![](sensemakr_files/figure-html/unnamed-chunk-38-1.png)<!-- -->


with showvars explicit



```r
par(mfrow=c(2,2))

plot(sense_cust_grp, contour = "t-value",
     showvars=list('village','villageMngao','age'))

plot(sense_cust_grp, contour = "lower bound",
     showvars=list('village','villageMngao','age'))

plot(sense_cust_grp, contour = "upper bound",
     showvars=list('village','villageMngao','age'))
```

![](sensemakr_files/figure-html/unnamed-chunk-39-1.png)<!-- -->


## Worst Case Plot


```r
par(mfrow=c(2,2))

plot(sense_cust_grp, type = "worst-case")
```

```
## Warning in rug(x = r2d_group, col = "cyan", lwd = 2): some values will be
## clipped
```

```r
plot(sense_cust_grp, type = "worst-case",showvars='masked',lim=0.5)

plot(sense_cust_grp, type = "worst-case",showvars='all',lim=0.5)

plot(sense_cust_grp, type = "worst-case",lim=0.5,
     showvars=list('village','villageMngao','age'))
```

![](sensemakr_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

-->
