% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensemakr.R
\name{sensemakr}
\alias{sensemakr}
\alias{sensemakr.lm}
\alias{sensemakr.fixest}
\alias{sensemakr.formula}
\alias{sensemakr.numeric}
\title{Sensitivity analysis to unobserved confounders}
\usage{
sensemakr(...)

\method{sensemakr}{lm}(
  model,
  treatment,
  benchmark_covariates = NULL,
  kd = 1,
  ky = kd,
  q = 1,
  alpha = 0.05,
  r2dz.x = NULL,
  r2yz.dx = r2dz.x,
  bound_label = "Manual Bound",
  reduce = TRUE,
  ...
)

\method{sensemakr}{fixest}(
  model,
  treatment,
  benchmark_covariates = NULL,
  kd = 1,
  ky = kd,
  q = 1,
  alpha = 0.05,
  r2dz.x = NULL,
  r2yz.dx = r2dz.x,
  bound_label = "Manual Bound",
  reduce = TRUE,
  ...
)

\method{sensemakr}{formula}(
  formula,
  method = c("lm", "feols"),
  vcov = "iid",
  data,
  treatment,
  benchmark_covariates = NULL,
  kd = 1,
  ky = kd,
  q = 1,
  alpha = 0.05,
  r2dz.x = NULL,
  r2yz.dx = r2dz.x,
  bound_label = "",
  reduce = TRUE,
  ...
)

\method{sensemakr}{numeric}(
  estimate,
  se,
  dof,
  treatment = "D",
  q = 1,
  alpha = 0.05,
  r2dz.x = NULL,
  r2yz.dx = r2dz.x,
  bound_label = "manual_bound",
  r2dxj.x = NULL,
  r2yxj.dx = r2dxj.x,
  benchmark_covariates = "manual_benchmark",
  kd = 1,
  ky = kd,
  reduce = TRUE,
  ...
)
}
\arguments{
\item{...}{arguments passed to other methods.}

\item{model}{An \code{lm} or \code{fixest} object with the outcome regression.}

\item{treatment}{A character vector with the name of the treatment variable
of the model.}

\item{benchmark_covariates}{The user has two options: (i)  character vector of the names of covariates that will be used to bound the plausible strength of the unobserved confounders. Each variable will be considered separately; (ii) a named list with character vector names of covariates that will be used, \emph{as a group}, to bound the plausible strength of the unobserved confounders. The names of the list will be used for the benchmark labels. Note: for factor variables with more than two levels, you need to provide the name of each level as encoded in the \code{fixest} model (the columns of \code{model.matrix}).}

\item{kd}{numeric vector. Parameterizes how many times stronger the confounder is related to the treatment in comparison to the observed benchmark covariate.
Default value is \code{1} (confounder is as strong as benchmark covariate).}

\item{ky}{numeric vector. Parameterizes how many times stronger the confounder is related to the outcome in comparison to the observed benchmark covariate.
Default value is the same as \code{kd}.}

\item{q}{percent change of the effect estimate that would be deemed problematic.  Default is \code{1},
which means a reduction of 100\% of the current effect estimate (bring estimate to zero). It has to be greater than zero.}

\item{alpha}{significance level.}

\item{r2dz.x}{hypothetical partial R2 of unobserved confounder
Z with treatment D, given covariates X.}

\item{r2yz.dx}{hypothetical partial R2 of unobserved confounder Z
with outcome Y, given covariates X and treatment D.}

\item{bound_label}{label to bounds provided manually in \code{r2dz.x} and \code{r2yz.dx}.}

\item{reduce}{should the bias adjustment reduce or increase the
absolute value of the estimated coefficient? Default is \code{TRUE}.}

\item{formula}{an object of the class \code{\link{formula}}: a symbolic description of the model to be fitted.}

\item{method}{the default is \code{\link{lm}}. This argument can be changed to estimate the model using \code{\link[fixest]{feols}}. In this case the formula needs to be written so it can be estimated with \code{\link[fixest]{feols}} and the package needs to be installed.}

\item{vcov}{the variance/covariance used in the estimation when using \code{\link[fixest]{feols}}. See \code{\link[fixest]{vcov.fixest}} for more details. Defaults to "iid".}

\item{data}{data needed only when you pass a formula as first parameter. An object of the class \code{\link{data.frame}} containing the variables used in the analysis.}

\item{estimate}{Coefficient estimate.}

\item{se}{standard error of the coefficient estimate.}

\item{dof}{residual degrees of freedom of the regression.}

\item{r2dxj.x}{partial R2 of covariate Xj with the treatment D (after partialling out the effect of the remaining covariates X, excluding Xj).}

\item{r2yxj.dx}{partial R2 of covariate Xj with the outcome Y (after partialling out the effect of the remaining covariates X, excluding Xj).}
}
\value{
An object of class \code{sensemakr}, containing:
\describe{
 \item{ \code{info} }{A \code{data.frame} with the general information of the analysis, including the formula used, the name of the treatment variable, parameter values such as \code{q}, \code{alpha}, and whether the bias is assumed to reduce the current estimate. }
 \item{ \code{sensitivity_stats} }{A \code{data.frame} with the sensitivity statistics for the treatment variable, as computed by the function \code{\link{sensitivity_stats}}.}
 \item{ \code{bounds} }{A \code{data.frame} with bounds on the strength of confounding according to some benchmark covariates, as computed by the function \code{\link{ovb_bounds}}.}
 }
}
\description{
This function performs sensitivity analysis to omitted variables as discussed in Cinelli and Hazlett (2020). It returns an object of
class \code{sensemakr} with several pre-computed sensitivity statistics for reporting.
After running \code{sensemakr} you may directly use the \code{plot}, \code{print} and \code{summary} methods in the returned object.
}
\examples{
# loads dataset
data("darfur")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                         pastvoted + hhsize_darfur + female + village, data = darfur)

# runs sensemakr for sensitivity analysis
sensitivity <- sensemakr(model, treatment = "directlyharmed",
                               benchmark_covariates = "female",
                               kd = 1:3)
# short description of results
sensitivity

# long description of results
summary(sensitivity)

# plot bias contour of point estimate
plot(sensitivity)

# plot bias contour of t-value
plot(sensitivity, sensitivity.of = "t-value")

# plot bias contour of lower limit of CI
plot(sensitivity, sensitivity.of = "lwr")

# plot bias contour of upper limit of CI
plot(sensitivity, sensitivity.of = "upr")

# plot extreme scenario
plot(sensitivity, type = "extreme")

# latex code for sensitivity table
ovb_minimal_reporting(sensitivity)

}
\references{
Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
}
\seealso{
The function \code{sensemakr} is a convenience function. You may use the other sensitivity functions of the package directly, such as the functions for sensitivity plots
(\code{\link{ovb_contour_plot}}, \code{\link{ovb_extreme_plot}}) the functions for computing bias-adjusted estimates and t-values (\code{\link{adjusted_estimate}}, \code{\link{adjusted_t}}),
the functions for computing the robustness value and partial R2 (\code{\link{robustness_value}}, \code{\link{partial_r2}}),  or the functions for bounding the strength
of unobserved confounders (\code{\link{ovb_bounds}}), among others.
}
