#' Calculates an RV (robustness value) statistic
#'
#' This function calculates the robustness value statistic described in "Make
#' Sense of Sensitivity". The robustness value describes the percentage of
#' residual variation in the treatment and outcome an unobserved confounder
#' would need to attenuate an observed effect size past a critical value
#'
#' Although exported, we recommend that users primarily access this
#' functionality through creating `sensemakr` objects. See \link{sensemakr} for
#' information on creating `sensemakr` objects.
#'
#' @param model An `lm` object which will be used to produce a robustness value
#' @param covariate A quoted character string describing the name of the
#' treatment covariate in the model object.
#' @param q A numeric fraction between 0 and 1 describing q% attenuation of the
#' observed treatment effect, defaults to 1 (complete attenuation)
#' @param alpha If specified, denotes a (1 - alpha) confidence interval used to
#' calculate the RV. If specified, the RV will reflect the percentage of
#' residual variation in the treatment/outcome that must be explained in order
#' for a (1 - alpha) confidence interval about the effect to cross the critical
#' threshold implied by `q`. If left empty or NULL, the point estimate must
#' cross the critical threshold.
#' @param t_statistic In lieu of supplying `model` and `covariate`, supply the
#' t-statistic of the treatment effect and `dof`
#' @param dof In lieu of supplying `model` and `covariate`, supply the residual
#' degrees of freedom of a model and `t_statistic`.
#' @param ... Additional parameters, not currently used.
#'
#' @examples
#' @examples
#' # Creating a sensemakr object using the built-in `darfur` data: this is the
#' # fastest way to calculate the robustness value.
#' data(darfur)
#' sense.out = sensemakr(formula = peacefactor ~ directlyharmed + female +
#'                         village + age,
#'                       data = darfur,
#'                       treatment = "directlyharmed",
#'                       benchmark = "female")
#'
#' # Manually calculating a robustness value using t-statistic and degrees of
#' # freedom
#' robustness_value(t_statistic = 4.18445, dof = 783)
#'
#' # Passing the function a model and covariate
#' model.out = lm(peacefactor ~ directlyharmed + female + village + age,
#'               data = darfur)
#' robustness_value(model = model.out, covariate = "directlyharmed")
#'
#' # Using an alternate q: How robust is our model to 50% attenuation?
#' robustness_value(t_statistic = 4.18445, dof = 783, q = 0.5)
#'
#' # Using an alternate alpha: How robust is our confidence interval to
#' # crossing 0?
#' robustness_value(t_statistic = 4.18445, dof = 783, alpha = 0.05)
#'
#' @return A q-robustness value, which is a number from 0 to 1.
#' @importFrom stats qt
#' @export
robustness_value = function(
  model = NULL, covariate = NULL,
  q = 1, alpha = NULL, ...,
  t_statistic = NULL, dof = NULL) {

  # Error: user didn't pass what we need
  if(is.null(model) && (is.null(t_statistic) || is.null(dof))) {
    stop("You must provide either a model object `model` or both `t_statistic`",
         " and `dof` arguments.")
  }

  # Error: q non-numeric or out of bounds
  if(!is.numeric(q) || length(q) > 1 || q < 0 || q > 1) {
    stop("The `q` parameter must be a numeric quantity between 0 and 1.")
  }

  # Error: alpha, if provided, was non-numeric or out of bounds
  if(!is.null(alpha) && (!is.numeric(alpha) || length(alpha) > 1 ||
                         alpha < 0 || alpha > 1)) {
    stop("The `alpha` parameter, if provided, must be a numeric quantity ",
         "between 0 and 1.")
  }

  # User passed a model object: calculate t and dof
  if(!is.null(model)) {
    if(is.null(covariate)) {
      stop("If supplying a model, the `covariate` parameter must be specified.")
    }
    model_results = model_helper(model)
    t_statistic =
      model_results$estimates[covariate] / model_results$se[covariate]
    dof = model_results$degrees_of_freedom
  }

  # Error: t-statistic is not a single number.
  if(!is.numeric(t_statistic) || length(t_statistic) > 1) {
    stop("The `t_statistic` parameter must be a single number.")
  }

  # Calculate RV
  # QF is q * absolute value(partial f). We can get partial f from t for cov.
  # of interest.
  qf = q * abs(t_statistic / sqrt(dof))

  # If we have an alpha value (i.e. we want to know whether the result will be
  # significant, rather than whether it will be >0) we need to adjust the qf
  # accordingly. [eqn. 18 from "Making Sense of Sensitivity"]
  if(!is.null(alpha)) {
    critical_t = abs(qt(alpha / 2,
                        df = dof - 1))
    qf = qf - (critical_t / sqrt(dof - 1))
  }

  # Eqn. 19 from "Making Sense of Sensitivity"
  0.5 * (sqrt(qf^4 + (4 * qf^2)) - qf^2)
}

model_helper = function(model) {
  UseMethod("model_helper", model)
}

model_helper.default = function(model) {
  stop("The `r_squared_helper` function must be passed an `lm` model object. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

model_helper.lm = function(model) {
  # Quickly extract things from an lm object
  list(
    estimates = coef(summary(model))[, "Estimate"],
    se = coef(summary(model))[, "Std. Error"],
    degrees_of_freedom = model$df.residual,
    r2 = r_squared_helper(model)
  )
}
