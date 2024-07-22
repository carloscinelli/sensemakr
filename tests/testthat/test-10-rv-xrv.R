context("Tests for the RV")

# is.interior <- function(t_statistic, dof, q, alpha, invert){
#   fq  <-  q * abs(t_statistic / sqrt(dof))
#   f.crit <- abs(qt(alpha / 2, df = dof - 1)) / sqrt(dof - 1)
#
#   if (invert) { # invert case
#     f1 <- f.crit
#     f2 <- fq
#   } else { # RV case
#     f1 <- fq
#     f2 <- f.crit
#   }
#   fqa <- f1 - f2 # computes fqa
#   return(fqa > 0 & f1 > 1/f2)
# }

# XRV: invert = FALSE
test_that("Original non constraining case (XRV)",
          {

            rm(list = ls())
            dof <- 7
            tcrit <- qt(p = 0.975, df = dof - 1)
            fs <- tcrit/sqrt(dof - 1)

            estimate <- 1
            se <- .3
            t <- estimate/se
            f <- t/sqrt(dof)

            rv <- robustness_value(t, dof = dof, alpha = 0.05)


            # optimal point
            r2dz.x  <- rv
            r2yz.dx <- r2dz.x/(fs^2 + r2dz.x)
            t_at_point <- adjusted_t(estimate = estimate, se = se,
                                     dof = dof,
                                     r2dz.x = r2dz.x,
                                     r2yz.dx = r2yz.dx)
            # needs to be exact here
            expect_equivalent(t_at_point, tcrit)

            # checks if not confounder weaker can bring below tcrit
            r2dz.x_seq <- seq(0.0001, r2dz.x, by = 0.0001)
            r2yz.dx_seq <- seq(0.0001, r2yz.dx, by = 0.0001)
            checks <- expand.grid(r2dz.x_seq = r2dz.x_seq, r2yz.dx_seq = r2yz.dx_seq)
            ts_to_check <- adjusted_t(estimate = estimate,
                                      se = se,
                                      dof = dof,
                                      r2dz.x = checks$r2dz.x_seq,
                                      r2yz.dx = checks$r2yz.dx_seq)

            # should be true: all ts are larger than tcrit
            expect_true(all(ts_to_check > tcrit))

            expect_error(robustness_value(t = 2, dof = 100, alpha = NULL))

          }
)

# test function: RV and XRV
alpha = 0.05
test_rv <- function(estimate, se, dof, invert, q) {
  t_statistic <- estimate/se
  nn <- length(t_statistic)
  tcrit <- qt(p = 0.975, df = dof - 1)
  fs <- tcrit/sqrt(dof - 1)
  rv.cur <- robustness_value.numeric(t_statistic, dof = dof,
                                     alpha = alpha, q=q,
                                     invert=invert)

  is.xrv = attr(rv.cur,"is.xrv")
  # optimal point
  if (!invert) { # if RV
    r2dz.x  <- rv.cur
    r2yz.dx <- rv.cur
    r2yz.dx[is.xrv] <- r2dz.x/(fs^2 + r2dz.x)

    for (ii in 1:nn) {
      t_at_point <- adjusted_t.numeric(estimate = estimate[ii],
                                       se = se[ii],
                                       dof = dof[ii],
                                       r2dz.x = r2dz.x[ii],
                                       r2yz.dx = r2yz.dx[ii],
                                       h0 = (1-q)*estimate[ii],
                                       reduce=!invert)
      # needs to be exact here
      expect_equal(as.numeric(t_at_point), tcrit[ii], tolerance=1e-4)

      # checks if no confounder weaker can bring below/above tcrit
      r2dz.x_seq <- seq(0, r2dz.x[ii], by = 0.0001)
      r2yz.dx_seq <- seq(0, r2yz.dx[ii], by = 0.0001)
      checks <- expand.grid(r2dz.x_seq = r2dz.x_seq,
                            r2yz.dx_seq = r2yz.dx_seq)
      ts_to_check <- adjusted_t.numeric(estimate = estimate[ii],
                                        se = se[ii],
                                        dof = dof[ii],
                                        r2dz.x = checks$r2dz.x_seq,
                                        r2yz.dx = checks$r2yz.dx_seq,
                                        h0 = (1-q)*estimate[ii],
                                        reduce=!invert)

      # should be true: all ts are larger than tcrit
      expect_true(all(ts_to_check > tcrit[ii]))
    }
  } else { # if IRV
    r2dz.x  <- rv.cur
    r2yz.dx <- rv.cur
    r2dz.x[is.xrv] <- r2yz.dx/(fs^2 + r2yz.dx)

    # checks if no confounder weaker can bring below/above tcrit
    for (ii in 1:nn) {
      t_at_point <- adjusted_t.numeric(estimate = estimate[ii],
                                       se = se[ii],
                                       dof = dof[ii],
                                       r2dz.x = r2dz.x[ii],
                                       r2yz.dx = r2yz.dx[ii],
                                       h0 = (1-q)*estimate[ii],
                                       reduce=!invert)

      expect_equal(as.numeric(t_at_point), tcrit[ii], tolerance=1e-4)

      r2dz.x_seq <- seq(0.0001, r2dz.x[ii], by = 0.0001)
      r2yz.dx_seq <- seq(0.0001, r2yz.dx[ii], by = 0.0001)
      checks <- expand.grid(r2dz.x_seq = r2dz.x_seq,
                            r2yz.dx_seq = r2yz.dx_seq)
      ts_to_check <- adjusted_t.numeric(estimate = estimate[ii],
                                        se = se[ii],
                                        dof = dof[ii],
                                        r2dz.x = checks$r2dz.x_seq,
                                        r2yz.dx = checks$r2yz.dx_seq,
                                        h0 = (1-q)*estimate[ii],
                                        reduce=!invert)

      # should be true: all ts are larger than tcrit
      expect_true(all(ts_to_check < tcrit[ii]))
    }
  }
}

# cases where invert = TRUE

# XRV (interior case): uncomment is.interior() to verify
#is.interior(1/.3, dof=7, q=1, alpha=0.05, invert=FALSE)
#is.interior(1/.3, dof=7, q=0.9, alpha=0.05, invert=FALSE)
test_that("More non-constraining cases (XRV): vectors, q != 1", {
  test_rv(estimate=1, se=.3, dof=7, invert=FALSE, q=1)
  test_rv(estimate=c(1,1), se=c(.3,.2), dof=c(7,7), invert=FALSE, q=0.9)
  test_rv(estimate=1, se=.3, dof=7, invert=FALSE, q=0.9)
  }
)


# RV
#is.interior(1/.3, dof=7, q=0.7, alpha=0.05, invert=FALSE)
#is.interior(6, dof=20, q=1, alpha=0.05, invert=FALSE)
test_that("Constraining cases (RV): q != 1", {
  test_rv(estimate=6, se=1, dof=20, q=1, invert=FALSE)
  test_rv(estimate=2, se=0.3, dof=20, q=2, invert=FALSE)
  }
)

# cases where invert = FALSE

# XIRV (interior case)
#is.interior(2, dof=5, q=1.5, alpha=0.05, invert=TRUE)
test_that("Non-constraining cases (XIRV): q != 1, vectors", {
  test_rv(estimate=2, se=1, dof=5, q=1.5, invert=TRUE)
  test_rv(estimate=c(2,1), se=c(1,0.5), dof=c(5,5), q=1.5, invert=TRUE)
  }
)

# IRV (bound case)
#is.interior(2, dof=20, q=1, alpha=0.05, invert=TRUE)
#is.interior(1.5, dof=20, q=1, alpha=0.05, invert=TRUE)
#is.interior(1, dof=100, q=0.5, alpha=0.05, invert=TRUE)
test_that("Constraining cases (IRV): q != 1, vectors", {
  test_rv(estimate=2, se=1, dof=20, q=1, invert=TRUE)
  test_rv(estimate=1.5, se=1, dof=100, q=1, invert=TRUE)
  test_rv(estimate=c(1,1.4), se=c(1,1), dof=c(100,24), q=0.5, invert=TRUE)
  }
)


