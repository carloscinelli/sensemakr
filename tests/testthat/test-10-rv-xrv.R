context("Tests for the RV")


test_that("Non contraining cases (XRV)",
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

          }
)
