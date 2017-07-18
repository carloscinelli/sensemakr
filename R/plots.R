
# graphics ----------------------------------------------------------------

##' @title Sensitivity plots
##' @description Several sensitivity plots.
##'
##' @param x sensemade object
##' @param type type of the plot
##' @param ... extra arguments
##'
##'
##' @return a ggplot object with the plot.
plot.sensemade <- function(x, type = "contour", ...){
  switch(type,
         contour = contourplot(x, ...),
         worstcase = worstcaseplot(x, ...)
  )
}
# x <- sense
# contour = "estimate"
# lim <- 0.3

contourplot <- function(x,
                        contour = c("estimate","t-value", "lower bound", "upper bound"),
                        nlevels = 15,
                        pch = 20,
                        cex = 1,
                        lim = NULL,
                        xlab = "Hypothetical partial R2 of unobserved confounder with the treatment",
                        ylab = "Hypothetical partial R2 of unobserved confounder with the outcome",
                        main = paste("Sensitivity of",  contour, "to unobserved confounder\nContours of adjusted estimates"),
                        top = 3,
                         ...){

  contour <- match.arg(contour)
  benchmarks <- x$benchmarks$benchmark_R2
  top <- min(c(nrow(benchmarks), top))
  benchmarks <- benchmarks[1:top, ]
  r2y <- benchmarks$r2y
  r2d <- benchmarks$r2d
  se <- x$treat.stats$se
  df <- x$treat.stats$df
  estimate <- x$treat.stats$estimate
  t <- estimate/se

  if (is.null(lim)) lim <- max(c(r2y, r2d), na.rm = TRUE) + 0.1
  s <- seq(0, lim, by = 0.001)

  if (contour == "estimate") {
    z <- adjust_estimate(estimate, outer(s, s, getbiasR2, se = se, df = df))
    contour(s, s, z, nlevels = nlevels,
            xlab = xlab,
            ylab = ylab,
            main = main)
    points(r2d, r2y, pch = 23, col = "black", bg = "red", cex = cex)
    contour(s, s, z = z, level = 0, add = TRUE, col = "red", lwd = 2, lty = 2)
    labels <- paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_est_r2, 3),")")
    r2dl <- jitter(r2d, factor = 20)
    r2yl <- jitter(r2y, factor = 20)
    text(r2dl, r2yl, labels = labels, cex = 0.7)
  } else if (contour == "t-value") {
    z <-  outer(s, s, gettR2, t = t, df = df)
    contour(s, s, z, nlevels = nlevels,
            xlab = xlab,
            ylab = ylab,
            main = main)
    points(r2d, r2y, pch = 23, col = "black", bg = "red", cex = cex)
    labels <- paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_t_r2, 3),")")
    contour(s, s, z = z, level = 2, add = TRUE, col = "red", lwd = 2, lty = 2)
    r2dl <- jitter(r2d, factor = 20)
    r2yl <- jitter(r2y, factor = 20)
    text(r2dl, r2yl, labels = labels, cex = 0.7)
  } else if (contour == "lower bound" | contour == "upper bound" ) {
    new_estimate <- adjust_estimate(estimate, outer(s, s, getbiasR2, se = se, df = df))
    new_se       <- outer(s, s, getseR2, se = se, df = df)
    if (contour == "lower bound") {
      z <- new_estimate - 2*new_se
      benchmarks$adj_lw_r2 <- benchmarks$adj_est_r2 - 2*benchmarks$adj_se_r2
      labs <- benchmarks$adj_lw_r2
    } else {
      z <- new_estimate + 2*new_se
      benchmarks$adj_up_r2 <- benchmarks$adj_est_r2 + 2*benchmarks$adj_se_r2
      labs <- benchmarks$adj_up_r2
    }
    contour(s, s, z, nlevels = nlevels,
            xlab = xlab,
            ylab = ylab,
            main = main)
    points(r2d, r2y, pch = 23, col = "black", bg = "red", cex = cex)
    labels <- paste0(benchmarks$covariate, "\n", "(",round(labs, 3),")")
    contour(s, s, z = z, level = 0, add = TRUE, col = "red", lwd = 2, lty = 2)
    r2dl <- jitter(r2d, factor = 20)
    r2yl <- jitter(r2y, factor = 20)
    text(r2dl, r2yl, labels = labels, cex = 0.7)

  }

  labels <- data.frame(labels = labels,x = r2dl, y = r2yl, stringsAsFactors = FALSE)
  rownames(z) <- colnames(z) <- s
  out <- list(plot_type = paste(contour,"contours"),
              contours = z,
              benchmarks = benchmarks,
              labels = labels)
  invisible(out)
}

# sense <- sensemakr(model, D, X)
# debugonce(sensemakr)
# data <- contourplot(sense, lim = 0.3)
# data <- contourplot(sense, contour = "t-value")
# data <- contourplot(sense, contour = "upper bound")
# data <- contourplot(sense, contour = "lower bound")
# data <- worstcaseplot(sense,  scenarios = c(1, 0.8, 0.5))

x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
a <- contourLines(x, y, volcano)

worstcaseplot <- function(x,
                          lim = NULL,
                          scenarios = c(1, 0.8, 0.5),
                          cex.legend = 0.5,
                          index = NULL, ...){
  benchmarks <- x$benchmarks$benchmark_R2
  r2d <- benchmarks$r2d

  if (!is.null(index)) r2d <- r2d[index]

  estimate <- x$treat.stats$estimate
  se  <- x$treat.stats$se
  df <- x$treat.stats$df

  if (is.null(lim)) lim <- max(r2d, na.rm = TRUE) + 0.1

  s <- seq(0, lim, by = 0.001)
  y <- adjust_estimate(estimate, getbiasR2(se = se, df = df, r2y = scenarios[1], r2d = s))

  plot(s,  y,
       type = "l", bty = "L", xlab = "Hypothetical partial R2 of unobserved confounder(s) with treatment",
       ylab = "Adjusted estimate",
       main = "Sensitivity of estimate to unobserved confounder(s)\n\"Worst-case\" scenarios of partial R2 with outcome", ...)
  abline(h = 0, col = "red", lty = 5)
  scenarios2 <- scenarios[-1]

  for (i in seq_along(scenarios2)) {
    y <- adjust_estimate(estimate, getbiasR2(se = se, df = df, r2y = scenarios2[i], r2d = s))
    lines(s,  y, lty = i + 1)
  }

  mr2y <- x$benchmarks$benchmark_all_vars$r2y_all
  p    <- length(scenarios)

  # max observed R2y
  y <- adjust_estimate(estimate, getbiasR2(se = se, df = df, r2y = mr2y, r2d = s))
  lines(s, y, lty = p + 1, col = "red")

  legend("topright",
         lty = c((1:p), p + 1),
         col = c(rep("black", p), "red"),
         legend = paste0(c(scenarios*100, round(mr2y*100)),  "%"),
         ncol = p + 1,
         title = "Hypothetical partial R2 of unobserved confounder(s) with outcome",
         cex  = cex.legend)
  rug(x = r2d, col = "red")

}
