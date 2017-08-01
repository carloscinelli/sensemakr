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
##' @export
plot.sensemade <- function(x,
                           type = c("contour", "worst-case"),
                           ...){

  # can optionally do showvars arg here

  # one way to generic pass to contourplot() and worstcaseplot()

  type <- match.arg(type)
  switch(type,
         contour = contourplot(x, ...),
         "worst-case" = worstcaseplot(x, ...)
  )
}

# A method must have all the arguments of the generic, including … if the generic does.

##' @export
contourplot <- function(x,
                        showvars = "masked",
                        contour = c("estimate","t-value", "lower bound", "upper bound"),
                        nlevels = 15,
                        pch = 20,
                        cex = 1,
                        lim = NULL,
                        xlab = "Hypothetical partial R2 of unobserved confounder with the treatment",
                        ylab = "Hypothetical partial R2 of unobserved confounder with the outcome",
                        main = paste("Sensitivity of",  contour, "to unobserved confounder\nContours of adjusted estimates"),
                        top = NULL,
                        x.label = NULL,
                        y.label = NULL){

#   showvars = "masked"
#   contour = c("estimate","t-value", "lower bound", "upper bound")
#   nlevels = 15
#   pch = 20
#   cex = 1
#   lim = NULL
#   xlab = "Hypothetical partial R2 of unobserved confounder with the treatment"
#   ylab = "Hypothetical partial R2 of unobserved confounder with the outcome"
#   main = paste("Sensitivity of",  contour, "to unobserved confounder\nContours of adjusted estimates")
#   top = NULL
#   x.label = NULL
#   y.label = NULL

  contour <- match.arg(contour)

  # 'showvars' options
  # 1 'masked' (default), plot benchmark_group + benchmark_masked
  # 2 'all', plot benchmark_eachvar + benchmark_group
  # 3 list("foo1","foo2"), plot explicit 'foo1' and 'foo2' only


  ################################################
  # treeage" showvars to determine
  # subset rows of the benchmark data

  if(is.character(showvars)==TRUE){

    if(showvars=='masked'){
      # use 'benchmark_masked' instead of 'benchmark_eachvar'
      benchmarks  <- x$benchmarks$benchmark_masked
      benchmarks_group  <- x$benchmarks$benchmark_group

    } else if(showvars=='all'){
      # no subset
      benchmarks  <- x$benchmarks$benchmark_eachvar
      benchmarks_group  <- x$benchmarks$benchmark_group

    } else {
      stop('You have supplied an incompatible "showvars" option')
    }

  }else if(is.list(showvars)==TRUE){
    # showvars=list('foo1','foo2')

    # subset 'benchmark_eachvar' and 'benchmark_group'
    # based on list elements in showvars

    # showvars = list('village','villageMngao','age')

    ind_each_in_showvar = row.names(x$benchmarks$benchmark_eachvar) %in% showvars
    ind_group_in_showvar = row.names(x$benchmarks$benchmark_group) %in% showvars

    benchmarks = (x$benchmarks$benchmark_eachvar)[ind_each_in_showvar,]
    benchmarks_group = (x$benchmarks$benchmark_group)[ind_group_in_showvar,]

  } else {
    stop('You have supplied an incompatible "showvars" option')
  }

  ###########################


  #### subset the benchmark data ####

  # 'benchmarks' assigned earlier based on showvars subset
  # benchmarks  <- x$benchmarks$benchmark_eachvar

  top         <- min(c(nrow(benchmarks), top))
  benchmarks  <- benchmarks[1:top, ]
  r2y         <- benchmarks$r2y
  r2d         <- benchmarks$r2d

  # 'benchmarks_group' assigned earlier based on showvars subset
  # benchmarks_group  <- x$benchmarks$benchmark_group
  r2y_group = benchmarks_group$r2y
  r2d_group = benchmarks_group$r2d

  ####


  # NOTE: if-else masking of design matrix columns tied to their overall grouping
  # design choice, make user mask these low level points
  # via 'showvars' argument

  # eg, we will always plot the points tied to 'grouping' benchmarks
  # but we will not mask the points tied to the levels of the 'grouping' benchmarks

  # one way to 'mask' is line 178 of https://github.com/statsccpr/ovb/blob/master/R/ovb.R


  #### contour data (treatment effect data) ####
  estimate    <- x$treat.stats$estimate
  se          <- x$treat.stats$se
  df          <- x$treat.stats$df
  t           <- estimate/se

  #### computing contours ####

  ## sequence of R2y and R2d
  if (is.null(lim)) lim <- max(c(r2y, r2d, r2y_group, r2d_group), na.rm = TRUE) + 0.1
  s <- seq(0, lim, by = 0.001)

  ## level curves
  if (contour == "estimate") {
    # estimate level curve
    z      <- adjust_estimate(estimate, outer(s, s, get_bias, se = se, df = df))
    labels <- paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_est_r2, 3),")")
    lev    <- 0

  } else if (contour == "t-value") {
    # t-value level curve
    z      <- outer(s, s, get_t, t = t, df = df)
    labels <- paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_t_r2, 3),")")
    lev    <- 2

  } else if (contour == "lower bound" | contour == "upper bound" ) {
    # CI level curves
    new_estimate <- adjust_estimate(estimate, outer(s, s, get_bias, se = se, df = df))
    new_se       <- outer(s, s, get_se, se = se, df = df)

    if (contour == "lower bound") {
      # CI lower bound
      z                    <- new_estimate - 1.96*new_se
      benchmarks$adj_lw_r2 <- benchmarks$adj_est_r2 - 1.96*benchmarks$adj_se_r2
      labs                 <- benchmarks$adj_lw_r2

    } else {
      # CI upper bound
      z                    <- new_estimate + 1.96*new_se
      benchmarks$adj_up_r2 <- benchmarks$adj_est_r2 + 1.96*benchmarks$adj_se_r2
      labs                 <- benchmarks$adj_up_r2

    }
    labels <- paste0(benchmarks$covariate, "\n", "(",round(labs, 3),")")
    lev <- 0

  }
  ####

  ## plot contours with benchmarks
  contour(s, s, z, nlevels = nlevels,
          xlab = xlab,
          ylab = ylab,
          main = main)

  points(r2d, r2y, pch = 23, col = "black", bg = "red", cex = cex)
  contour(s, s, z = z, level = lev, add = TRUE, col = "red", lwd = 2, lty = 2)
  points(r2d_group, r2y_group, pch = 23, col = "black", bg = "cyan", cex = cex)




  #### add labels ####
  # todo: try to create a better function for positioning labels and substitute jitter
  if (is.null(x.label))
    r2dl <- jitter(r2d, factor = 20)

  if (is.null(y.label))
    r2yl <- jitter(r2y, factor = 20)

  text(r2dl, r2yl, labels = labels, cex = 0.7)

  if(nrow(benchmarks_group)>0){

    labels_group <- benchmarks_group$covariate

    text(x=benchmarks_group$r2d,
         y=benchmarks_group$r2y,
         labels=labels_group,
         cex=0.7,
         pos=4)

  }

  labels <- data.frame(labels = labels,x = r2dl, y = r2yl, stringsAsFactors = FALSE)
  ####

  #### data for reproducibility ####
  rownames(z) <- colnames(z) <- s
  out <- list(plot_type = paste(contour,"contours"),
              contours = z,
              benchmarks = benchmarks,
              benchmarks_group = benchmarks_group,
              labels = labels)

  ####

  invisible(out)
}

contourplot

##' @export
worstcaseplot <- function(x,
                          showvars = "masked",
                          lim = NULL,
                          scenarios = c(1, 0.3),
                          cex.legend = 0.6,
                          index = NULL,
                          xlab = "Hypothetical partial R2 of unobserved confounder(s) with treatment",
                          ylab = "Adjusted estimate",
                          main = "Sensitivity of estimate to unobserved confounder(s)\n\"Worst-case\" scenarios of partial R2 with outcome"){

  # treeage" showvars to determine
  # subset rows of the benchmark data

  if(is.character(showvars)==TRUE){

    if(showvars=='masked'){
      # use 'benchmark_masked' instead of 'benchmark_eachvar'
      benchmarks  <- x$benchmarks$benchmark_masked
      benchmarks_group  <- x$benchmarks$benchmark_group

    } else if(showvars=='all'){
      # no subset
      benchmarks  <- x$benchmarks$benchmark_eachvar
      benchmarks_group  <- x$benchmarks$benchmark_group

    } else {
      stop('You have supplied an incompatible "showvars" option')
    }

  }else if(is.list(showvars)==TRUE){
    # showvars=list('foo1','foo2')

    # subset 'benchmark_eachvar' and 'benchmark_group'
    # based on list elements in showvars

    # showvars = list('village','villageMngao','age')

    ind_each_in_showvar = row.names(x$benchmarks$benchmark_eachvar) %in% showvars
    ind_group_in_showvar = row.names(x$benchmarks$benchmark_group) %in% showvars

    benchmarks = (x$benchmarks$benchmark_eachvar)[ind_each_in_showvar,]
    benchmarks_group = (x$benchmarks$benchmark_group)[ind_group_in_showvar,]

  } else {
    stop('You have supplied an incompatible "showvars" option')
  }

  ###########################


  # can optionally do showvars here for worstcaseplot

  # 'benchmarks' assigned earlier based on showvars subset

  # benchmarks <- x$benchmarks$benchmark_eachvar
  r2d <- benchmarks$r2d

  # 'benchmarks_group' assigned earlier based on showvars subset
  # benchmarks_group  <- x$benchmarks$benchmark_group
  r2d_group = benchmarks_group$r2d


  # mike note: what is index
  if (!is.null(index)) r2d <- r2d[index]

  if (!is.null(index)) r2d_group <- r2d_group[index]


  estimate <- x$treat.stats$estimate
  se  <- x$treat.stats$se
  df <- x$treat.stats$df

  if (is.null(lim)) lim <- max(r2d, na.rm = TRUE) + 0.1
  s <- seq(0, lim, by = 0.001)

  mr2y <- x$benchmarks$benchmark_dropallvar$r2y_all


  y <- adjust_estimate(estimate, get_bias(se = se, df = df, r2y = scenarios[1], r2d = s))

  out <- data.frame(r2d = s,
                    r2y = scenarios[1],
                    adj_est = y)


  plot(s,  y,
       type = "l", bty = "L",
       xlab = xlab,
       ylab = ylab,
       main = main)
  abline(h = 0, col = "red", lty = 5)
  scenarios2 <- scenarios[-1]

  for (i in seq_along(scenarios2)) {
    y <- adjust_estimate(estimate, get_bias(se = se, df = df, r2y = scenarios2[i], r2d = s))
    out2 <- data.frame(r2d = s,
                      r2y = scenarios2[i],
                      adj_est = y)
    out <- rbind(out, out2)
    lines(s,  y, lty = i + 1)
  }

  p    <- length(scenarios)

  # max observed R2y
  y <- adjust_estimate(estimate, get_bias(se = se, df = df, r2y = mr2y, r2d = s))
  lines(s, y, lty = p + 1, col = "red")
  out2 <- data.frame(r2d = s,
                     r2y = mr2y,
                     adj_est = y)
  out <- rbind(out, out2)

  out <- list(plot_type = "worst case plot",
              adjusted_estimates = out)

  legend("topright",
         lty = c((1:p), p + 1),
         col = c(rep("black", p), "red"),
         legend = paste0(c(scenarios*100, round(mr2y*100)),  "%"),
         ncol = p + 1,
         title = "Hypothetical partial R2 of unobserved confounder(s) with outcome",
         cex  = cex.legend)

  rug(x = r2d, col = "red")

  # figure out rug on grouped terms
  # mike has not used 'r2y_group'
  # confirm with group, if r2d_group used correctly in this context

  rug(x = r2d_group, col = "cyan",lwd=2)


  invisible(out)
}
