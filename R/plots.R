# graphics ----------------------------------------------------------------
##' @title The plot method for a sensemakr object
##' @description Several sensitivity plots.
##'
##' @param x a `sensemakr` object, result of \code{\link{sensemakr}}
##' @param type a character string representing the type of plot: "contour" or "worst-case".
##' @param ... extra arguments that might be passed to underlying functions
##' @seealso See \code{\link{par}} for graphical parameters
##' and \code{\link{plot}} for the generic plot method
##'
##'
##' @examples
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # runs benchmarking
##' sense = sensemakr(model, treatment = "directlyharmed")
##'
##' # draws plots
##' plot1_data = plot(sense)
##' plot2_data = plot(sense, contour = "t-value")
##' plot3_data = plot(sense, contour = "lower-limit")
##' plot4_data = plot(sense, contour = "upper-limit")
##' plot5_data = plot(sense, type = "worst-case")
##'
##'
##' @export
plot.sensemakr = function(x,
                          # showvars='masked',  # moved to lower level functions
                          type = c("contour", "worst-case"),
                          ...){
  ######################################################
  # 'showvars' options
  # 1 'masked' (default), plot benchmark_group + benchmark_masked
  # 2 'all', plot benchmark_eachvar + benchmark_group
  # 3 list("foo1","foo2"), plot explicit 'foo1' and 'foo2' only

  # later plot methods make use of
  # benchmarks  = x$benchmarks$benchmark_eachvar
  # benchmarks_group  = x$benchmarks$benchmark_group

  # so create
  # benchmarks = x$benchmarks$benchmarks_2plot1
  # benchmarks_group = x$benchmarks$benchmarks_2plot2
  ######################################################

  # "treeage" showvars to determine
  # subset rows of the benchmark data
#
#   if(is.character(showvars)==TRUE){
#
#     # x$benchmarks$benchmark_eachvar
#     # x$benchmarks$benchmark_group
#
#     if(showvars=='masked'){
#       # use 'benchmark_masked' instead of 'benchmark_eachvar'
#       x$benchmarks$benchmarks_2plot1 = x$benchmarks$benchmark_masked
#       x$benchmarks$benchmarks_2plot2 = x$benchmarks$benchmark_group
#
#     } else if(showvars=='all'){
#       # no subset
#       x$benchmarks$benchmarks_2plot1  = x$benchmarks$benchmark_eachvar
#       x$benchmarks$benchmarks_2plot2  = x$benchmarks$benchmark_group
#
#     } else {
#       stop('You have supplied an incompatible "showvars" option')
#     }
#
#   }else if(is.list(showvars)==TRUE){
#     # showvars=list('foo1','foo2')
#
#     # subset 'benchmark_eachvar' and 'benchmark_group'
#     # based on list elements in showvars
#
#     # showvars = list('village','villageMngao','age')
#
#     ind_each_in_showvar = row.names(x$benchmarks$benchmark_eachvar) %in% showvars
#     ind_group_in_showvar = row.names(x$benchmarks$benchmark_group) %in% showvars
#
#     x$benchmarks$benchmarks_2plot1 = (x$benchmarks$benchmark_eachvar)[ind_each_in_showvar,]
#     x$benchmarks$benchmarks_2plot2 = (x$benchmarks$benchmark_group)[ind_group_in_showvar,]
#
#   } else {
#     stop('You have supplied an incompatible "showvars" option')
#   }


  type = match.arg(type)
  switch(type,
         contour = contourplot(x, ...),
         "worst-case" = worstcaseplot(x, ...)
  )
}

# A method must have all the arguments of the generic, including … if the generic does.

##' @name plot.sensemakr
##' @param showvars chooses which subsets of benchmarks to display.
##' Valid options are: 'masked', 'all', or list('foo1','foo2')
##' @param contour a character string choosing what the contour lines represent: "estimate","t-value", "lower-limit", or "upper-limit"
##' @param nlevels an integer representing how many contour levels to display
##' @param lim a single numeric specifying the limits of a square plot window (one numeric applied to both x and y)
##' @param top an integer controlling the number of 'top' ranked benchmarks to display
##' @param pch see \code{\link{par}}
##' @param cex see \code{\link{par}}
##' @param xlab see \code{\link{par}}
##' @param ylab see \code{\link{par}}
##' @param main see \code{\link{par}}
##' @export
# now with showvars internally, can export
contourplot = function(x,
                       showvars='masked',
                       contour = c("estimate","t-value", "lower-limit", "upper-limit"),
                       nlevels = 15,
                       lim = NULL,
                       top = NULL,
                       pch = 20,
                       cex = 1,
                       xlab = "Hypothetical partial R2 of unobserved confounder with the treatment",
                       ylab = "Hypothetical partial R2 of unobserved confounder with the outcome",
                       # x.label = NULL,  # deprecated jitter
                       # y.label = NULL,  # deprecated jitter
                       main = paste("Sensitivity of",  contour, "to unobserved confounder\nContours of adjusted estimates")
                       ){


  if(is.character(showvars)==TRUE){

    # x$benchmarks$benchmark_eachvar
    # x$benchmarks$benchmark_group

    if(showvars=='masked'){
      # use 'benchmark_masked' instead of 'benchmark_eachvar'
      x$benchmarks$benchmarks_2plot1 = x$benchmarks$benchmark_masked
      x$benchmarks$benchmarks_2plot2 = x$benchmarks$benchmark_group

    } else if(showvars=='all'){
      # no subset
      x$benchmarks$benchmarks_2plot1  = x$benchmarks$benchmark_eachvar
      x$benchmarks$benchmarks_2plot2  = x$benchmarks$benchmark_group

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

    x$benchmarks$benchmarks_2plot1 = (x$benchmarks$benchmark_eachvar)[ind_each_in_showvar,]
    x$benchmarks$benchmarks_2plot2 = (x$benchmarks$benchmark_group)[ind_group_in_showvar,]

  } else {
    stop('You have supplied an incompatible "showvars" option')
  }

  #   contour = c("estimate","t-value", "lower-limit", "upper-limit")
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

  contour = match.arg(contour)



  ######################################################
  # subset the benchmark data
  ######################################################

  # 'working benchmarks' assigned earlier in generic plot.sensemakr()
  # based on showvars subset
  # x$benchmarks$benchmarks_2plot1
  # x$benchmarks$benchmarks_2plot2

  # benchmarks  = x$benchmarks$benchmark_eachvar
  benchmarks = x$benchmarks$benchmarks_2plot1

  # potential 0-length error if 'subsetted' out in top level plot()
  # x$benchmarks$benchmarks_2plot1
  # nrow(benchmarks)
  # later on add if(nrow()>0)

  # benchmarks_group  = x$benchmarks$benchmark_group
  benchmarks_group = x$benchmarks$benchmarks_2plot2



  # if nrow(benchmarks)==0, then after this,
  # any(is.na(benchmarks))==TRUE


  top         = min(c(nrow(benchmarks), top))
  benchmarks  = benchmarks[1:top, ]
  r2y         = benchmarks$r2y
  r2d         = benchmarks$r2d

  r2y_group = benchmarks_group$r2y
  r2d_group = benchmarks_group$r2d



  #### contour data (treatment effect data) ####
  estimate    = x$treat.stats$estimate
  se          = x$treat.stats$se
  df          = x$treat.stats$df
  t           = estimate/se

  #### computing contours ####

  ## sequence of R2y and R2d
  if (is.null(lim)) lim = max(c(r2y, r2d, r2y_group, r2d_group), na.rm = TRUE) + 0.1
  s = seq(0, lim, by = 0.001)

  ######################################################
  # level curves
  ######################################################

  if (contour == "estimate") {
    # estimate level curve
    z      = adjust_estimate(estimate, outer(s, s, get_bias, se = se, df = df))

    # labels = paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_est_r2, 3),")")
    # deprecating benchmarks$covariate
    # rely on row.names(benchmarks)

    labels = paste0(row.names(benchmarks), "\n", "(",round(benchmarks$adj_est_r2, 3),")")

    lev    = 0

  } else if (contour == "t-value") {
    # t-value level curve
    z      = outer(s, s, get_t, t = t, df = df)

    # labels = paste0(benchmarks$covariate, "\n", "(",round(benchmarks$adj_t_r2, 3),")")
    # deprecating benchmarks$covariate
    # rely on row.names(benchmarks)

    labels = paste0(row.names(benchmarks), "\n", "(",round(benchmarks$adj_t_r2, 3),")")


    lev    = 2

  } else if (contour == "lower-limit" | contour == "upper-limit" ) {
    # CI level curves
    new_estimate = adjust_estimate(estimate, outer(s, s, get_bias, se = se, df = df))
    new_se       = outer(s, s, get_se, se = se, df = df)

    if (contour == "lower-limit") {
      # CI lower-limit
      z                    = new_estimate - 1.96*new_se
      benchmarks$adj_lw_r2 = benchmarks$adj_est_r2 - 1.96*benchmarks$adj_se_r2
      labs                 = benchmarks$adj_lw_r2

    } else {
      # CI upper-limit
      z                    = new_estimate + 1.96*new_se
      benchmarks$adj_up_r2 = benchmarks$adj_est_r2 + 1.96*benchmarks$adj_se_r2
      labs                 = benchmarks$adj_up_r2

    }

    # labels = paste0(benchmarks$covariate, "\n", "(",round(labs, 3),")")
    # deprecating benchmarks$covariate
    # rely on row.names(benchmarks)

    labels = paste0(row.names(benchmarks), "\n", "(",round(labs, 3),")")

    lev = 0

  }

  ######################################################
  # plot contours with benchmarks
  ######################################################

  contour(s, s, z, nlevels = nlevels,
          xlab = xlab,
          ylab = ylab,
          main = main)

  points(r2d, r2y, pch = 23, col = "black", bg = "red", cex = cex)
  contour(s, s, z = z, level = lev, add = TRUE, col = "red", lwd = 2, lty = 2)
  points(r2d_group, r2y_group, pch = 23, col = "black", bg = "cyan", cex = cex)



  ######################################################
  # add labels
  # todo: try to create a better function for positioning labels and substitute jitter
  ######################################################

  # deprecate 'jitter' since it breaks group benchmark plots

  #   if (is.null(x.label))
  #     r2dl = jitter(r2d, factor = 20)
  #
  #   if (is.null(y.label))
  #     r2yl = jitter(r2y, factor = 20)
  #
  #   text(r2dl, r2yl, labels = labels, cex = 0.7)
  # from r2dl to r2d
  # from r2yl to r2y

  text(r2d, r2y, labels = labels, cex = 0.7)

  if(nrow(benchmarks_group)>0){

    labels_group = benchmarks_group$covariate

    text(x=benchmarks_group$r2d,
         y=benchmarks_group$r2y,
         labels=labels_group,
         cex=0.7,
         pos=4)

  }

  labels = data.frame(labels = labels,x = r2d, y = r2y, stringsAsFactors = FALSE)

  ######################################################
  # data for reproducibility
  ######################################################

  rownames(z) = colnames(z) = s
  out = list(plot_type = paste(contour,"contours"),
             contours = z,
             benchmarks_2plot1 = benchmarks,
             benchmarks_2plot2 = benchmarks_group,
             labels = labels)

  invisible(out)
}


##' @name plot.sensemakr
##' @param scenarios a numeric vector where each element represents a worst-case scenario for R2
##' @param cex.legend a seperate `cex` argument used for drawing the legend. See \code{\link{par}}
##' @export
# now with showvars internally, can export
worstcaseplot = function(x,
                         showvars='masked',
                         lim = NULL,
                         scenarios = c(1, 0.3),
                         cex.legend = 0.6,
                         # index = NULL, # carlos round back is this necessary, whats it used for
                         xlab = "Hypothetical partial R2 of unobserved confounder(s) with treatment",
                         ylab = "Adjusted estimate",
                         main = paste("Sensitivity of estimate to unobservedconfounder(s)\n",
                                      "\"Worst-case\" scenarios of partial R2 with outcome")
                         ){


  if(is.character(showvars)==TRUE){

    # x$benchmarks$benchmark_eachvar
    # x$benchmarks$benchmark_group

    if(showvars=='masked'){
      # use 'benchmark_masked' instead of 'benchmark_eachvar'
      x$benchmarks$benchmarks_2plot1 = x$benchmarks$benchmark_masked
      x$benchmarks$benchmarks_2plot2 = x$benchmarks$benchmark_group

    } else if(showvars=='all'){
      # no subset
      x$benchmarks$benchmarks_2plot1  = x$benchmarks$benchmark_eachvar
      x$benchmarks$benchmarks_2plot2  = x$benchmarks$benchmark_group

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

    x$benchmarks$benchmarks_2plot1 = (x$benchmarks$benchmark_eachvar)[ind_each_in_showvar,]
    x$benchmarks$benchmarks_2plot2 = (x$benchmarks$benchmark_group)[ind_group_in_showvar,]

  } else {
    stop('You have supplied an incompatible "showvars" option')
  }

  # 'working benchmarks' assigned earlier in generic plot.sensemakr()
  # based on showvars subset
  # x$benchmarks$benchmarks_2plot1
  # x$benchmarks$benchmarks_2plot2

  # benchmarks = x$benchmarks$benchmark_eachvar
  benchmarks = x$benchmarks$benchmarks_2plot1

  r2d = benchmarks$r2d

  # 'benchmarks_group' assigned earlier based on showvars subset
  # benchmarks_group  = x$benchmarks$benchmark_group
  benchmarks_group = x$benchmarks$benchmarks_2plot2

  r2d_group = benchmarks_group$r2d


  # # mike note: what is index
  # if (!is.null(index)) r2d = r2d[index]
  #
  # if (!is.null(index)) r2d_group = r2d_group[index]


  estimate = x$treat.stats$estimate
  se  = x$treat.stats$se
  df = x$treat.stats$df

  if (is.null(lim)) lim = max(r2d, na.rm = TRUE) + 0.1
  s = seq(0, lim, by = 0.001)

  mr2y = x$benchmarks$benchmark_dropallvar$r2y_all


  y = adjust_estimate(estimate, get_bias(se = se, df = df, r2y = scenarios[1], r2d = s))

  out = data.frame(r2d = s,
                   r2y = scenarios[1],
                   adj_est = y)


  plot(s,  y,
       type = "l", bty = "L",
       xlab = xlab,
       ylab = ylab,
       main = main)
  abline(h = 0, col = "red", lty = 5)
  scenarios2 = scenarios[-1]

  for (i in seq_along(scenarios2)) {
    y = adjust_estimate(estimate, get_bias(se = se, df = df, r2y = scenarios2[i], r2d = s))
    out2 = data.frame(r2d = s,
                      r2y = scenarios2[i],
                      adj_est = y)
    out = rbind(out, out2)
    lines(s,  y, lty = i + 1)
  }

  p    = length(scenarios)

  # max observed R2y
  y = adjust_estimate(estimate, get_bias(se = se, df = df, r2y = mr2y, r2d = s))
  lines(s, y, lty = p + 1, col = "red")
  out2 = data.frame(r2d = s,
                    r2y = mr2y,
                    adj_est = y)
  out = rbind(out, out2)

  out = list(plot_type = "worst case plot",
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
