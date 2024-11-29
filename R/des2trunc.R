#' Estimate skewness and kurtosis based on truncated normal distribution in two groups in one study
#'
#'This function can be used to calculate the skewness and kurtosis based on the truncated normal distribution.
#'Also, this function estimate the mean and standard deviation of the parent distribution.
#'
#'@param vmean sample mean of the beta distributed data
#'@param vsd sample standard deviation of the beta distributed data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal and beta curves.
#'@param ... other arguments
#'@import nleqslv
#'@export
#'@examples
#'data('trun_mdat')
#'ex <- trun_mdat[4, ]
#' #independent two groups
#'des2trunc(vmean=c(ex$m1, ex$m2), vsd=c(ex$sd1, ex$sd2),
#' hi = c(ex$hi1, ex$hi2), lo = c(ex$lo1, ex$lo2))
#'
#'@seealso \code{\link{desbeta}}
#'
des2trunc <- function(vmean = vmean, vsd = vsd, lo = lo,
                        hi = hi,
                        control = list(allowSingular = TRUE),
                        showFigure = FALSE,
                        ...){

  if(length(vmean) != length(vsd) & length(vsd) != 2)
    stop("Provide mean and sd for two dependent groups or for one group please use function `destrunc1`")
  g1 <- destrunc(vmean = vmean[[1]], vsd = vsd[[1]], lo = lo[[1]],
                  hi = hi[[1]],  showFigure = FALSE)
  g2 <- destrunc(vmean = vmean[[2]], vsd = vsd[[2]], lo = lo[[2]],
                  hi = hi[[2]], showFigure = FALSE)

  result <- dat <- data.frame(g1pmean = g1$pmean, g1psd = g1$psd, g1tm = g1$tm,
                              g1tsd = g1$tsd ,g1skew = g1$skewness,g1kurt = g1$kurtosis,
                              g2pmean = g2$pmean, g2psd = g2$psd, g2tm = g2$tm,
                              g2tsd = g2$tsd,g2skew = g2$skewness,g2kurt = g2$kurtosis
  )
  if (showFigure == TRUE) {
    ymax_norm1 <- max(dnorm(x = seq(from = lo[[1]], to=hi[[1]],
                                    length.out = 1000),
                            mean = vmean[[1]], sd = vsd[[1]]))
    ymax_norm2 <- max(dnorm(x = seq(from = lo[[2]], to=hi[[2]],
                                    length.out = 1000),
                            mean = vmean[[2]], sd = vsd[[2]]))
    ymax_trun1 <- max(dtruncnorm(x = seq(from = lo[[1]], to=hi[[1]],
                                           length.out = 1000),
                                   mean = result$g1mean, sd = result$g1sd))
    ymax_trun2 <- max(dtruncnorm(x = seq(from = lo[[2]], to=hi[[2]],
                                           length.out = 1000),
                                 mean = result$g2mean, sd = result$g2sd))
    ymax <- max(ymax_norm1, ymax_norm2, ymax_trun1, ymax_trun2)

    ynames <- paste("Group 1: skew1=", round(dat$g1skew, 3), ", kurt1=",
                    round(dat$g1kurt,3),
                    "\nGroup 2 (dashed): skew2=", round(dat$g2skew, 3), ", kurt2=",
                    round(dat$g2kurt,3),
                    "\n red-trunc; blue-normal" ,sep = "" )
    fig <- ggplot2::ggplot(data.frame(result)) +
      #plot for first group
      stat_function(fun = dnorm, args = list(vmean[[1]], vsd[[1]]),
                    colour = "blue", na.rm = TRUE) +
      stat_function(fun = dtruncnorm, args = list(a=lo[[1]], b=hi[[1]],
                                                  mean=result$g1pmean,
                                                  sd=result$g1psd),
                    na.rm = TRUE, colour = "red")+
      #plot for second group
      stat_function(fun = dnorm, args = list(vmean[[2]], vsd[[2]]),
                    colour = "blue", linetype = 2, na.rm = TRUE)+
      stat_function(fun = dtruncnorm, args = list(a=lo[[2]], b=hi[[2]],
                                                  mean=result$g2pmean,
                                                  sd=result$g2psd),
                    na.rm = TRUE, linetype = 2, colour = "red")+
      scale_y_continuous(limits = c(0, ymax)) +
      scale_x_continuous(limits = c(min(lo) , max(hi))) +
      labs(title = ynames, y = "density", x = "x") +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "grey50"),
            legend.position = "bottom",
            strip.background = element_rect(colour = "black",
                                            fill = "white"))
     print(fig)
     final <- list(result, fig)
  }
  if(showFigure == TRUE){
    final <- list(result, fig)
  } else {
    final <- result
  }
  return(final)
}
