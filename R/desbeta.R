#' Estimate skewness and kurtosis based on Beta distribution in one sample
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution. Also, this function estimate the shape parameters alpha and beta.
#'@param vmean sample mean of the beta distributed data
#'@param vsd sample standard deviation of the beta distributed data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal and beta curves.
#'@param ... other arguments
#'@import ggplot2
#'@export
#'@examples
#'data('beta_mdat')
#'desbeta(vmean=beta_mdat$m2[6], vsd=beta_mdat$sd2[6],
#'hi = beta_mdat$hi2[6], lo = beta_mdat$lo2[6], showFigure = TRUE)
#'
#'@seealso \code{\link{destrunc}}
#'
#'@references
#'\insertRef{johnson1995continuous}{detectnorm}
#'
#'\insertRef{smithson2006better}{detectnorm}
#'
#'\insertRef{olkin2014cons}{detectnorm}
#'
desbeta <- function(vmean,
                    vsd,
                    lo,
                    hi,
                    showFigure = FALSE,
                     ...) {
  vrange <- hi - lo
  bmean <- (vmean - lo)/vrange
  bv <- (vsd^2)/(vrange^2)
  bsd <- sqrt(bv)
  d <- (1 - bmean)/bmean
  # Method of moments
  balpha <- bmean * (bmean * (1 - bmean)/bv - 1)
  bbeta <- balpha * d
  bskew <- 2 * (bbeta - balpha) * sqrt(bbeta + balpha + 1)
  bskew <- bskew/((bbeta + balpha + 2) * sqrt(bbeta * balpha))
  a1 <- ((balpha - bbeta)^2) * (balpha + bbeta + 1)
  a2 <- bbeta * balpha * (bbeta + balpha + 2)
  b1 <- balpha * bbeta * (balpha + bbeta + 2) * (balpha + bbeta + 3)
  bkurt <- 6 * (a1 - a2)/b1


  final_result <- data.frame(bmean = bmean, bsd = bsd,
                             balpha = balpha, bbeta = bbeta,
                             bskew = bskew, bkurt = bkurt)
  if (showFigure == TRUE) {
    ynames <- paste("skew =", round(final_result$bskew, 3), ", kurt =",
                    round(final_result$bkurt,3),
                    " red-beta; blue-normal" ,sep = "")
    fig <- ggplot2::ggplot(final_result) +
      stat_function(fun = dnorm, args = list(vmean, vsd),
                    colour = "blue", na.rm = TRUE) +
      stat_function(fun = .dbeta4param,
                    args = list(alpha = balpha, beta = bbeta,
                                lo = lo, hi = hi),
                    colour = "red",
                    na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 1.2)) +
      scale_x_continuous(limits = c(lo , hi)) +
      labs(title = ynames, y = "density", x = "x") +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "grey50"),
            legend.position = "bottom",
            strip.background = element_rect(colour = "black",
                                            fill = "white"))
    print(fig)
  }
  return(final_result)
}
