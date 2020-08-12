#' Calculate skewness and kurtosis based on Beta distribution in one group
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution. Also, this function estimate the shape parameters alpha and beta.
#'@param vmean sample mean of the truncated data
#'@param vsd sample standard deviation of the truncated data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@examples
#'data("metadat")
#'desbeta(vmean = metadat$m2[6], vsd = metadat$sd2[6],
#'        hi = metadat$p.max[6], showFigure = T)
#'@seealso \code{\link{destrunc}}
#'@export
desbeta <- function(vmean,
                     vsd,
                     lo = 0,
                     hi = 1,
                     showFigure = FALSE,
                     ...) {
  require(ggplot2)
  vrange <- hi - lo
  bmean <- (vmean - lo)/vrange
  bv <- (vsd^2)/(vrange^2)
  d <- (1 - bmean)/bmean
  # Method of moments
  balpha <- bmean * (bmean * (1 - bmean)/bv - 1)
  bbeta <- balpha * d

  bskew <- 2 * (bbeta - balpha) * sqrt(bbeta + balpha + 1)
  bskew <- bskew/((bbeta + balpha + 2) * sqrt(bbeta * balpha))
  a1 <- ((balpha - bbeta)^2) * (balpha + bbeta + 1)
  a2 <- bbeta * balpha * (bbeta + balpha + 2)
  b1 <- balpha * bbeta * (balpha + bbeta + 2) * (balpha + bbeta + 3)
  bkurtosis <- 6 * (a1 - a2)/b1

  if (balpha <= 0 || bbeta <= 0 || is.na(balpha) || is.na(bbeta)) {
    balpha <- bbeta <- bmean <- bv <- bskew <- bkurtosis <- NA
  }
  result <- data.frame(alpha = balpha,
                  beta = bbeta,
                  mean = bmean,
                  sd = sqrt(bv),
                  skewness = bskew,
                  kurtosis = bkurtosis)
  if (showFigure == TRUE) {
    ynames <- paste("skew=", round(bskew, 3), ", kurt=",
                    round(bkurtosis, 3), sep = "")
    fig <- ggplot2::ggplot(data.frame(x = c(lo, hi)), aes(x = x)) +
      stat_function(fun = dnorm, args = list(vmean, vsd),
                    colour = "blue")+
      stat_function(fun = dbeta4param,
                    args = list(alpha=balpha,beta=bbeta,
                                hi=hi, lo=lo),
                    colour = "red")+
      labs(title = ynames, y = "density", x = "x") +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "grey50"),
            legend.position = "bottom",
            strip.background = element_rect(colour = "black",
                                            fill = "white"))
    result <- list(dat = result, fig = fig)
  }
  return(result)
}
