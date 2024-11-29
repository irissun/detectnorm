#' Estimate skewness and kurtosis based on truncated normal distribution in one sample
#'
#'This function can be used to calculate the skewness and kurtosis based on the truncated normal distribution.
#'Also, this function estimate the mean and standard deviation of the parent distribution.
#'
#'@param vmean sample mean of the beta distributed data
#'@param vsd sample standard deviation of the beta distributed data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal and beta curves.
#'@param xstart arguments used for \code{nleqslv} to solve the equations.
#'@param control arguments used for \code{nleqslv} to solve the equations.
#'@param ... other arguments
#'@import nleqslv
#'@export
#'@examples
#'data('trun_mdat')
#'destrunc(vmean=trun_mdat$m2[8], vsd=trun_mdat$sd2[8],
#'hi = trun_mdat$hi2[8], lo = trun_mdat$lo2[8])
#'
#'@seealso \code{\link{desbeta}}
#'
destrunc <- function(vmean,
                      vsd,
                      lo,
                      hi,
                      showFigure = FALSE,
                      xstart = c(vmean, vsd),
                      control = list(allowSingular = TRUE),
                      ...){
  if(missing(lo)){lo <- 0} else{lo <- lo}
  if(vsd == 0){vsd <- .01}
  #To gain the mean and SD of the parent distribution
  model.x <- function(x){
    f <- numeric(2)
    lower.std = (lo - x[1])/x[2]
    upper.std = (hi - x[1])/x[2]
    f[1] = x[1] + x[2]*(dnorm(lower.std) - dnorm(upper.std))/
      (pnorm(upper.std) - pnorm(lower.std)) - vmean
    f[2] = x[2]^2*(1+(lower.std*dnorm(lower.std)-upper.std*dnorm(upper.std))/
                     (pnorm(upper.std)-pnorm(lower.std))-
                     ((dnorm(lower.std)-dnorm(upper.std))/
                        (pnorm(upper.std)-pnorm(lower.std)))^2) -vsd^2
    f
  }
  ans <- as.data.frame(nleqslv(xstart, model.x,
                               control = control))
  #if(ans$x[1]> vmean + 4*vsd | ans$x[1]<vmean - 4*vsd){warning("Mean of Parent distribution is out of 4sd range")}
  if(ans$x[2]<0){
    warning("SD of parent distribution is less than zero")
    ans$x[[2]] <- NA
  }
  if(ans$message[1] == "No better point found (algorithm has stalled)"){
    warning(ans$message[1])
    ans$x[[1]] <- NA
  }
  if(ans$message[2] == "No better point found (algorithm has stalled)"){
    warning(ans$message[2])
    ans$x[[2]] <- NA
  }
  # This part is for simplifying the calculation
  h1l = (lo - ans$x[[1]])/ans$x[[2]]; h1u = (hi - ans$x[[1]])/ans$x[[2]]
  h2l <- h1l^2 - 1; h2u <- h1u^2 - 1
  d2l <- dnorm(h1l); d2u <- dnorm(h1u)
  p2l <- pnorm(h1l);	p2u <- pnorm(h1u)
  z1 <- d2u/(p2u - p2l); z0 <- d2l/(p2u - p2l)
  pmean <- ans$x[[1]]
  psd <- ans$x[[2]]
  # The expected mean, variance, skewness and kurtosis for truncated distribution
  m1 <- ans$x[[1]] - ans$x[[2]]*(z1-z0)
  sd1 <- ans$x[[2]]*sqrt(1 - (h1u*z1 - h1l*z0) - (z1 - z0)^2)
  skew <- ((h2l*d2l - h2u*d2u)/(p2u - p2l) -
             3*(h1l*d2l - h1u*d2u)*(d2l - d2u)/(p2u - p2l)^2 +
             2*(d2l - d2u)^3/(p2u - p2l)^3)/
    (1 + (h1l*d2l - h1u*d2u)/(p2u - p2l) - (d2l - d2u)^2/(p2u - p2l)^2)^(3/2)
  kurt <- (-3*(z1 - z0)^4 - 6*(h1u*z1 - h1l*z0)*(z1 - z0)^2 - 2*(z1 - z0)^2 -
             4*(h1u^2*z1 - h1l^2*z0)*(z1 - z0) - 3*(h1u*z1 - h1l*z0) -
             (h1u^3*z1 - h1l^3*z0) + 3
  )/
    (1 + (h1l*d2l - h1u*d2u)/(p2u - p2l) - (d2l - d2u)^2/(p2u - p2l)^2)^2 -3
  result<- data.frame(pmean = ans$x[[1]], psd = ans$x[[2]],
                     #pmean and psd is the population mean and sd of the parent distribution
                      tm = m1, tsd = sd1, skewness = skew, kurtosis = kurt)

  if (showFigure == TRUE) {
    ynames <- paste("skew =", round(result$skewness, 3), ", kurt =",
                    round(result$kurtosis,3),
                    " red-truncated; blue-normal" ,sep = "")
    fig <- ggplot2::ggplot(data.frame(result)) +
      stat_function(fun = dnorm, args = list(vmean, vsd),
                    colour = "blue", na.rm = TRUE) +
      stat_function(fun = truncnorm::dtruncnorm, args = list(a=lo, b=hi,
                                                  mean=pmean,
                                                  sd=psd),
                    na.rm = TRUE, colour = "red")+
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
  return(result)
}
