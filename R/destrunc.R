#' Calculate skewness and kurtosis based on truncated normal distribution in one group
#'
#'This function can be used to calculate the skewness and kurtosis based on the truncated normal distribution. Also, it can be used to estimate the mean and variance of the parent distribution (the distribution before truncated).
#'@param vmean sample mean of the truncated data
#'@param vsd sample standard deviation of the truncated data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param rawdata when raw data is available, we could still use it to check it figuratively, if the data was closed to the normal distribution, or truncated normal distribution.
#'@param ... other arguments
#'@import ggplot2
#'@import truncnorm
#'@export
#'@examples
#'\dontrun{
#'data("metadat")
#'destrunc(vmean=dat$m2[6], vsd=dat$sd2[6],
#'hi = dat$p.max[6],showFigure = T)
#'}
#'@seealso \code{\link{desbeta}}
destrunc <- function(vmean,
                      vsd,
                      lo,
                      hi,
                      rawdata = NULL,
                      showFigure = FALSE,
                      ...){
  require(ggplot2)
  require(truncnorm)
  if(!is.null(rawdata)){
    vmean <- mean(rawdata, is.na = TRUE)
    print(paste("mean is ", vmean, sep=""))
    vsd <- sd(rawdata)
    print(paste("sd is ", vsd, sep=""))
    if(missing(lo)){
      lo <- min(rawdata)
    }
    if(missing(hi)){
      hi <- max(rawdata)
    }
    print(paste("min. is ", lo, sep=""))
    print(paste("max. is ", hi, sep=""))
  }else{
    vmean <- vmean
    vsd <- vsd
    lo <- lo
    hi <- hi
  }
  model <- function(x){
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
  xstart <- c(0, 1)
  #To gain the mean and SD of the parent distribution
  ans <- as.data.frame(nleqslv::nleqslv(xstart, model, method = "Newton",
                                        control = list(btol=.000001,
                                                       delta="newton",
                                                       allowSingular=FALSE)))
  # This part is for simplifying the calculation
  h1l = (lo - ans$x[[1]])/ans$x[[2]]; h1u = (hi - ans$x[[1]])/ans$x[[2]]
  h2l <- h1l^2 - 1; h2u <- h1u^2 - 1
  d2l <- dnorm(h1l); d2u <- dnorm(h1u)
  p2l <- pnorm(h1l);	p2u <- pnorm(h1u)
  z1 <- d2u/(p2u - p2l); z0 <- d2l/(p2u - p2l)
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
                tm = m1, tsd = sd1, skew = skew, kurt = kurt)

  if (showFigure == TRUE) {
    ynames <- paste("skew=", round(result$skew, 3),
                    ",kurt=", round(result$kurt, 3),
                    "; red-trunc; blue-normal",
                    sep = "")
    if(!is.null(data)){
      ynames <- paste(ynames, sep="")
      fig <- ggplot2::ggplot(data.frame(x = rawdata),
                             aes(x = rawdata)) +
        geom_histogram(aes(y=..density..), colour = "black", fill = "white",
                       bins = (hi - lo)-1, boundary = 0) +
        geom_density(alpha = .2) +
        stat_function(fun = dnorm, args = list(vmean, vsd),
                      colour = "blue")+
        stat_function(fun = dtruncnorm, args = list(a=lo, b=hi,
                                                    mean=result$pmean,
                                                    sd=result$psd),
                      colour = "red")+
        labs(title = ynames, y = "density", x = "x") +
        theme(panel.background = element_rect(fill = "white",
                                              colour = "grey50"),
              legend.position = "bottom",
              strip.background = element_rect(colour = "black",
                                              fill = "white"))
    } else{
      fig <- ggplot2::ggplot(data.frame(x = c(lo, hi)), aes(x = x)) +
        stat_function(fun = dnorm, args = list(vmean, vsd),
                      colour = "blue")+
        stat_function(fun = dtruncnorm, args = list(a=lo, b=hi,
                                                    mean=result$pmean,
                                                    sd=result$psd),
                      colour = "red")+
        labs(title = ynames, y = "density", x = "x") +
        theme(panel.background = element_rect(fill = "white",
                                              colour = "grey50"),
              legend.position = "bottom",
              strip.background = element_rect(colour = "black",
                                              fill = "white"))
    }

    result <- list(dat = result, fig = fig)
  }
  return(result)
}

