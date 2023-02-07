#' Calculate skewness and kurtosis based on Beta distribution in one group
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution. Also, this function estimate the shape parameters alpha and beta.
#'@param vmean sample mean of the truncated data
#'@param vsd sample standard deviation of the truncated data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param method when method = 'MM', the method used is the method of moments, when method = "ML', the method used to estimate the distribution is maximum likelihood
#'@param rawdata when raw data is available, we could still use it to check it figuratively, if the data was closed to the normal distribution, or truncated normal distribution.
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@return If `showFigure = TRUE`, the output will be a list with two objects: one is the data frame of shape parameters (alpha and beta), mean and standard deviation of standard beta distribution (mean and sd), and skewness and kurtosis; the other is the theoretical figures of beta and normal distributions. If `showFigure = FALSE`, the output will be only the data frame.
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
desbeta <- function(vmean, vsd, lo, hi, method = "MM", rawdata = NULL,
                    showFigure = FALSE, design = "independent",
                    cor_xy, ...) {
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
    } else{
        vmean <- vmean
        vsd <- vsd
        lo <- lo
        hi <- hi
    }
  #if(vsd == 0){vsd <- .01}
    vrange <- hi - lo
    bmean <- (vmean - lo)/vrange
    bv <- (vsd^2)/(vrange^2)
    d <- (1 - bmean)/bmean
    if(design == "independent"){
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
    result <- data.frame(alpha = balpha, beta = bbeta, mean = bmean, sd = sqrt(bv),
        skewness = bskew, kurtosis = bkurtosis)
    }
    if(design == "dependent" & length(vmean) == 2 & length(vsd) == 2
       & length(hi) == 2 & length(lo)==2 & !is.null(cor_xy)){
      vrange <- hi - lo
      bmean <- (vmean - lo)/vrange
      bsd <- vsd/vrange
      mod_dir <- function(x){
        f <- numeric(4)
        m <- x[1] + x[2] + x[3] + x[4]
        f[1] = (x[1] + x[2])/m - bmean[1]
        f[2] = (x[1] + x[3])/m - bmean[2]
        f[3] = (x[1] + x[2])*(x[4] + x[3])/(m^2*(m+1)) - bsd[1]^2
        #f[4] = (x[1] + x[3])*(x[4] + x[2])/(m^2*(m+1)) - sd2^2
        f[4] = (x[1]*x[4] - x[2]*x[3])/sqrt((x[1]+x[2])*(x[1] + x[3])*
                                              (x[4] +x[3])*(x[4]+x[2])) - cor_xy
        f
      }
      xstart <- c(.1, .1, .1, .1)
      ans <- as.data.frame(nleqslv(xstart, mod_dir, method = "Newton"))
      beta_fun <- function(a, b){
        m <- a + b
        bmean <- a/(a+b)
        bsd <- sqrt(a*b/(m^2*(m+1)))
        bskew <- 2*(b-a)*sqrt(a+b+1)/((a+b+2)*sqrt(a*b))
        bkurt <- 6*((b - a)^2*(a+b+1) - a*b*(a+b+2))/(a*b*(a+b+2)*(a+b+3))
        return(data.frame(bmean, bsd, bskew, bkurt))
      }
      g1 <-  beta_fun(a = ans$x[1] + ans$x[2], b = ans$x[3] + ans$x[4])
      g2 <- beta_fun(a = ans$x[1] + ans$x[3], b = ans$x[2] + ans$x[4])
      result <- data.frame(alpha11= ans$x[1], alpha10 = ans$x[2],
                           alpha01 = ans$x[3], alpha00 = ans$x[4],
                           g1mean = g1$bmean, g1sd = g1$bsd, g1skew = g1$bskew,
                           g1kurt = g1$bskew, g2mean = g2$bmean,g2sd = g2$bsd,
                           g2skew = g2$bskew, g2kurt = g2$bkurt)
    }
    if (showFigure == TRUE) {
        ynames <- paste("skew=", round(bskew, 3), ", kurt=", round(bkurtosis,
            3), "; red-beta; blue-normal",sep = "" )
        if(!is.null(rawdata)){
            ynames <- paste(ynames, "; black-density", sep="")
            fig <- ggplot2::ggplot(data.frame(x = rawdata),
                                   aes(x = rawdata)) +
                geom_histogram(aes(y=..density..), colour = "black", fill = "white",
                               bins = (hi - lo)+1) +
                geom_density(alpha = .2) +
                stat_function(fun = dnorm, args = list(vmean, vsd), colour = "blue") +
                stat_function(fun = dbeta4param, args =
                                  list(alpha = balpha, beta = bbeta, hi = hi, lo = lo),
                              colour = "red") + labs(title = ynames,
                                                     y = "density", x = "x") +
                theme(panel.background = element_rect(fill = "white",
                                                      colour = "grey50"),
                      legend.position = "bottom",
                      strip.background = element_rect(colour = "black",
                                                      fill = "white"))
        } else{
            fig <- ggplot2::ggplot(data.frame(x = c(lo, hi)), aes(x = x)) +
                stat_function(fun = dnorm, args = list(vmean, vsd), colour = "blue") +
                stat_function(fun = dbeta4param, args =
                                  list(alpha = balpha, beta = bbeta, hi = hi, lo = lo),
                              colour = "red") + labs(title = ynames,
                                                     y = "density", x = "x") +
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
