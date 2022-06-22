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
#'@import ggplot2
#'@export
#'@examples
#'data('metadat')
#'desbeta(vmean=metadat$m2[6], vsd=metadat$sd2[6],
#'hi = metadat$p.max[6], lo = 0, showFigure = T)
#'
#'@seealso \code{\link{destrunc}}
desbeta <- function(vmean,
                    vsd,
                    lo,
                    hi,
                    method = "MM",
                    rawdata = NULL,
                    showFigure = FALSE, ...) {
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
  if(vsd == 0){vsd <- .01}
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
    result <- data.frame(alpha = balpha, beta = bbeta, mean = bmean, sd = sqrt(bv),
        skewness = bskew, kurtosis = bkurtosis)
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
            fig <- ggplot(data.frame(x = c(lo, hi)), aes(x = x)) +
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
