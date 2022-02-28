#' Calculate skewness and kurtosis based on Beta distribution in one group
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution. Also, this function estimate the shape parameters alpha and beta.
#'@param vmean sample mean of the truncated data
#'@param vsd sample standard deviation of the truncated data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@import ggplot2
#'@import truncnorm
#'@export
#'@examples
#'\dontrun{
#'data('metadat')
#'desbeta(vmean=metadat$m2[6], vsd=metadat$sd2[6],
#'hi = metadat$p.max[6], lo = 0, showFigure = T)
#'}
#'
#'@seealso \code{\link{destrunc}}
desbeta <- function(vmean,
                    vsd,
                    lo,
                    hi,
                    rawdata = NULL,
                    showFigure = FALSE, ...) {
    require(ggplot2)
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
