desbeta_ind <- function(vmean = vmean, vsd = vsd, lo = lo,
                        hi = hi, showFigure = showFigure, ...){
    vrange <- hi - lo
    bmean <- (vmean - lo)/vrange
    bv <- (vsd^2)/(vrange^2)
    bsd <- sqrt(bv)
    d <- (1 - bmean)/bmean
    if(length(vmean) != length(vsd) & length(vsd) != 2)
      stop("Provide means and sds for two independent groups")
    # Method of moments

    balpha <- bmean * (bmean * (1 - bmean)/bv - 1)
    bbeta <- balpha * d
    bskew <- 2 * (bbeta - balpha) * sqrt(bbeta + balpha + 1)
    bskew <- bskew/((bbeta + balpha + 2) * sqrt(bbeta * balpha))
    a1 <- ((balpha - bbeta)^2) * (balpha + bbeta + 1)
    a2 <- bbeta * balpha * (bbeta + balpha + 2)
    b1 <- balpha * bbeta * (balpha + bbeta + 2) * (balpha + bbeta + 3)
    bkurtosis <- 6 * (a1 - a2)/b1

    #if (balpha <= 0 || bbeta <= 0 || is.na(balpha) || is.na(bbeta)) {
    #  balpha <- bbeta <- bmean <- bv <- bskew <- bkurtosis <- NA
    #}
    result <- dat <- data.frame(g1bmean = bmean[[1]],
                                g1bsd = bsd[[1]],
                                g1alpha = balpha[[1]],
                                g1beta = bbeta[[1]],
                                g1skew = bskew[[1]],
                                g1kurt = bkurtosis[[1]],
                                #group2
                                g2bmean = bmean[[2]],
                                g2bsd = bsd[[2]],
                                g2alpha = balpha[[2]],
                                g2beta = bbeta[[2]],
                                g2skew = bskew[[2]],
                                g2kurt = bkurtosis[[2]]
                                )

    if (showFigure == TRUE) {
      ynames <- paste("Group 1: skew1=", round(dat$g1skew, 3), ", kurt1=",
                      round(dat$g1kurt,3),
                      "\nGroup 2 (dashed): skew2=", round(dat$g2skew, 3), ", kurt2=",
                      round(dat$g2kurt,3),
                      "\n red-beta; blue-normal" ,sep = "" )
      fig <- ggplot2::ggplot(data.frame(result)) +
        stat_function(fun = dnorm, args = list(vmean[[1]], vsd[[1]]),
                      colour = "blue", na.rm = TRUE) +
        stat_function(fun = dbeta4param,
                      args = list(alpha = result$g1alpha, beta = result$g1beta,
                                  lo = lo[[1]], hi = hi[[1]]),
                      colour = "red",
                      na.rm = TRUE) +
        stat_function(fun = dnorm, args = list(vmean[[2]], vsd[[2]]),
                      colour = "blue", linetype = 2, na.rm = TRUE)+
        stat_function(fun = dbeta4param,
                      args = list(alpha = result$g2alpha, beta = result$g2beta,
                                  lo = lo[[2]], hi = hi[[2]]),
                      colour = "red", linetype = 2, na.rm = TRUE) +
        scale_y_continuous(limits = c(0, 1.2)) +
        scale_x_continuous(limits = c(min(lo) , max(hi))) +
        labs(title = ynames, y = "density", x = "x") +
        theme(panel.background = element_rect(fill = "white",
                                              colour = "grey50"),
              legend.position = "bottom",
              strip.background = element_rect(colour = "black",
                                              fill = "white"))
        result <- list(dat, fig)
    }
    return(result)
}
