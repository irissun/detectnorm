des2beta <- function(vmean, vsd, lo = 0, hi = 1, showFigure = FALSE, ...) {
    
    # Compute the parameters of a beta distribution with the same mean and SD of a
    # variable with a finite range.  Return the parameters, and the skewness and
    # kurtosis of this beta distribution.
    
    # vmean: Sample mean vsd: Sample standard deviation low: Minimum possible value
    # of the variable hi: Maximum possible value of the variable
    
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
    
    if (balpha <= 0 || bbeta <= 0) {
        balpha <- bbeta <- bmean <- bv <- bskew <- bkurtosis <- NA
    }
    
    if (showFigure == TRUE) {
        x <- seq(0, 1, length = 1000)
        ynames <- paste("skew=", round(bskew, 3), ",
                  kurt=", 
            round(bkurtosis, 3), sep = "")
        df <- data.frame(xdat = rep(x, 2))
        df$ydat <- c(dbeta(x, shape1 = balpha, shape2 = bbeta), dnorm(x, mean = bmean, 
            sd = sqrt(bv)))
        df$group <- c(rep("beta", 1000), rep("normal", 1000))
        fig <- ggplot2::ggplot(df, aes(x = xdat, y = ydat, group = group)) + geom_line(aes(linetype = group, 
            col = group)) + geom_vline(xintercept = bmean, color = "black", linetype = "dashed") + 
            labs(title = ynames, y = "density", x = "x") + theme(panel.background = element_rect(fill = "white", 
            colour = "grey50"), legend.position = "bottom", strip.background = element_rect(colour = "black", 
            fill = "white"))
        print(fig)
    }
    results <- list(alpha = balpha, beta = bbeta, mean = bmean, sd = sqrt(bv), 
        skewness = bskew, kurtosis = bkurtosis)
    results
}
