desbeta_dep <- function(vmean = vmean, vsd = vsd, lo = lo,
                        hi = hi, cor_xy = cor_xy,
                        showFigure = showFigure, ...){
  vrange <- hi - lo
  bmean <- (vmean - lo)/vrange
  bv <- (vsd^2)/(vrange^2)
  bsd <- sqrt(bv)
  d <- (1 - bmean)/bmean
  if(length(vmean) != length(vsd) & length(vsd) != 2)
    stop("Provide mean and sd for two dependent groups")
  #check whether the dependent design specify the correlation
  if (is.null(cor_xy))
    stop("Specify the correlation for two dependent groups, cor_xy = ")
  mod_dir <- function(x){
    f <- numeric(4)
    m <- x[1] + x[2] + x[3] + x[4]
    #f[1] = (x[1] + x[2])/m - bmean[1]
    f[1] = (x[1] + x[3])/m - bmean[2] + (x[1] + x[2])/m - bmean[1]
    #f[1] = x[1] + x[2] + x[3] + x[4] - m
    f[2] = (bmean[1]*m)*(x[4] + x[3])/(m^2*(m+1)) - bsd[1]^2
    f[3] = (bmean[2]*m)*(x[4] + x[2])/(m^2*(m+1)) - bsd[2]^2
    f[4] = (x[1]*x[4] - x[2]*x[3])/sqrt((bmean[1]*m)*(bmean[2]*m)*
                                          (x[4] +x[3])*(x[4]+x[2])) - cor_xy
    f
  }
  xstart <- c(.1, .1, .1, .1)
  ans <- as.data.frame(nleqslv(xstart, mod_dir, method = "Newton", ...))
  beta_fun <- function(a, b){
    m <- a + b
    bmean <- a/(a+b)
    bsd <- sqrt(a*b/(m^2*(m+1)))
    bskew <- 2*(b-a)*sqrt(a+b+1)/((a+b+2)*sqrt(a*b))
    bkurt <- 6*((b - a)^2*(a+b+1) - a*b*(a+b+2))/(a*b*(a+b+2)*(a+b+3))
    return(data.frame(bmean, bsd, balpha = a, bbeta = b, bskew, bkurt))
  }
  g1 <-  beta_fun(a = ans$x[1] + ans$x[2], b = ans$x[3] + ans$x[4])
  g2 <- beta_fun(a = ans$x[1] + ans$x[3], b = ans$x[2] + ans$x[4])
  result <- dat <- data.frame(alpha11= ans$x[1], alpha10 = ans$x[2],
                       alpha01 = ans$x[3], alpha00 = ans$x[4],
                       g1mean = g1$bmean, g1sd = g1$bsd, g1alpha = g1$balpha,
                       g1beta = g1$bbeta, g1skew = g1$bskew, g1kurt = g1$bkurt,
                       g2mean = g2$bmean,g2sd = g2$bsd,g2alpha = g2$balpha,
                       g2beta = g2$bbeta, g2skew = g2$bskew, g2kurt = g2$bkurt)
  if (showFigure == TRUE) {
    ynames <- paste("Group 1: skew1=", round(dat$g1skew, 3), ", kurt1=",
                    round(dat$g1kurt,3),
                    "\nGroup 2 (dashed): skew2=", round(dat$g2skew, 3), ", kurt2=",
                    round(dat$g2kurt,3),
                    "\n Correlation = ", cor_xy,
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
