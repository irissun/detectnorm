#' Estimate skewness and kurtosis based on truncated normal distribution in a two group sample
#'
#'This function can be used to calculate the skewness and kurtosis based on the truncated normal distribution.
#'Also, this function estimate the mean and standard deviation of the parent distribution.
#'
#'@param vmean a vector of two sample means of the beta distributed data
#'@param vsd a vector of two sample standard deviation of the beta distributed data
#'@param lo a vector of two minimum possible value
#'@param hi a vector of two maximum possible value
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal and beta curves.
#'@param ... other arguments
#'@import nleqslv
#'@export
#'@examples
#'data('beta_mdat')
#'ex <- beta_mdat[2, ]
#' #independent two groups
#'des2beta(vmean=c(ex$m1, ex$m2), vsd=c(ex$sd1, ex$sd2),
#' hi = c(ex$hi1, ex$hi2), lo = c(ex$lo1, ex$lo2), design = "ind")
#'
#'# dependent two groups with correlation equal to .3
#' ex <- beta_mdat[12, ]
#' des2beta(vmean=c(ex$m1, ex$m2), vsd=c(ex$sd1, ex$sd2),
#' hi = c(ex$hi1, ex$hi2), lo = c(ex$lo1, ex$lo2), design = "dep", cor_xy = .3)
#'@seealso \code{\link{desbeta}}
#'
#'@references
#'\insertRef{johnson1995continuous}{detectnorm}
#'
#'\insertRef{smithson2006better}{detectnorm}
#'
#'\insertRef{olkin2014cons}{detectnorm}
#'
des2beta <- function(vmean = vmean,
                      vsd = vsd,
                      lo = lo,
                      hi = hi,
                      design = "ind",
                      cor_xy = NULL,
                      showFigure = FALSE, ...){

  if(length(vmean) != length(vsd) & length(vsd) != 2)
    stop("Provide means and standard deviations for the two groups")

  vrange <- hi - lo
  bmean <- (vmean - lo)/vrange
  bv <- (vsd^2)/(vrange^2)
  bsd <- sqrt(bv)
  d <- (1 - bmean)/bmean

  if (design == "ind"){
    # Method of moments
    balpha <- bmean * (bmean * (1 - bmean)/bv - 1)
    bbeta <- balpha * d
    bskew <- 2 * (bbeta - balpha) * sqrt(bbeta + balpha + 1)
    bskew <- bskew/((bbeta + balpha + 2) * sqrt(bbeta * balpha))
    a1 <- ((balpha - bbeta)^2) * (balpha + bbeta + 1)
    a2 <- bbeta * balpha * (bbeta + balpha + 2)
    b1 <- balpha * bbeta * (balpha + bbeta + 2) * (balpha + bbeta + 3)
    bkurtosis <- 6 * (a1 - a2)/b1

    result <- dat <- data.frame(
      #group 1
      g1bmean = bmean[[1]], g1bsd = bsd[[1]],
      g1alpha = balpha[[1]], g1beta = bbeta[[1]],
      g1skew = bskew[[1]], g1kurt = bkurtosis[[1]],
      #group2
      g2bmean = bmean[[2]], g2bsd = bsd[[2]],
      g2alpha = balpha[[2]], g2beta = bbeta[[2]],
      g2skew = bskew[[2]], g2kurt = bkurtosis[[2]])

  } else if(design == "dep") {
    #check whether the dependent design specify the correlation
    if (is.null(cor_xy) == TRUE) stop("Specify the correlation for two dependent groups, cor_xy = ")
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
    #group1
    a1 <- ans$x[1] + ans$x[2]
    b1 <- ans$x[3] + ans$x[4]
    m1 <- a1 + b1
    bmean1 <-a1/m1
    bsd1 <- sqrt(a1*b1/(m1^2*(m1+1)))
    bskew1 <- 2*(b1-a1)*sqrt(a1+b1+1)/((a1+b1+2)*sqrt(a1*b1))
    bkurt1 <- 6*((b1 - a1)^2*(a1+b1+1) - a1*b1*(a1+b1+2))/(a1*b1*(a1+b1+2)*(a1+b1+3))
    #group2
    a2 <- ans$x[1] + ans$x[3]
    b2 <- ans$x[2] + ans$x[4]
    m2 <- a2 + b2
    bmean2 <-a2/m2
    bsd2 <- sqrt(a2*b2/(m2^2*(m2+1)))
    bskew2 <- 2*(b2-a2)*sqrt(a2+b2+1)/((a2+b2+2)*sqrt(a2*b2))
    bkurt2 <- 6*((b2 - a2)^2*(a2+b2+1) - a2*b2*(a2+b2+2))/(a2*b2*(a2+b2+2)*(a2+b2+3))

    result <- dat <- data.frame(alpha11= ans$x[1], alpha10 = ans$x[2],
                         alpha01 = ans$x[3], alpha00 = ans$x[4],
                         #group 1
                         g1bmean = bmean1, g1bsd = bsd1,
                         g1alpha = a1, g1beta = b1,
                         g1skew = bskew1, g1kurt = bkurt1,
                         #group2
                         g2bmean = bmean2, g2bsd = bsd2,
                         g2alpha = a2, g2beta = b2,
                         g2skew = bskew2, g2kurt = bkurt2)

  }
  if (showFigure == TRUE) {
    ynames <- paste("Group 1: skew1=", round(dat$g1skew, 3), ", kurt1=",
                    round(dat$g1kurt,3),
                    "\nGroup 2 (dashed): skew2=", round(dat$g2skew, 3), ", kurt2=",
                    round(dat$g2kurt,3),
                    "\nRed-beta; blue-normal", "\ncor_xy = ",
                    ifelse(design == "dep", cor_xy, NA),
                    sep = "" )
    ymax_norm1 <- max(dnorm(x = seq(from = lo[[1]], to=hi[[1]],
                                    length.out = 1000),
                            mean = vmean[[1]], sd = vsd[[1]]))
    ymax_norm2 <- max(dnorm(x = seq(from = lo[[2]], to=hi[[2]],
                                    length.out = 1000),
                            mean = vmean[[2]], sd = vsd[[2]]))
    ymax_beta1 <- max(.dbeta4param(x = seq(from = lo[[1]], to=hi[[1]],
                                           length.out = 1000),
                                   alpha = result$g1alpha, beta = result$g1beta,
                                   lo = lo[[1]], hi = hi[[1]]))
    ymax_beta2 <- max(.dbeta4param(x = seq(from = lo[[2]], to=hi[[2]],
                                           length.out = 1000),
                                   alpha = result$g2alpha, beta = result$g2beta,
                                   lo = lo[[2]], hi = hi[[2]]))
    if(ymax_beta1 >=1 & ymax_beta2 >=1){ymax <- 1.2
    } else {
      ymax <- max(ymax_norm1, ymax_norm2, ymax_beta1, ymax_beta2)
    }
    fig <- ggplot2::ggplot(data.frame(result)) +
      stat_function(fun = dnorm, args = list(vmean[[1]], vsd[[1]]),
                    colour = "blue", na.rm = TRUE) +
      stat_function(fun = .dbeta4param,
                    args = list(alpha = result$g1alpha, beta = result$g1beta,
                                lo = lo[[1]], hi = hi[[1]]),
                    colour = "red",
                    na.rm = TRUE) +
      stat_function(fun = dnorm, args = list(vmean[[2]], vsd[[2]]),
                    colour = "blue", linetype = 2, na.rm = TRUE)+
      stat_function(fun = .dbeta4param,
                    args = list(alpha = result$g2alpha, beta = result$g2beta,
                                lo = lo[[2]], hi = hi[[2]]),
                    colour = "red", linetype = 2, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, ymax)) +
      scale_x_continuous(limits = c(min(lo) , max(hi))) +
      labs(title = ynames, y = "density", x = "x") +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "grey50"),
            legend.position = "bottom",
            strip.background = element_rect(colour = "black",
                                            fill = "white"))
    print(fig)
  }
  if(showFigure == TRUE){
    final <- list(result, fig)
  } else {
      final <- result
    }
  return(final)
}
