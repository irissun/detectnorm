#' @title Calculate skewness and kurtosis based on Beta or truncated normal distribution in a meta-analysis for SMD (Two independent groups)
#'
#' @description This function can be used to calculate the skewness and kurtosis based on the Beta distribution with the dataset used to conduct meta-analysis.
#' @encoding UTF-8
#'@param m1i vector to the means of first group
#'@param sd1i vector to specifiy the standard deviation of first group
#'@param n1i vector to specify the sample size of first group
#'@param lo1i vector to specify the possible minimum of the first group
#'@param hi1i vector to specify the possible maximum of the first group
#'@param m2i vector to the means of second group
#'@param sd2i vector to specifiy the standard deviation of second group
#'@param n2i vector to specify the sample size of second group
#'@param lo2i vector to specify the possible minimum of the second group
#'@param hi2i vector to specify the possible maximum of the second group
#'@param distri Beta distribution is used when using `distri = "beta"`; Truncated normal distribution is used when using `distri = "truncnorm"`
#'@param showFigure when showFigure = TRUE, it will display all the plots (within the result as a list, result$fig) with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@return The output of the data frame adding some columns of the possible skewness and kurtosis for each groups.
#'@export
#'@importFrom stats dnorm pnorm sd density
#'@importFrom utils data
#'@import ggplot2
#'@import Rdpack
#'@importFrom psych skew
#'@examples
#'#truncated normal data
#'data("trun_mdat")
#'ex <- detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
#'hi1i = 4,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
#'hi2i = 4,lo2i=0,distri = "truncnorm", data = trun_mdat)
#'head(ex)
#'#extremely non-normal data
#'data("beta_mdat")
#'ex2 <- detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
#'hi1i = hi1,lo1i = lo1,m2i = m2,sd2i = sd2,n2i = n2,
#'hi2i = hi2,lo2i=lo2,distri = "beta", data = beta_mdat)
#'head(ex2)
#'mean(ex2$skew1)#sample skewness calculated from the sample
#'mean(ex2$g1_skewness) #estimated using beta
#'@references
#'\insertRef{barr1999mean}{detectnorm}
#'
#'\insertRef{johnson1995continuous}{detectnorm}
#'
#'\insertRef{robert1995simulation}{detectnorm}
#'
#'\insertRef{shah1966estimation}{detectnorm}
#'
#'\insertRef{smithson2006better}{detectnorm}
#'
#'\insertRef{sun2020influence}{detectnorm}
#'
detectnorm <- function(m1i,sd1i,n1i, lo1i, hi1i,
                        m2i, sd2i, n2i, lo2i, hi2i,
                        showFigure = FALSE,
                        distri = "beta",
                        ...) {
  if (missing(data))
    data <- NULL
  no.data <- is.null(data)
  if (is.null(data)) {
    data <- data.frame(m1i = m1i, sd1i = sd1i, n1i = n1i, lo1i = lo1i, hi1i = hi1i,
                       m2i = m2i, sd2i = sd2i, n2i = n2i, lo2i = lo2i, hi2i = hi2i)
  } else if (!is.null(data)) {
    data <- data.frame(data)
    arg <- match.call()
    m1i <- data[[arg$m1i]]
    m2i <- data[[arg$m2i]]
    sd1i <- data[[arg$sd1i]]
    sd2i <- data[[arg$sd2i]]
    n1i <- data[[arg$n1i]]
    n2i <- data[[arg$n2i]]
    if(is.numeric(arg$lo1i)){lo1i <- rep(arg$lo1i, length(m1i))} else{
      lo1i <- data[[arg$lo1i]]
    }
    if(is.numeric(arg$lo2i)){lo2i <- rep(arg$lo2i, length(m1i))} else{
      lo2i <- data[[arg$lo2i]]
    }
    if(is.numeric(arg$hi1i)){hi1i <- rep(arg$hi1i, length(m1i))} else{
      hi1i <- data[[arg$hi1i]]
    }
    if(is.numeric(arg$hi2i)){hi2i <- rep(arg$hi2i, length(m1i))} else{
      hi2i <- data[[arg$hi2i]]
    }
  }
  finalre1 <- list()
  finalre2 <- list()
  fig1 <- list()
  fig2 <- list()
  if (distri == "beta") {
    distri <- 'desbeta'
    distri <- get(distri)
  } else if (distri == "truncnorm"){
    distri <- 'destrunc'
    distri <- get(distri)
  } else {
    distri <- get(distri)
  }
  if (showFigure == TRUE) {
    for (i in 1:nrow(data)) {
      g1 <- do.call(distri, list(m1i[[i]], sd1i[[i]], lo1i[[i]],hi1i[[i]],
                                 showFigure=TRUE))
      finalre1[[i]] <- g1$dat
      fig1[[i]] <- g1$fig + labs(title = paste("Group 1 in Study ",
                                               i, sep = ""))
      g2 <- do.call(distri, list(m2i[[i]], sd2i[[i]], lo2i[[i]], hi2i[[i]],
                                 showFigure=TRUE))
      finalre2[[i]] <- g2$dat
      fig2[[i]] <- g2$fig + labs(title = paste("Group 2 in Study ",
                                               i, sep = ""))
    }
    finalre1 <- do.call(rbind, finalre1)
    colnames(finalre1) <- paste(colnames(finalre1), 1, sep = "")
    finalre2 <- do.call(rbind, finalre2)
    colnames(finalre2) <- paste(colnames(finalre2), 2, sep = "")
    finalre <- cbind(finalre1, finalre2)
    fig <- list(fig1, fig2)
    dat <- data.frame(data, finalre)
    dat <- list(dat = data, fig = fig)
  } else {
    for (i in 1:nrow(data)) {
      finalre1[[i]] <- do.call(distri, list(m1i[[i]], sd1i[[i]],  lo1i[[i]], hi1i[[i]]))
      finalre2[[i]] <- do.call(distri, list(m2i[[i]], sd2i[[i]], lo2i[[i]], hi2i[[i]]))
    }
    finalre1 <- do.call(rbind, finalre1)
    colnames(finalre1) <- paste("g1_", colnames(finalre1), sep = "")
    finalre2 <- do.call(rbind, finalre2)
    colnames(finalre2) <- paste("g2_", colnames(finalre2), sep = "")
    finalre <- cbind(finalre1, finalre2)
    dat <- data.frame(data, finalre)
  }
  return(dat)
}
