#' Calculate skewness and kurtosis based on truncated normal distribution in a meta-analysis for SMD
#'
#'This function can be used to calculate the skewness and kurtosis based on the truncated normal distribution with the dataset used to conduct meta-analysis. Also, it can be used to estimate the mean and variance of the parent distribution (the distribution before truncated).
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@example
#'des2trunc()
des2trunc <- function(m1i,sd1i,n1i,lo1i,hi1i,
                     m2i, sd2i, n2i, lo2i, hi2i, data,
                     showFigure = FALSE,
                     ...){
  if (missing(data))
    data <- NULL
  no.data <- is.null(data)
  if (is.null(data)) {

  }
  else {
    if (!is.data.frame(data))
      data <- data.frame(data)
  }
  arg <- match.call()
  if(!is.null(data)){
    m1i <- data[[arg$m1i]]
    m2i <- data[[arg$m2i]]
    sd1i <- data[[arg$sd1i]]
    sd2i <- data[[arg$sd2i]]
    n1i <- data[[arg$n1i]]
    n2i <- data[[arg$n2i]]
    hi1i <- data[[arg$hi1i]]
    hi2i <- data[[arg$hi2i]]
    if(length(lo1i)==1){lo1i <- rep(lo1i, length(m1i))}else{
      lo1i <- data[[arg$lo1i]]
    }
    if(length(lo2i)==1){lo2i <- rep(lo2i, length(m1i))}else{
      lo2i <- data[[arg$lo2i]]
    }
  }
  finalre1 <- list()
  finalre2 <- list()
  fig1 <- list()
  fig2 <- list()
  if(showFigure == TRUE){
    for (i in 1:nrow(data)){
      g1 <- destrunc(vmean = m1i[[i]], vsd = sd1i[[i]],
                                hi = hi1i[[i]], lo = lo1i[[i]],
                                showFigure = TRUE)
      finalre1[[i]] <- g1$dat
      fig1[[i]] <- g1$fig + labs(title = paste("Group 1 in Study ",
                                 i, sep=""))
      g2 <- destrunc(vmean = m2i[[i]], vsd = sd2i[[i]],
                                hi = hi2i[[i]], lo = lo2i[[i]],
                                showFigure = TRUE)
      finalre2[[i]] <- g2$dat
      fig2[[i]] <- g2$fig + labs(title = paste("Group 2 in Study ",
                                 i, sep=""))
    }
    finalre1 <- do.call(rbind, finalre1)
    colnames(finalre1) <- paste(colnames(finalre1),1, sep="")
    finalre2 <- do.call(rbind,finalre2)
    colnames(finalre2) <- paste(colnames(finalre2),2, sep="")
    finalre <- cbind(finalre1, finalre2)
    fig <- list(fig1, fig2)
    dat <- data.frame(dat, finalre)
    dat <- list(dat = dat, fig = fig)
  } else {
    for (i in 1:nrow(data)){
        finalre1[[i]] <- destrunc(vmean = m1i[[i]], vsd = sd1i[[i]],
                                  hi = hi1i[[i]], lo = lo1i[[i]])
        finalre2[[i]] <- destrunc(vmean = m2i[[i]], vsd = sd2i[[i]],
                                  hi = hi2i[[i]], lo = lo2i[[i]])
      }
    finalre1 <- do.call(rbind, finalre1)
    colnames(finalre1) <- paste(colnames(finalre1),1, sep="")
    finalre2 <- do.call(rbind,finalre2)
    colnames(finalre2) <- paste(colnames(finalre2),2, sep="")
    finalre <- cbind(finalre1, finalre2)
    dat <- data.frame(dat, finalre)
  }
  return(dat)
}
