#' Calculate skewness and kurtosis based on Beta or truncated normal distribution in a meta-analysis for SMD (Two independent groups)
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution with the dataset used to conduct meta-analysis.
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
#'@param showFigure when showFigure = TRUE, it will display all the plots (within the result as a list, result$fig) with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@export
#'@examples
#'\dontrun{
#'data("metadat")
#'ex <- detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
#'hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
#'hi2i = p.max,lo2i=0,distri = "beta", data = metadat)
#'}
#'
detectnorm <- function(m1i,sd1i,n1i, lo1i, hi1i,
                        m2i, sd2i, n2i, lo2i, hi2i,
                        data,
                        showFigure = FALSE,
                        distri = "beta",
                        ...) {
  if (missing(data))
    data <- NULL
  no.data <- is.null(data)
  if (is.null(data)) {

  } else {
    if(!is.data.frame(data))
      data <- data.frame(data)
  }
  arg <- match.call()
  if (!is.null(data)) {
    m1i <- data[[arg$m1i]]
    m2i <- data[[arg$m2i]]
    sd1i <- data[[arg$sd1i]]
    sd2i <- data[[arg$sd2i]]
    n1i <- data[[arg$n1i]]
    n2i <- data[[arg$n2i]]
    hi1i <- data[[arg$hi1i]]
    hi2i <- data[[arg$hi2i]]
    if (length(lo1i) == 1) {
      lo1i <- rep(lo1i, length(m1i))
    } else {
      lo1i <- data[[arg$lo1i]]
    }
    if (length(lo2i) == 1) {
      lo2i <- rep(lo2i, length(m1i))
    } else {
      lo2i <- data[[arg$lo2i]]
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
      g1 <- do.call(distri, list(m1i[[i]], sd1i[[i]], hi1i[[i]], lo1i[[i]],
                                 showFigure=TRUE))
      finalre1[[i]] <- g1$dat
      fig1[[i]] <- g1$fig + labs(title = paste("Group 1 in Study ",
                                               i, sep = ""))
      g2 <- do.call(distri, list(m2i[[i]], sd2i[[i]], hi2i[[i]], lo2i[[i]],
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
      finalre1[[i]] <- do.call(distri, list(m1i[[i]], sd1i[[i]], hi1i[[i]], lo1i[[i]]))
      finalre2[[i]] <- do.call(distri, list(m2i[[i]], sd2i[[i]], hi2i[[i]], lo2i[[i]]))
    }
    finalre1 <- do.call(rbind, finalre1)
    colnames(finalre1) <- paste(colnames(finalre1), 1, sep = "")
    finalre2 <- do.call(rbind, finalre2)
    colnames(finalre2) <- paste(colnames(finalre2), 2, sep = "")
    finalre <- cbind(finalre1, finalre2)
    dat <- data.frame(data, finalre)
  }
  return(dat)
}
