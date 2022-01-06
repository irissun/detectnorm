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
