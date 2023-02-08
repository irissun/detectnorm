#' Calculate skewness and kurtosis based on Beta distribution in two group
#'
#'This function can be used to calculate the skewness and kurtosis based on the Beta distribution. Also, this function estimate the shape parameters alpha and beta.
#'@param vmean sample mean of the beta distributed data
#'@param vsd sample standard deviation of the beta distributed data
#'@param lo minimum possible value
#'@param hi maximum possible value
#'@param design specify if the two groups are independent or dependent.
#'@param cor_xy specify the correlations between the two groups when they are dependent.
#'@param showFigure when showFigure = TRUE, it will display the plots with theoretical normal curve and the truncated normal curve.
#'@param ... other arguments
#'@return If `showFigure = TRUE`, the output will be a list with two objects: one is the data frame of shape parameters (alpha and beta), mean and standard deviation of standard beta distribution (mean and sd), and skewness and kurtosis; the other is the theoretical figures of beta and normal distributions of the two groups. If `showFigure = FALSE`, the output will be only the data frame.
#'@import ggplot2
#'@export
#'@examples
#'data('beta_mdat')
#'desbeta(vmean=beta_mdat$m2[6], vsd=beta_mdat$sd2[6],
#'hi = beta_mdat$hi2[6], lo = beta_mdat$lo2[6], showFigure = TRUE)
#'
#'@seealso \code{\link{destrunc}}
#'
#'@references
#'\insertRef{johnson1995continuous}{detectnorm}
#'
#'\insertRef{smithson2006better}{detectnorm}
#'
#'\insertRef{olkin2014cons}{detectnorm}
#'
desbeta <- function(vmean, vsd, lo, hi, cor_xy,
                    design = c("independent", "dependent"),
                    showFigure = FALSE,
                     ...) {
  design <- match.arg(design)
  switch(design,
         independent = out <- desbeta_ind(vmean = vmean, vsd = vsd, lo = lo,
                                          hi = hi, showFigure = showFigure, ...),
         dependent = out <- desbeta_dep(vmean = vmean, vsd = vsd, lo = lo,
                                        hi = hi, showFigure = showFigure,
                                        cor_xy = cor_xy, ...))
  out
}
