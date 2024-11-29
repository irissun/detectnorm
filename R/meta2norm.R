#'@title Calculate skewness and kurtosis based on Beta distribution in a meta-analysis for SMD (Two independent or dependent groups)
#'
#'@description This function can be used to calculate the skewness and kurtosis based on the Beta distribution with the dataset used to conduct meta-analysis.
#'@encoding UTF-8
#'@param m1 vector to the means of first group
#'@param sd1 vector to specifiy the standard deviation of first group
#'@param lo1 vector to specify the possible minimum of the first group
#'@param hi1 vector to specify the possible maximum of the first group
#'@param m2 vector to the means of second group
#'@param sd2 vector to specifiy the standard deviation of second group
#'@param lo2 vector to specify the possible minimum of the second group
#'@param hi2 vector to specify the possible maximum of the second group
#'@param design specify if the two groups are independent or dependent.
#'@param cor_xy specify the correlations between the two groups when they are dependent.
#'@param distri Beta distribution is used when using `distri = "beta"`; Truncated normal distribution is used when using `distri = "truncnorm"`
#'@param metadat The data.frame of meta-analytic data with means, stantand deviations, minimums, maximums, design (specify whether "dep" or "ind"), and correlations (for dependent two groups).
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
#'ex <- meta2norm(m1 = m1,sd1 = sd1, hi1 = hi1, lo1 = lo1,
#'m2 = m2, sd2 = sd2, hi2 = hi2, lo2 = lo2, distri = "trunc",
#'metadat = trun_mdat)
#'head(ex)
#'#extremely non-normal data
#'data("beta_mdat")
#'ex2 <- meta2norm(m1 = m1,sd1 = sd1, hi1 = hi1, lo1 = lo1,
#'m2 = m2, sd2 = sd2, hi2 = hi2, lo2 = lo2, distri = "beta",
#'metadat = beta_mdat)
#'head(ex2)
#'mean(ex2$skew1)#sample skewness calculated from the sample
#'mean(ex2$g1skew) #estimated using beta
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
meta2norm <- function(m1, m2,
                       sd1, sd2,
                       hi1,hi2, lo1, lo2,
                       cor_xy = NA,
                       distri = "beta",
                       design = "ind",
                       metadat = NULL,
                       showFigure = FALSE,
                        ...) {
  if(is.null(metadat) == FALSE & is.data.frame(metadat) == TRUE){
    m1 <- eval(substitute(m1), metadat)
    m2 <- eval(substitute(m2), metadat)
    sd1 <- eval(substitute(sd1), metadat)
    sd2 <- eval(substitute(sd2), metadat)
    hi1 <- eval(substitute(hi1), metadat)
    hi2 <- eval(substitute(hi2), metadat)
    lo1 <- eval(substitute(lo1), metadat)
    lo2 <- eval(substitute(lo2), metadat)
    cor_xy <- eval(substitute(cor_xy), metadat)
    if(length(cor_xy) == 1) {
      cor_xy <- rep(cor_xy, length(m1))
    }
    design <- eval(substitute(design), metadat)
    if(length(design) == 1) {
      design <- rep(design, length(m1))
    }

    final_dat <- list()
    figure <- list()
    for(i in 1:nrow(metadat)){
      if(distri == "beta"){
        if(is.na(cor_xy[[i]]) | design[[i]] == "ind"){
          #independent design
          if(showFigure == TRUE){
            indat <- des2beta(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                              hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]),
                              design = design[[i]], showFigure = TRUE)
            figure[[i]] <- indat[[2]]
            indat <- indat[[1]]
          } else if(showFigure == FALSE){
            indat <- des2beta(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                              hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]),
                              design = design[[i]])
          }

          tail <- ifelse(m1[[i]] < m2[[i]] & indat$g1skew >0 & indat$g2skew < 0, "toward",
          ifelse(m1[[i]] > m2[[i]] & indat$g1skew < 0 & indat$g2skew > 0, "toward", "away"))
          final_dat[[i]] <- c(indat$g1skew, indat$g1kurt,
                              indat$g2skew, indat$g2kurt,
                              tail)
        } else if(!is.na(cor_xy[[i]]) & design[[i]] == "dep"){
          #dependent design
          if(showFigure == TRUE){
            depdat <- des2beta(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                               hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]),
                               cor_xy = cor_xy[[i]], design = design[[i]],
                               showFigure = TRUE)
            figure[[i]] <- depdat[[2]]
            depdat <- depdat[[1]]
          } else if(showFigure == FALSE){
            depdat <- des2beta(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                               hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]),
                               cor_xy = cor_xy[[i]], design = design[[i]])
          }
          tail <- ifelse(m1[[i]] < m2[[i]] & depdat$g1skew >0 & depdat$g2skew < 0, "toward",
                         ifelse(m1[[i]] > m2[[i]] & depdat$g1skew < 0 & depdat$g2skew > 0, "toward", "away"))
          final_dat[[i]] <- c(depdat$g1skew, depdat$g1kurt, depdat$g2skew, depdat$g2kurt, tail)
        } else {
          warning("Check the design and cor_xy. If independent, specify cor_xy as NA.")
        }
      } else if(distri == "trunc"){
        if(is.na(cor_xy[[i]]) & design[[i]] == "ind"){
          #independent design
          if(showFigure == TRUE){
            indat <- des2trunc(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                              hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]),
                              showFigure = TRUE)
            figure[[i]] <- indat[[2]]
            indat <- indat[[1]]
          } else if(showFigure == FALSE){
            indat <- des2trunc(vmean=c(m1[[i]], m2[[i]]), vsd=c(sd1[[i]], sd2[[i]]),
                               hi = c(hi1[[i]], hi2[[i]]), lo = c(lo1[[i]], lo2[[i]]))
          }
          tail <- ifelse(m1[[i]] < m2[[i]] & indat$g1skew >0 & indat$g2skew < 0, "toward",
                         ifelse(m1[[i]] > m2[[i]] & indat$g1skew < 0 & indat$g2skew > 0, "toward", "away"))
          final_dat[[i]] <- c(indat$g1skew, indat$g1kurt, indat$g2skew, indat$g2kurt, tail)

        } else {
          warning("The function for correlated truncated distribution is under development.")
        }
      }
    }
   final_dat <- data.frame(do.call(rbind, final_dat))
   colnames(final_dat) <- c("g1skew", "g1kurt", "g2skew", "g2kurt", "check_tail")
   final_dat$g1skew <- as.numeric(final_dat$g1skew)
   final_dat$g1kurt <- as.numeric(final_dat$g1kurt)
   final_dat$g2skew <- as.numeric(final_dat$g2skew)
   final_dat$g2kurt <- as.numeric(final_dat$g2kurt)
   final_dat <- cbind(metadat, final_dat)
   if(showFigure == TRUE) final_dat <- list(final_dat = final_dat, figure = figure)
  }

  return(final_dat)
}
