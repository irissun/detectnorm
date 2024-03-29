% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectnorm.R
\encoding{UTF-8}
\name{detectnorm}
\alias{detectnorm}
\title{Calculate skewness and kurtosis based on Beta or truncated normal distribution in a meta-analysis for SMD (Two independent groups)}
\usage{
detectnorm(
  m1i,
  sd1i,
  n1i,
  lo1i,
  hi1i,
  m2i,
  sd2i,
  n2i,
  lo2i,
  hi2i,
  data,
  showFigure = FALSE,
  distri = "beta",
  ...
)
}
\arguments{
\item{m1i}{vector to the means of first group}

\item{sd1i}{vector to specifiy the standard deviation of first group}

\item{n1i}{vector to specify the sample size of first group}

\item{lo1i}{vector to specify the possible minimum of the first group}

\item{hi1i}{vector to specify the possible maximum of the first group}

\item{m2i}{vector to the means of second group}

\item{sd2i}{vector to specifiy the standard deviation of second group}

\item{n2i}{vector to specify the sample size of second group}

\item{lo2i}{vector to specify the possible minimum of the second group}

\item{hi2i}{vector to specify the possible maximum of the second group}

\item{data}{the opitional original data frame containing the data for the function}

\item{showFigure}{when showFigure = TRUE, it will display all the plots (within the result as a list, result$fig) with theoretical normal curve and the truncated normal curve.}

\item{distri}{Beta distribution is used when using `distri = "beta"`; Truncated normal distribution is used when using `distri = "truncnorm"`}

\item{...}{other arguments}
}
\value{
The output of the data frame adding some columns of the possible skewness and kurtosis for each groups.
}
\description{
This function can be used to calculate the skewness and kurtosis based on the Beta distribution with the dataset used to conduct meta-analysis.
}
\examples{
#truncated normal data
data("trun_mdat")
ex <- detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
hi1i = 4,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2,
hi2i = 4,lo2i=0,distri = "truncnorm", data = trun_mdat)
head(ex)
#extremely non-normal data
data("beta_mdat")
ex2 <- detectnorm(m1i = m1,sd1i = sd1,n1i = n1,
hi1i = hi1,lo1i = lo1,m2i = m2,sd2i = sd2,n2i = n2,
hi2i = hi2,lo2i=lo2,distri = "beta", data = beta_mdat)
head(ex2)
mean(ex2$skew1)#sample skewness calculated from the sample
mean(ex2$g1_skewness) #estimated using beta
}
\references{
\insertRef{barr1999mean}{detectnorm}

\insertRef{johnson1995continuous}{detectnorm}

\insertRef{robert1995simulation}{detectnorm}

\insertRef{shah1966estimation}{detectnorm}

\insertRef{smithson2006better}{detectnorm}

\insertRef{sun2020influence}{detectnorm}
}
