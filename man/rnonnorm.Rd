% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnonnorm.R
\name{rnonnorm}
\alias{rnonnorm}
\title{Non-normal Distribution}
\usage{
rnonnorm(n, mean = 0, sd = 1, skew = 0, kurt = 0)
}
\arguments{
\item{n}{number of observations}

\item{mean}{mean}

\item{sd}{standard deviation}

\item{skew}{skewness}

\item{kurt}{kurtosis}
}
\value{
A list of two objects: non-normal data is `dat`; and the other is the Fleishman Coeffficients used to generate the distributions.
}
\description{
Generating Non-normal data with specified skewness and kurtosis using Fleishman's Method
}
\details{
This function can be used to generate non-normal data with specified skewness and kurtosis using Fleishman's Power Method.
}
\examples{
set.seed(341031)
exdat <- rnonnorm(n = 100, mean = 1, sd = 2, skew = 3, kurt = 2)$dat
hist(exdat)

}
\references{
\insertRef{fleishman1978method}{detectnorm}
}
