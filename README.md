
<!-- README.md is generated from README.Rmd. Please edit that file -->

# detectnorm: A Packcage for Detectng Non-normality

<!-- badges: start -->
<!-- badges: end -->

## Description

The goal of `detectnorm` is to speculate the skewness and kurtosis based
on the Beta and truncated normal distribution. When conducting a
meta-analysis for two independent groups, we generally retrieved very
limited statistics from primary studies, and it is hard to access the
raw data. However, the non-normality of raw data could influence the
meta-analytic results greatly (Sun & Cheung, 2020). This package allows
meta-analysis researchers to speculate the skewness and kurtosis of the
raw data, even without the raw data. Instead of normal distribution, if
the researcher believes the population distribution is non-normal, then
beta-distribution could be a good choice. If the researcher believes the
population distribution is normal but truncated by the measuring ability
of the instrument, then truncated normal distribution is a good option.
The package provides not only the skewness and kurtosis estimates but
also the figures to visualize them. Now, this package could work
directly with the standardized mean difference for two independent
groups. It also works for one group of data.

## Documentation

A good start to understand the problems of non-normality in primary
studies on meta-analytic results is the following paper:

Sun, R. W., & Cheung, S. F. (2020). The influence of nonnormality from
primary studies on the standardized mean difference in meta-analysis.
*Behavior Research Methods, 52*(4), 1552-1567.

## Installation

You can install the official version within R with:

``` r
install.packages("detectnorm")
```

You can also install the development version of `detectnorm` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("irissun/detectnorm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# load package
library(detectnorm)
# examine the meta-analysis dataset by Bora et al (2019).
# Note: it is not exactly the same data as Bora et al (2019) meta-analyzed. 
# we also computed the cv (i.e., coefficient of variability) for the two groups (cv1 and cv2)
data(metadat)
head(metadat)
#>                   study n1    m1   sd1      cv1 n2    m2  sd2       cv2 p.max
#> 1 Corcoran et al., 1995 55 15.60 3.900 25.00000 30 18.30 1.60  8.743169    20
#> 2  Sarfati et al., 1997 24 18.40 6.700 36.41304 24 24.90 2.10  8.433735    28
#> 3 Sarfati et al., 1999a 25  9.90 3.690 37.27273 15 13.20 0.90  6.818182    14
#> 4 Sarfati et al., 1999b 26 18.65 6.446 34.56300 13 24.40 2.30  9.426230    28
#> 5  Russell et al., 2003  5 17.40 5.030 39.92063  7 23.86 3.84 62.337662    30
#> 6  Kington et al., 2000 16  7.75 1.240 16.00000 16  9.06 0.85  9.381898    10
#>            d
#> 1 -0.8219599
#> 2 -1.3091967
#> 3 -1.1062829
#> 4 -1.0534843
#> 5  1.4786940
#> 6 -1.2323171
```

The situations for beta distribution:

``` r
set.seed(32411)
#Using Fleishman's method to generate non-normal data
#dat1 <- rnonnorm(n = 1000, mean = 0, sd = 1, skew = 2, kurt = 5)$dat
#hist(dat1)
#psych::describe(dat1)
#Suppose we don't know about the raw data
#result <- desbeta(vmean = mean(dat1), vsd = sd(dat1),lo = min(dat1), hi = max(dat1), showFigure = TRUE, rawdata = dat1)
#result
```
