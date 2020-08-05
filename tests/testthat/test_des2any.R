library(testthat)
library(detectnorm)

context("Test des2any against des2beta and other functions")

data("metadat")

ex_beta_any <- des2any( m1i = m1,sd1i = sd1,n1i = n1,hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2, hi2i = p.max,lo2i=0,data = metadat, anyfunc = desbeta)
ex_trunc_any <- des2any( m1i = m1,sd1i = sd1,n1i = n1,hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2, hi2i = p.max,lo2i=0,data = metadat, anyfunc = destrunc)

ex_beta <- des2beta( m1i = m1,sd1i = sd1,n1i = n1,hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2, hi2i = p.max,lo2i=0,data = metadat)
ex_trunc <- des2trunc( m1i = m1,sd1i = sd1,n1i = n1,hi1i = p.max,lo1i = 0,m2i = m2,sd2i = sd2,n2i = n2, hi2i = p.max,lo2i=0,data = metadat)

test_that("Check if des2beta and des2any using desbeta give identical results", {
    expect_equivalent(
        ex_beta,
        ex_beta_any
    )
  })

test_that("Check if des2trunc and des2any using destrunc give identical results", {
    expect_equivalent(
        ex_trunc,
        ex_trunc_any
    )
  })