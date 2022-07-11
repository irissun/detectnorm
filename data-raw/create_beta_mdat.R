# Generating meta-analytic data with non-normality using beta distributions
# 40 studies
# population mean1 = 1, mean2 = 1.5, sd1=sd2=1
k <- 40 # study size
set.seed(302321)
nlist <- as.integer(stats::runif(n = k, min = 20, max = 200))
datlist <- list()
for(i in 1:40){
  study1 <-
    detectnorm::rnonnorm(n = nlist[[i]], mean = 1, sd = 1, skew = 4, kurt = 2)$dat
  study2 <-
    detectnorm::rnonnorm(n = nlist[[i]], mean = 1.5, sd = 1, skew = -4, kurt = 2)$dat
  datlist[[i]] <- data.frame(study = i, n1 = nlist[[i]], m1 = mean(study1),
                             sd1 = sd(study1), lo1 = min(study1),hi1 = max(study1),
                             n2 = nlist[[i]], m2 = mean(study2), sd2 = sd(study2),
                             lo2 = min(study2), hi2 = max(study2),skew1=psych::skew(study1),
                             skew2=psych::skew(study2))
}
#' need to revise detectnorm
beta_mdat <- do.call(rbind, datlist)
usethis::use_data(beta_mdat, compress = "xz")
