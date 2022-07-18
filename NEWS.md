# detectnorm 1.0.0.9000

 This is a new package that works for meta-analysis with two independent groups, as well as primary studies. You can check the nonnormality with only mean, standard deviation, minimum and maximum. The minimum and maximum don't need to be the sample minimum and sample maximum. It could be the scale range. There are three main functions to the package:

 - `desbeta` if you assumed the nonnormality of the population

 - `destrunc` if you assumed the normality of the population, but truncated by the scale

 - `detectnorm` before compute the SMD or MD, check the nonnormality of the studies

 It helps you to decide if you use the robust methods in various statistics, not only meta-analysis.
