#Power calculation for two probabilities with same sample sizes
#fill in all but one of the variables to calculate the last one (so you can find needed sample size for a given power and effect size, or the power of a comparison for a given sample size and effect)
#h is effect size
#n is number of observations per sample
#sig.level = type I error prob
#power = type II error prob
#alternative = character string specifying alternative hypothesis ("two.sided", "greater" or "less")
library(pwr)
pwr.2p.test(h=0.5, n=NULL, sig.level=0.05, power=0.8, alternative=c("two.sided"))