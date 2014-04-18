#Power calculations for the general linear model
#u = df in numerator
#v = df in denominator
#f2 = effect size
#sig.level = significance level (Type I error prob)
#power = power of test (1 minus type II error prob)
#for a 2-way ANOVA design  with replication, 2 factors each of which has 2 levels.  Model I, each MS is tested over the error.  df for Factor A is a-1, for Factor B is b-1,  A x B is (a-1)(b-1), and the error is ab(n-1).  2x2 would have the df for A, B, and A X B all be 1.  
library(pwr)
pwr.f2.test(u=1,v=NULL,f2=0.5,sig.level=0.5,power=0.8)