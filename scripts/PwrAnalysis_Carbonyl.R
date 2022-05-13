##power Analysis

library(pwr)
# calculate minimal sample size
pwr.anova.test(k=5,            # 5 groups are compared
               f=.25,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               power=.8)       # confint./power = .8