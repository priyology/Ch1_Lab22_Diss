##power Analysis

library(pwr)
# calculate minimal sample size
pwr.anova.test(k = 16,            # 4 groups (MHW treatment) are compared
               f = .25,          # moderate effect size
               sig.level = .05,  # alpha/sig. level = .05
               n = 15)          # n of participants

## power = 0.64



pwr.anova.test(k = 16,            # 4 groups (MHW treatment) are compared
               f = .25,          # moderate effect size
               sig.level = .05,  # alpha/sig. level = .05
               power = 0.8)     # desired power level 

## n = 19.6 (~ 20)