##power Analysis

library(pwr)
# calculate minimal sample size
pwr.anova.test(k = 4,            # 4 groups (MHW x SH_Temp x SH_Tide) are compared
               f = .25,          # moderate effect size
               sig.level = .05,  # alpha/sig. level = .05
               n = 50)          # n of oysters (5 per tank, 10 tanks per heatwave)

## power = 0.84



pwr.anova.test(k = 4,            # 16 groups (MHW x SH_Temp x SH_Tide) are compared
               f = .25,          # moderate effect size
               sig.level = .05,  # alpha/sig. level = .05
               power = 0.8)     # desired power level 

## n = 45