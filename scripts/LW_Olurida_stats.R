#### ~ O. LURIDA Length (mm) & Width (mm) STATS ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(pbkrtest) ## to use with emmeans
library(ggeffects) ## another model plotting option

#### DAY 1 =====

### load data sheet

Olurida_LWd1 <- read_csv("data/O_lurida/LWd1_stats.csv")
glimpse(Olurida_LWd1)
summary(Olurida_LWd1)
View(Olurida_LWd1)

#### Model selection ====
Lm.null <- lm(L ~ 1, data = Olurida_LWd1)
summary(Lm.null)

# Call:
#  lm(formula = L ~ 1, data = Olurida_LWd1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5246 -0.8221 -0.0921  0.8662  3.5054 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.03060    0.06371   94.66   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.274 on 399 degrees of freedom
#
#Number of Fisher Scoring iterations: 2

Lm.1 <- lm(L ~ Batch, data = Olurida_LWd1)
summary(Lm.1)

# Call:
# lm(formula = L ~ Batch, data = Olurida_LWd1)
#
# Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.4447 -0.8495 -0.0642  0.9287  3.4255 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    5.95072    0.09003  66.096   <2e-16 ***
#  BatchB: 18/24  0.15976    0.12732   1.255     0.21    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.273 on 398 degrees of freedom
#Multiple R-squared:  0.00394,	Adjusted R-squared:  0.001438 
#F-statistic: 1.574 on 1 and 398 DF,  p-value: 0.2103

Wm.null <- glm(W ~ 1, data = Olurida_LWd1)
summary(Wm.null)

#Call:
#glm(formula = W ~ 1, data = Olurida_LWd1)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8224  -0.9564  -0.0394   0.8968   4.7286  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.93844    0.06483   76.18   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.68094)
#
#Null deviance: 670.7  on 399  degrees of freedom
#Residual deviance: 670.7  on 399  degrees of freedom
#AIC: 1345.9
#
#Number of Fisher Scoring iterations: 2

Wm.1 <- glm(W ~ Batch, data = Olurida_LWd1)
summary(Wm.1)

# Call:
#glm(formula = W ~ Batch, data = Olurida_LWd1)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8196  -0.9550  -0.0394   0.8997   4.7257  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.935555   0.091792  53.769   <2e-16 ***
#  BatchB: 18/24 0.005765   0.129814   0.044    0.965    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.685156)
#
#Null deviance: 670.70  on 399  degrees of freedom
#Residual deviance: 670.69  on 398  degrees of freedom
#AIC: 1347.9
#
#Number of Fisher Scoring iterations: 2


#### *** ======================================================

#### End MHW =====
Olurida_LWend_stats <- read_csv("data/O_lurida/LW_EndMHW_stats.csv")
glimpse(Olurida_LWend_stats)
summary(Olurida_LWend_stats)
View(Olurida_LWend_stats)


### change attributes about statistical factors
Olurida_LWend_stats$SH_Temp <- as.factor(Olurida_LWend_stats$SH_Temp) ## factor
is.factor(Olurida_LWend_stats$SH_Temp) ## TRUE
Olurida_LWend_stats$SH_Tide <- as.factor(Olurida_LWend_stats$SH_Tide) ## factor
is.factor(Olurida_LWend_stats$SH_Tide) ## TRUE
Olurida_LWend_stats$MHW <- as.factor(Olurida_LWend_stats$MHW) ## character
is.factor(Olurida_LWend_stats$MHW) ## TRUE

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========

#### W ~ L ====

m.LW <- glm(W ~ L, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(m.LW)

#Call:
#glm(formula = W ~ L, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.4051  -0.5346  -0.0625   0.4652   4.5047  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.74672    0.15596   4.788 2.01e-06 ***
#  L            0.68974    0.02418  28.526  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.6766466)
#
#Null deviance: 1087.86  on 795  degrees of freedom
#Residual deviance:  537.26  on 794  degrees of freedom
#AIC: 1952

#Number of Fisher Scoring iterations: 2

### plot model

library(ggdark)
m.LW.plot <- ggpredict(m.LW, terms = "L")
plot(m.LW.plot) +
dark_theme_classic() +
#scale_color_manual(values=c("#4575B4", "#FDAE61")) +
labs(title = expression(paste("O. lurida: W ~ L")), 
#subtitle = "Gamma distribution: link = 'identity'",
     x = "Length (mm)", 
    y = "Width (mm)")

#### LENGTHS ========

#### L.m_null: L ~ 1 ===============
L.m_null <- glm(L ~ 1, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m_null)

#Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0654  -0.8736  -0.0484   0.8181   4.2126  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.33639    0.04277   148.2   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.455771)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1157.3  on 795  degrees of freedom
#AIC: 2560.9
#
#Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### L.m1: L ~ SH_Temp ===============
L.m1 <- glm(L ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m1)

# Call:
# glm(formula = L ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.9765  -0.8452  -0.0514   0.7995   4.1335  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.42529    0.06035 106.463   <2e-16 ***
#  SH_Temp21   -0.17779    0.08535  -2.083   0.0376 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.449682)
#
# Null deviance: 1157.3  on 795  degrees of freedom
# Residual deviance: 1151.0  on 794  degrees of freedom
# AIC: 2558.5
#
#Number of Fisher Scoring iterations: 2

#### L.m2: L ~ SH_Tide ===============
L.m2 <- glm(L ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m2)

# Call:
#  glm(formula = L ~ SH_Tide, family = gaussian(link = "identity"), 
#      data = Olurida_LWend_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.9816  -0.8809  -0.0786   0.8279   4.0974  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.45160    0.06016 107.235  < 2e-16 ***
#  SH_TideTide -0.23101    0.08519  -2.712  0.00684 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.44423)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1146.7  on 794  degrees of freedom
#AIC: 2555.5
#
#Number of Fisher Scoring iterations: 2

#### L.m3: L ~ MHW ===============
L.m3 <- glm(L ~ MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m3)

# Call:
#  glm(formula = L ~ MHW, family = gaussian(link = "identity"), 
#      data = Olurida_LWend_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.9885  -0.8357  -0.0378   0.8109   4.3114  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6.6205     0.0844  78.442  < 2e-16 ***
#  MHW18        -0.3828     0.1195  -3.203  0.00141 ** 
#  MHW21        -0.2399     0.1198  -2.002  0.04564 *  
#  MHW24        -0.5135     0.1194  -4.302  1.9e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.424678)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1128.3  on 792  degrees of freedom
#AIC: 2546.7
#
#Number of Fisher Scoring iterations: 2

#### L.m4: L ~ SH_Temp + SH_Tide ===============
L.m4 <- glm(L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m4)

# Call:
# glm(formula = L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0700  -0.8900  -0.0896   0.8183   4.0182  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.53999    0.07350  88.980  < 2e-16 ***
#  SH_Temp21   -0.17721    0.08501  -2.085  0.03743 *  
#  SH_TideTide -0.23056    0.08501  -2.712  0.00683 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.438171)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1140.5  on 793  degrees of freedom
#AIC: 2553.2
#
#Number of Fisher Scoring iterations: 2

#### L.m5: L ~ SH_Temp + MHW ===============
L.m5 <- glm(L ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m5)

# Call:
# glm(formula = L ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8994  -0.8618  -0.0240   0.8083   4.2329  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.70956    0.09420  71.223  < 2e-16 ***
#  SH_Temp21   -0.17815    0.08443  -2.110  0.03516 *  
#  MHW18       -0.38329    0.11925  -3.214  0.00136 ** 
#  MHW21       -0.23940    0.11955  -2.002  0.04558 *  
#  MHW24       -0.51350    0.11910  -4.311 1.83e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.418495)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1122.0  on 791  degrees of freedom
#AIC: 2544.2
#
#Number of Fisher Scoring iterations: 2

#### L.m6: L ~ SH_Tide + MHW ===============
L.m6 <- glm(L ~ SH_Tide + MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m6)

# Call:
#  glm(formula = L ~ SH_Tide + MHW, family = gaussian(link = "identity"), 
#      data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.1041  -0.8541  -0.0571   0.8259   4.1964  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.73605    0.09402  71.642  < 2e-16 ***
#  SH_TideTide -0.23115    0.08427  -2.743  0.00623 ** 
#  MHW18       -0.38342    0.11902  -3.221  0.00133 ** 
#  MHW21       -0.24044    0.11932  -2.015  0.04424 *  
#  MHW24       -0.51350    0.11887  -4.320 1.76e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.413038)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1117.7  on 791  degrees of freedom
#AIC: 2541.1
#
#Number of Fisher Scoring iterations: 2


#### L.m7: L ~ SH_Temp + SH_Tide + MHW ===============
L.m7 <- glm(L ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m7)

#Call:
#  glm(formula = L ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), 
#      data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0150  -0.8316  -0.0783   0.8206   4.1178  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.82462    0.10276  66.410  < 2e-16 ***
#  SH_Temp21   -0.17757    0.08408  -2.112  0.03501 *  
#  SH_TideTide -0.23070    0.08408  -2.744  0.00621 ** 
#  MHW18       -0.38387    0.11876  -3.232  0.00128 ** 
#  MHW21       -0.23999    0.11906  -2.016  0.04418 *  
#  MHW24       -0.51350    0.11861  -4.329 1.69e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.406884)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1111.4  on 790  degrees of freedom
#AIC: 2538.7
#
#Number of Fisher Scoring iterations: 2

#### Interactions ===============

#### L.m8: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide ===============
L.m8 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m8)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * SH_Tide, 
#    family = gaussian(link = "identity"), data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0599  -0.8264  -0.0750   0.7995   4.1534  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            6.77974    0.11098  61.089  < 2e-16 ***
#  SH_Temp21             -0.08782    0.11875  -0.740  0.45982    
#SH_TideTide           -0.14071    0.11890  -1.183  0.23700    
#MHW18                 -0.38409    0.11875  -3.234  0.00127 ** 
#  MHW21                 -0.23930    0.11905  -2.010  0.04477 *  
#  MHW24                 -0.51350    0.11860  -4.330 1.69e-05 ***
#  SH_Temp21:SH_TideTide -0.17997    0.16815  -1.070  0.28483    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.406625)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1109.8  on 789  degrees of freedom
#AIC: 2539.5
#
#Number of Fisher Scoring iterations: 2

#### L.m9: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW ===============
L.m9 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m9)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8803  -0.8603  -0.0612   0.7811   4.1987  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      6.99406    0.12549  55.736  < 2e-16 ***
#  SH_Temp21       -0.51578    0.16727  -3.084 0.002117 ** 
#  SH_TideTide     -0.23137    0.08385  -2.759 0.005924 ** 
#  MHW18           -0.64378    0.16727  -3.849 0.000128 ***
#  MHW21           -0.52930    0.16812  -3.148 0.001704 ** 
#  MHW24           -0.64309    0.16727  -3.845 0.000130 ***
#  SH_Temp21:MHW18  0.52074    0.23685   2.199 0.028196 *  
#  SH_Temp21:MHW21  0.57742    0.23745   2.432 0.015250 *  
#  SH_Temp21:MHW24  0.25919    0.23655   1.096 0.273541    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.398896)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1100.9  on 787  degrees of freedom
#AIC: 2537.1
#
#Number of Fisher Scoring iterations: 2

#### L.m10: L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW ===============
L.m10 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m10)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Tide * MHW, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0762  -0.8186  -0.0607   0.7728   4.1102  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        6.88568    0.12595  54.670  < 2e-16 ***
#  SH_Temp21         -0.17745    0.08416  -2.109  0.03529 *  
#  SH_TideTide       -0.35294    0.16789  -2.102  0.03585 *  
#  MHW18             -0.43743    0.16789  -2.606  0.00935 ** 
#  MHW21             -0.28265    0.16831  -1.679  0.09348 .  
# MHW24             -0.66156    0.16789  -3.941 8.85e-05 ***
#  SH_TideTide:MHW18  0.10705    0.23773   0.450  0.65261    
#SH_TideTide:MHW21  0.08515    0.23833   0.357  0.72099    
#SH_TideTide:MHW24  0.29613    0.23743   1.247  0.21268    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.409276)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1109.1  on 787  degrees of freedom
#AIC: 2543
#
#Number of Fisher Scoring iterations: 2

#### L.m11: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide*MHW ===============
L.m11 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide*MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(L.m11)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * SH_Tide * 
#      MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.9028  -0.8448  -0.0474   0.7714   4.2793  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)                  7.05910    0.16759  42.120  < 2e-16
#SH_Temp21                   -0.52430    0.23701  -2.212 0.027248
#SH_TideTide                 -0.36146    0.23701  -1.525 0.127649
#MHW18                       -0.78942    0.23701  -3.331 0.000907
#MHW21                       -0.66388    0.23701  -2.801 0.005220
#MHW24                       -0.80104    0.23701  -3.380 0.000762
#SH_Temp21:SH_TideTide        0.01704    0.33519   0.051 0.959468
#SH_Temp21:MHW18              0.70398    0.33519   2.100 0.036027
#SH_Temp21:MHW21              0.76669    0.33604   2.282 0.022785
#SH_Temp21:MHW24              0.27896    0.33519   0.832 0.405523
#SH_TideTide:MHW18            0.29128    0.33519   0.869 0.385111
#SH_TideTide:MHW21            0.27205    0.33693   0.807 0.419655
#SH_TideTide:MHW24            0.31590    0.33519   0.942 0.346251
#SH_Temp21:SH_TideTide:MHW18 -0.36840    0.47463  -0.776 0.437873
#SH_Temp21:SH_TideTide:MHW21 -0.37922    0.47586  -0.797 0.425740
#SH_Temp21:SH_TideTide:MHW24 -0.03954    0.47403  -0.083 0.933545
#
#(Intercept)                 ***
#  SH_Temp21                   *  
#  SH_TideTide                    
#MHW18                       ***
#  MHW21                       ** 
#  MHW24                       ***
#  SH_Temp21:SH_TideTide          
#SH_Temp21:MHW18             *  
#  SH_Temp21:MHW21             *  
#  SH_Temp21:MHW24                
#SH_TideTide:MHW18              
#SH_TideTide:MHW21              
#SH_TideTide:MHW24              
#SH_Temp21:SH_TideTide:MHW18    
#SH_Temp21:SH_TideTide:MHW21    
#SH_Temp21:SH_TideTide:MHW24    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.404385)
#
#Null deviance: 1157.3  on 795  degrees of freedom
#Residual deviance: 1095.4  on 780  degrees of freedom
#AIC: 2547.1
#
#Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### L.m12: SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank) ===============
L.m12 <- lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank), data = Olurida_LWend_stats)
summary(L.m12)

# Linear mixed model fit by REML ['lmerMod']
#Formula: L ~ SH_Temp + SH_Tide + MHW + SH_Temp * MHW + (1 | Tank)
#Data: Olurida_LWend_stats
#
#REML criterion at convergence: 2448.5
#
#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-2.78126 -0.73254 -0.04976  0.65323  3.15515 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Tank     (Intercept) 0.2586   0.5085  
#Residual             1.1898   1.0908  
#Number of obs: 796, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)      6.99495    0.25515  27.415
#SH_Temp21       -0.51578    0.15426  -3.344
#SH_TideTide     -0.23315    0.07733  -3.015
#MHW18           -0.64378    0.35668  -1.805
#MHW21           -0.53122    0.35702  -1.488
#MHW24           -0.64309    0.35668  -1.803
#SH_Temp21:MHW18  0.51641    0.21843   2.364
#SH_Temp21:MHW21  0.58021    0.21900   2.649
#SH_Temp21:MHW24  0.25919    0.21815   1.188
#
#Correlation of Fixed Effects:
#  (Intr) SH_Tm21 SH_TdT MHW18  MHW21  MHW24  SH_T21:MHW1
#SH_Temp21    -0.302                                                
#SH_TideTide  -0.152  0.000                                         
#MHW18        -0.699  0.216   0.000                                 
#MHW21        -0.699  0.216   0.002  0.500                          
#MHW24        -0.699  0.216   0.000  0.500  0.500                   
#SH_T21:MHW1   0.213 -0.706   0.002 -0.305 -0.153 -0.153            
#SH_T21:MHW21  0.214 -0.704  -0.006 -0.152 -0.308 -0.152  0.497     
#SH_T21:MHW24  0.214 -0.707   0.000 -0.153 -0.153 -0.306  0.499     
#SH_T21:MHW21
#SH_Temp21                
#SH_TideTide              
#MHW18                    
#MHW21                    
#MHW24                    
#SH_T21:MHW1              
#SH_T21:MHW21             
#SH_T21:MHW24  0.498

#### AIC/BIC Scores ===============
AIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6, L.m7, L.m8, L.m9, L.m10, L.m11, L.m12)
BIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6, L.m7, L.m8, L.m9, L.m10, L.m11, L.m12)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(L.m12), resid(L.m12))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(L.m12))
qqline(resid(L.m12))

#### Density Plot of Residuals ===============
plot(density(resid(L.m12)))

#### Pairwise Comparisons ===============
## pairwise comparison for m12
emm_Lm12a <-  emmeans(L.m12, specs = ~ SH_Temp|MHW)
emm_Lm12a
pairwise_Lm12a <- contrast(emm_Lm12a, interaction = "pairwise")
pairwise_Lm12a

#  pairwise_Lm12a
# MHW = 15:
#  SH_Temp_pairwise estimate    SE  df t.ratio p.value
# 15 - 21           0.51578 0.154 771   3.344  0.0009
#
# MHW = 18:
#  SH_Temp_pairwise estimate    SE  df t.ratio p.value
# 15 - 21          -0.00063 0.155 771  -0.004  0.9968
#
#MHW = 21:
#  SH_Temp_pairwise estimate    SE  df t.ratio p.value
#15 - 21          -0.06443 0.155 771  -0.414  0.6787
#
#MHW = 24:
#  SH_Temp_pairwise estimate    SE  df t.ratio p.value
#15 - 21           0.25659 0.154 771   1.663  0.0966
#
#Results are averaged over the levels of: SH_Tide 
#Degrees-of-freedom method: kenward-roger 

emm_Lm12b <-  emmeans(L.m12, specs = ~ MHW|SH_Temp)
emm_Lm12b
pairwise_Lm12b <- contrast(emm_Lm12b, interaction = "pairwise")
pairwise_Lm12b

# SH_Temp = 15:
#MHW_pairwise estimate    SE   df t.ratio p.value
#15 - 18       0.64378 0.357 19.4   1.805  0.0866
#15 - 21       0.53122 0.357 19.5   1.488  0.1527
#15 - 24       0.64309 0.357 19.4   1.803  0.0869
#18 - 21      -0.11256 0.357 19.5  -0.315  0.7559
#18 - 24      -0.00069 0.357 19.4  -0.002  0.9985
#21 - 24       0.11187 0.357 19.5   0.313  0.7573
#
#SH_Temp = 21:
#  MHW_pairwise estimate    SE   df t.ratio p.value
#15 - 18       0.12737 0.357 19.5   0.357  0.7250
#15 - 21      -0.04899 0.357 19.5  -0.137  0.8922
#15 - 24       0.38390 0.357 19.4   1.076  0.2950
#18 - 21      -0.17636 0.357 19.5  -0.494  0.6268
#18 - 24       0.25653 0.357 19.5   0.719  0.4807
#21 - 24       0.43289 0.357 19.5   1.213  0.2396

#Results are averaged over the levels of: SH_Tide 
#Degrees-of-freedom method: kenward-roger

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.Lm12 <- tbl_regression(L.m12, exponentiate = TRUE) ## table!
tbl.Lm12
inline_text(tbl.Lm12,  variable = SH_Temp, level = "21˚C") ##in-line text
# "0.37 (95% CI 0.17, 0.81; p=0.012)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

L.m12.plot_byMHW.Tide <- ggpredict(L.m12, terms = c("MHW", "SH_Temp", "SH_Tide"))
plot(L.m12.plot_byMHW.Tide) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Length (mm)")

ggsave(filename = "fig_output/model_Olurida_Length-MHW.Tide.png",Width (mm) = 5.10, height = 5.77, dpi = 300)

L.m12.plot_bySHtemp <- ggpredict(L.m12, terms = c("SH_Temp", "MHW", "SH_Tide"))
plot(L.m12.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste("O. lurida: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Length (mm)")

ggsave(filename = "fig_output/model_Olurida_Length-SH_Temp.png",Width (mm) = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

L.m12.DARKplot_byMHW.Tide <- ggpredict(L.m12, terms = c("MHW", "SH_Temp", "SH_Tide"))
plot(L.m12.DARKplot_byMHW.Tide) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Length (mm)")

ggsave(filename = "fig_output/DARKmodel_Olurida_Length-MHW.Tide.png",Width (mm) = 5.10, height = 5.77, dpi = 300)

L.m12.DARKplot_bySHtemp <- ggpredict(L.m12, terms = c("SH_Temp", "MHW", "SH_Tide"))
plot(L.m12.DARKplot_bySHtemp) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste("O. lurida: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Length (mm)")

ggsave(filename = "fig_output/DARKmodel_Olurida_Length-SH_Temp.png",Width (mm) = 5.10, height = 5.77, dpi = 300)

#### WIDTHS ========

#### W.m_null: W ~ 1 ===============
W.m_null <- glm(W ~ 1, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(W.m_null)

#Call:
#  glm(formula = W ~ 1, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.6302  -0.8982  -0.1397   0.7508   5.1578  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.11721    0.04146   123.4   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.368376)
#
#Null deviance: 1087.9  on 795  degrees of freedom
#Residual deviance: 1087.9  on 795  degrees of freedom
#AIC: 2511.6
#
#Number of Fisher Scoring iterations: 2


#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### W.m1: W ~ SH_Temp ===============
W.m1 <- glm(W ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(W.m1)

# Call:
# glm(formula = W ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.5806  -0.8891  -0.1648   0.7790   5.1082  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.16684    0.05862  88.142   <2e-16 ***
#  SH_Temp21   -0.09926    0.08290  -1.197    0.232    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.36763)
#
#Null deviance: 1087.9  on 795  degrees of freedom
#Residual deviance: 1085.9  on 794  degrees of freedom
#AIC: 2512.2
#
#Number of Fisher Scoring iterations: 2


#### W.m2: W ~ SH_Tide ===============
W.m2 <- glm(W ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(W.m2)

# Call:
#glm(formula = W ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.6661  -0.8911  -0.1566   0.7391   5.1939  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.15310    0.05857  87.980   <2e-16 ***
#  SH_TideTide -0.07196    0.08294  -0.868    0.386    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.368801)
#
#Null deviance: 1087.9  on 795  degrees of freedom
#Residual deviance: 1086.8  on 794  degrees of freedom
#AIC: 2512.8
#
#Number of Fisher Scoring iterations: 2

#### W.m3: W ~ MHW ===============
W.m3 <- glm(W ~ MHW, family = gaussian(link = "identity"), data = Olurida_LWend_stats)
summary(W.m3)
#
#Call:
#  glm(formula = W ~ MHW, family = gaussian(link = "identity"), 
#      data = Olurida_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8740  -0.8715  -0.1020   0.7214   4.9140  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.36105    0.08219  65.225  < 2e-16 ***
#  MHW18       -0.32044    0.11639  -2.753  0.00604 ** 
#  MHW21       -0.25950    0.11668  -2.224  0.02643 *  
#  MHW24       -0.39602    0.11624  -3.407  0.00069 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.351161)
#
#Null deviance: 1087.9  on 795  degrees of freedom
#Residual deviance: 1070.1  on 792  degrees of freedom
#AIC: 2504.5
#
# Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(W.m_null, W.m1, W.m2, W.m3)
BIC(W.m_null, W.m1, W.m2, W.m3)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(W.m4), resid(W.m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(W.m4))
qqline(resid(W.m4))

#### Density Plot of Residuals ===============
plot(density(resid(W.m4)))

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.Wm4 <- tbl_regression(W.m4, exponentiate = TRUE) ## table!
tbl.Wm4
inline_text(tbl.Wm4,  variable = MHW, level = "21") ##in-line text
# "0.77 (95% CI 0.42, 1.43; p=0.4)"


