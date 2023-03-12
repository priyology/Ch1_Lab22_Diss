#### ~ C. SIKAMEA Length (cm) & Width (cm) STATS ~ =====

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

Csikamea_LWd1 <- read_csv("data/C_sikamea/LWd1_stats.csv")
glimpse(Csikamea_LWd1)
summary(Csikamea_LWd1)
View(Csikamea_LWd1)

#### Model selection ====
m.LWd1 <- lm(L ~ W, data = Csikamea_LWd1)
summary(m.LWd1)

#Call:
#  lm(formula = L ~ W, data = Csikamea_LWd1)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.81965 -0.16642  0.00036  0.15490  0.74738 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.3579     0.3438   3.950 0.000147 ***
#  W             0.7276     0.1763   4.127 7.73e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.2776 on 98 degrees of freedom
#Multiple R-squared:  0.1481,	Adjusted R-squared:  0.1394 
#F-statistic: 17.03 on 1 and 98 DF,  p-value: 7.728e-05

### plot model

library(ggdark)
m.LWd1.plot <- ggpredict(m.LWd1, terms = "W")
plot(m.LWd1.plot) +
  dark_theme_classic() +
  #scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("C. sikamea: L ~ W")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Width (cm)", 
       y = "Length (cm)")

#### *** ======================================================

#### End MHW =====
Csikamea_LWend_stats <- read_csv("data/C_sikamea/LW_EndMHW_stats.csv")
glimpse(Csikamea_LWend_stats)
summary(Csikamea_LWend_stats)
View(Csikamea_LWend_stats)

### change attributes about statistical factors
Csikamea_LWend_stats$SH_Temp <- as.factor(Csikamea_LWend_stats$SH_Temp) ## factor
is.factor(Csikamea_LWend_stats$SH_Temp) ## TRUE
Csikamea_LWend_stats$SH_Tide <- as.factor(Csikamea_LWend_stats$SH_Tide) ## factor
is.factor(Csikamea_LWend_stats$SH_Tide) ## TRUE
Csikamea_LWend_stats$MHW <- as.factor(Csikamea_LWend_stats$MHW) ## character
is.factor(Csikamea_LWend_stats$MHW) ## TRUE

#### C. SIKAMEA STATS ===============

#### Gaussian Distribution ========

#### W ~ L ====

m.LW <- glm(W ~ L, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(m.LW)

#Call:
#glm(formula = W ~ L, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.65323  -0.10476  -0.00866   0.09886   0.64452  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.17274    0.05329   22.01   <2e-16 ***
#  L            0.24450    0.01963   12.46   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.02572083)
#
#Null deviance: 23.590  on 763  degrees of freedom
#Residual deviance: 19.599  on 762  degrees of freedom
#AIC: -624.45
#
#Number of Fisher Scoring iterations: 2


### plot model

library(ggdark)
m.LW.plot <- ggpredict(m.LW, terms = "L")
plot(m.LW.plot) +
  dark_theme_classic() +
  #scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("C. sikamea: W ~ L")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Length (cm)", 
       y = "Width (cm)")


#### LENGTHS ========

#### L.m_null: L ~ 1 ===============
L.m_null <- glm(L ~ 1, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m_null)

#Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.82595  -0.20145  -0.01845   0.20280   0.76405  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.6989     0.0107   252.2   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08750035)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 66.763  on 763  degrees of freedom
#AIC: 309.95
#
#Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### L.m1: L ~ SH_Temp ===============
L.m1 <- glm(L ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m1)

#Call:
#glm(formula = L ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.82560  -0.20261  -0.01286   0.20065   0.77614  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.67786    0.01501 178.439   <2e-16 ***
#  SH_Temp21    0.04274    0.02136   2.001   0.0458 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.0871574)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 66.414  on 762  degrees of freedom
#AIC: 307.95
#
#Number of Fisher Scoring iterations: 2


#### L.m2: L ~ SH_Tide ===============
L.m2 <- glm(L ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m2)

# Call:
#  glm(formula = L ~ SH_Tide, family = gaussian(link = "identity"), 
#      data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7671  -0.2029  -0.0090   0.1957   0.8161  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.75911    0.01458 189.244  < 2e-16 ***
#  SH_TideTide -0.12423    0.02095  -5.929 4.61e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08375088)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 63.818  on 762  degrees of freedom
#AIC: 277.49
#
#Number of Fisher Scoring iterations: 2


#### L.m3: L ~ MHW ===============
L.m3 <- glm(L ~ MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m3)

# Call:
#glm(formula = L ~ MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.80905  -0.19673  -0.02442   0.19818   0.79221  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.67079    0.02113 126.416  < 2e-16 ***
#  MHW18       -0.03322    0.02972  -1.118 0.264045    
#MHW21        0.11562    0.03004   3.849 0.000128 ***
#  MHW24        0.03326    0.02972   1.119 0.263550    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08480684)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 64.453  on 760  degrees of freedom
#AIC: 289.05
#
#Number of Fisher Scoring iterations: 2


#### L.m4: L ~ SH_Temp + SH_Tide ===============
L.m4 <- glm(L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m4)

# Call:
# glm(formula = L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.76162  -0.19844  -0.01427   0.20038   0.83739  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.73793    0.01782 153.646  < 2e-16 ***
#  SH_Temp21    0.04301    0.02090   2.058   0.0399 *  
#  SH_TideTide -0.12432    0.02091  -5.947 4.17e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08339666)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 63.465  on 761  degrees of freedom
#AIC: 275.24
#
#Number of Fisher Scoring iterations: 2

#### L.m5: L ~ SH_Temp + MHW ===============
L.m5 <- glm(L ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m5)

# Call:
#glm(formula = L ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.83105  -0.20238  -0.01976   0.19638   0.80452  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.64948    0.02341 113.173  < 2e-16 ***
#  SH_Temp21    0.04401    0.02103   2.093 0.036719 *  
#  MHW18       -0.03414    0.02966  -1.151 0.250037    
#MHW21        0.11564    0.02997   3.858 0.000124 ***
#  MHW24        0.03256    0.02966   1.098 0.272616    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08443147)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 64.083  on 759  degrees of freedom
#AIC: 286.65
#
#Number of Fisher Scoring iterations: 2


#### L.m6: L ~ SH_Tide + MHW ===============
L.m6 <- glm(L ~ SH_Tide + MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m6)

# Call:
#glm(formula = L ~ SH_Tide + MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.74628  -0.19785  -0.01878   0.20285   0.84365  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.73034    0.02295 118.979  < 2e-16 ***
#  SH_TideTide -0.12299    0.02062  -5.964 3.76e-09 ***
#  MHW18       -0.03255    0.02907  -1.120 0.263215    
#MHW21        0.11426    0.02938   3.889 0.000109 ***
#  MHW24        0.03393    0.02907   1.167 0.243492    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08111658)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 61.567  on 759  degrees of freedom
#AIC: 256.05
#
#Number of Fisher Scoring iterations: 2

#### L.m7: L ~ SH_Temp + SH_Tide + MHW ===============
L.m7 <- glm(L ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m7)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.76837  -0.19842  -0.00402   0.19831   0.86512  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.70896    0.02496 108.544  < 2e-16 ***
#  SH_Temp21    0.04425    0.02056   2.152 0.031714 *  
#  SH_TideTide -0.12308    0.02057  -5.983 3.38e-09 ***
#  MHW18       -0.03347    0.02900  -1.154 0.248804    
#MHW21        0.11427    0.02931   3.899 0.000105 ***
#  MHW24        0.03323    0.02900   1.146 0.252218    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08073037)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 61.194  on 758  degrees of freedom
#AIC: 253.4
#
#Number of Fisher Scoring iterations: 2

#### Interactions ===============

#### L.m8: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide ===============
L.m8 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m8)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * SH_Tide, 
#    family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.76145  -0.20206  -0.00797   0.20173   0.85839  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            2.70249    0.02688 100.548  < 2e-16 ***
#  SH_Temp21              0.05720    0.02864   1.997 0.046187 *  
#  SH_TideTide           -0.10988    0.02891  -3.800 0.000156 ***
#  MHW18                 -0.03326    0.02902  -1.146 0.252109    
#MHW21                  0.11427    0.02932   3.897 0.000106 ***
#  MHW24                  0.03338    0.02901   1.150 0.250325    
#SH_Temp21:SH_TideTide -0.02674    0.04116  -0.650 0.516204    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.08079199)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 61.160  on 757  degrees of freedom
#AIC: 254.98
#
#Number of Fisher Scoring iterations: 2

#### L.m9: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW ===============
L.m9 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m9)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.76253  -0.20067  -0.00694   0.20303   0.87717  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      2.697001   0.030455  88.558  < 2e-16 ***
#  SH_Temp21        0.069036   0.041296   1.672 0.094987 .  
#SH_TideTide     -0.123169   0.020597  -5.980 3.44e-09 ***
#  MHW18           -0.030608   0.040849  -0.749 0.453921    
#MHW21            0.141809   0.040849   3.472 0.000547 ***
#  MHW24            0.051019   0.040742   1.252 0.210874    
#SH_Temp21:MHW18 -0.006702   0.058089  -0.115 0.908174    
#SH_Temp21:MHW21 -0.056890   0.058714  -0.969 0.332888    
#SH_Temp21:MHW24 -0.036354   0.058085  -0.626 0.531588  

#### L.m10: L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW ===============
L.m10 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m10)

# Call:
#  glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Tide * MHW, family = gaussian(link = "identity"), 
#      data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.75362  -0.19797  -0.00942   0.19936   0.82438  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        2.66986    0.03028  88.185  < 2e-16 ***
#  SH_Temp21          0.04516    0.02047   2.206  0.02766 *  
#  SH_TideTide       -0.04324    0.04105  -1.053  0.29256    
#MHW18              0.04315    0.04030   1.071  0.28460    
#MHW21              0.12994    0.04040   3.216  0.00135 ** 
#  MHW24              0.09492    0.04030   2.355  0.01876 *  
#  SH_TideTide:MHW18 -0.15741    0.05775  -2.726  0.00657 ** 
#  SH_TideTide:MHW21 -0.03124    0.05840  -0.535  0.59288    
#SH_TideTide:MHW24 -0.12689    0.05775  -2.197  0.02831 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.07997711)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 60.383  on 755  degrees of freedom
#AIC: 249.21
#
#Number of Fisher Scoring iterations: 2

#### L.m11: L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide*MHW ===============
L.m11 <- glm(L ~ SH_Temp + SH_Tide + MHW + SH_Temp*SH_Tide*MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(L.m11)

#Call:
#  glm(formula = L ~ SH_Temp + SH_Tide + MHW + SH_Temp * SH_Tide * 
#        MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.76337  -0.18821  -0.01034   0.19164   0.81704  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)                  2.636960   0.039999  65.925  < 2e-16
#SH_Temp21                    0.112332   0.057154   1.965 0.049735
#SH_TideTide                 -0.000585   0.057154  -0.010 0.991836
#MHW18                        0.049780   0.056568   0.880 0.379138
#MHW21                        0.211000   0.056568   3.730 0.000206
#MHW24                        0.115740   0.056568   2.046 0.041101
#SH_Temp21:SH_TideTide       -0.087570   0.082166  -1.066 0.286873
#SH_Temp21:MHW18             -0.014092   0.080617  -0.175 0.861282
#SH_Temp21:MHW21             -0.165500   0.080828  -2.048 0.040952
#SH_Temp21:MHW24             -0.042766   0.080617  -0.530 0.595932
#SH_TideTide:MHW18           -0.165046   0.081275  -2.031 0.042637
#SH_TideTide:MHW21           -0.141679   0.081275  -1.743 0.081707
#SH_TideTide:MHW24           -0.132243   0.081047  -1.632 0.103168
#SH_Temp21:SH_TideTide:MHW18  0.017528   0.115566   0.152 0.879487
#SH_Temp21:SH_TideTide:MHW21  0.227019   0.116883   1.942 0.052479
#SH_Temp21:SH_TideTide:MHW24  0.013237   0.115553   0.115 0.908831
#
#(Intercept)                 ***
#  SH_Temp21                   *  
#  SH_TideTide                    
#MHW18                          
#MHW21                       ***
#  MHW24                       *  
#  SH_Temp21:SH_TideTide          
#SH_Temp21:MHW18                
#SH_Temp21:MHW21             *  
#  SH_Temp21:MHW24                
#SH_TideTide:MHW18           *  
#  SH_TideTide:MHW21           .  
#SH_TideTide:MHW24              
#SH_Temp21:SH_TideTide:MHW18    
#SH_Temp21:SH_TideTide:MHW21 .  
#SH_Temp21:SH_TideTide:MHW24    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.0799977)
#
#Null deviance: 66.763  on 763  degrees of freedom
#Residual deviance: 59.838  on 748  degrees of freedom
#AIC: 256.29
#
#Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### FINAL MODEL: L.m12: SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank) ===============
L.m12 <- lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank), data = Csikamea_LWend_stats)
summary(L.m12)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: L ~ SH_Temp + SH_Tide + MHW + SH_Tide * MHW + (1 | Tank)
#Data: Csikamea_LWend_stats
#
#REML criterion at convergence: 277.1
#
#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-2.64708 -0.69966 -0.02839  0.71529  2.91289 
#
#Random effects:
#  Groups   Name        Variance  Std.Dev.
#Tank     (Intercept) 0.0001523 0.01234 
#Residual             0.0798538 0.28258 
#Number of obs: 764, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)
#(Intercept)         2.66981    0.03075  69.99763  86.817  < 2e-16
#SH_Temp21           0.04518    0.02046 740.07700   2.209  0.02751
#SH_TideTide        -0.04325    0.04102 740.86901  -1.054  0.29216
#MHW18               0.04319    0.04102  55.97140   1.053  0.29685
#MHW21               0.12996    0.04112  56.48975   3.161  0.00253
#MHW24               0.09497    0.04102  55.97150   2.315  0.02428
#SH_TideTide:MHW18  -0.15744    0.05771 740.31267  -2.728  0.00652
#SH_TideTide:MHW21  -0.03127    0.05836 740.62570  -0.536  0.59219
#SH_TideTide:MHW24  -0.12699    0.05771 740.31547  -2.201  0.02807#
#
#(Intercept)       ***
# SH_Temp21         *  
#  SH_TideTide          
#MHW18                
#MHW21             ** 
#  MHW24             *  
#  SH_TideTide:MHW18 ** 
#  SH_TideTide:MHW21    
#SH_TideTide:MHW24 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SH_T21 SH_TdT MHW18  MHW21  MHW24  SH_TT:MHW1
#SH_Temp21   -0.326                                              
#SH_TideTide -0.648  0.006                                       
#MHW18       -0.669 -0.003  0.484                                
#MHW21       -0.669  0.000  0.483  0.501                         
#MHW24       -0.669 -0.003  0.484  0.502  0.501                  
#SH_TT:MHW18  0.463 -0.011 -0.711 -0.685 -0.343 -0.344           
#SH_TT:MHW21  0.454  0.000 -0.703 -0.340 -0.679 -0.340  0.500    
#SH_TT:MHW24  0.462 -0.008 -0.711 -0.344 -0.343 -0.685  0.505    
#SH_TT:MHW21
#SH_Temp21              
#SH_TideTide            
#MHW18                  
#MHW21                  
#MHW24                  
#SH_TT:MHW18            
#SH_TT:MHW21            
#SH_TT:MHW24  0.500     

#### L.m13: L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + SH_Temp*SH_Tide*MHW + (1|Tank) ===============
L.m13 <- lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + SH_Temp*SH_Tide*MHW + (1|Tank), data = Csikamea_LWend_stats)
summary(L.m13)

# 
# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
# [lmerModLmerTest]
# Formula: 
#  L ~ SH_Temp + SH_Tide + MHW + SH_Tide * MHW + SH_Temp * SH_Tide *  
#  MHW + (1 | Tank)
#Data: Csikamea_LWend_stats
#
#REML criterion at convergence: 295.3
#
#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-2.68144 -0.67790 -0.03385  0.68441  2.88649 
#
#Random effects:
#  Groups   Name        Variance  Std.Dev.
#Tank     (Intercept) 0.0001502 0.01226 
#Residual             0.0798750 0.28262 
#Number of obs: 764, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error         df
#(Intercept)                  2.637e+00  4.034e-02  1.850e+02
#SH_Temp21                    1.122e-01  5.711e-02  7.327e+02
#SH_TideTide                 -7.063e-04  5.711e-02  7.327e+02
#MHW18                        4.978e-02  5.705e-02  1.850e+02
#MHW21                        2.110e-01  5.705e-02  1.850e+02
#MHW24                        1.157e-01  5.705e-02  1.850e+02
#SH_TideTide:MHW18           -1.650e-01  8.121e-02  7.326e+02
#SH_TideTide:MHW21           -1.416e-01  8.121e-02  7.330e+02
#SH_TideTide:MHW24           -1.322e-01  8.099e-02  7.331e+02
#SH_Temp21:SH_TideTide       -8.733e-02  8.211e-02  7.336e+02
#SH_Temp21:MHW18             -1.401e-02  8.056e-02  7.326e+02
#SH_Temp21:MHW21             -1.655e-01  8.077e-02  7.327e+02
#SH_Temp21:MHW24             -4.267e-02  8.056e-02  7.326e+02
#SH_Temp21:SH_TideTide:MHW18  1.730e-02  1.155e-01  7.332e+02
#SH_Temp21:SH_TideTide:MHW21  2.269e-01  1.168e-01  7.334e+02
#SH_Temp21:SH_TideTide:MHW24  1.298e-02  1.155e-01  7.334e+02

#t value Pr(>|t|)    
#(Intercept)                  65.364  < 2e-16 ***
#  SH_Temp21                     1.965 0.049742 *  
#  SH_TideTide                  -0.012 0.990136    
#MHW18                         0.873 0.384058    
#MHW21                         3.698 0.000286 ***
#  MHW24                         2.029 0.043930 *  
#  SH_TideTide:MHW18            -2.031 0.042597 *  
#  SH_TideTide:MHW21            -1.744 0.081558 .  
#SH_TideTide:MHW24            -1.633 0.102988    
#SH_Temp21:SH_TideTide        -1.064 0.287839    
#SH_Temp21:MHW18              -0.174 0.861983    
#SH_Temp21:MHW21              -2.049 0.040850 *  
#  SH_Temp21:MHW24              -0.530 0.596516    
#SH_Temp21:SH_TideTide:MHW18   0.150 0.880972    
#SH_Temp21:SH_TideTide:MHW21   1.943 0.052451 .  
#SH_Temp21:SH_TideTide:MHW24   0.112 0.910518    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### AIC/BIC Scores ===============
AIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6, L.m7, L.m8, L.m9, L.m10, L.m11, L.m12, L.m13)
BIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6, L.m7, L.m8, L.m9, L.m10, L.m11, L.m12, L.m13)

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
emm_Lm12a <-  emmeans(L.m12, specs = ~ SH_Tide|MHW)
emm_Lm12a
pairwise_Lm12a <- contrast(emm_Lm12a, interaction = "pairwise")
pairwise_Lm12a

# MHW = 15:
#SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide     0.0432 0.0410 741   1.054  0.2922
#
#MHW = 18:
#  SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide     0.2007 0.0406 740   4.944  <.0001
#
#MHW = 21:
#  SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide     0.0745 0.0415 740   1.795  0.0730
#
#MHW = 24:
#  SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide     0.1702 0.0406 740   4.194  <.0001
#
#Results are averaged over the levels of: SH_Temp 
#Degrees-of-freedom method: kenward-roger 

emm_Lm12b <-  emmeans(L.m12, specs = ~ MHW|SH_Tide)
emm_Lm12b
pairwise_Lm12b <- contrast(emm_Lm12b, interaction = "pairwise")
pairwise_Lm12b

# SH_Tide = No Tide:
#MHW_pairwise estimate     SE   df t.ratio p.value
#15 - 18       -0.0432 0.0410 55.3  -1.053  0.2969
#15 - 21       -0.1300 0.0411 55.8  -3.161  0.0025
#15 - 24       -0.0950 0.0410 55.3  -2.315  0.0243
#18 - 21       -0.0868 0.0410 55.3  -2.115  0.0389
#18 - 24       -0.0518 0.0409 54.8  -1.265  0.2111
#21 - 24        0.0350 0.0410 55.3   0.853  0.3973
#
#SH_Tide = Tide:
#  MHW_pairwise estimate     SE   df t.ratio p.value
#15 - 18        0.1142 0.0421 60.8   2.715  0.0086
#15 - 21       -0.0987 0.0429 65.2  -2.303  0.0245
#15 - 24        0.0320 0.0421 60.8   0.761  0.4495
#18 - 21       -0.2129 0.0425 63.5  -5.005  <.0001
#18 - 24       -0.0822 0.0417 59.1  -1.970  0.0536
#21 - 24        0.1307 0.0425 63.5   3.073  0.0031
#
#Results are averaged over the levels of: SH_Temp 
#Degrees-of-freedom method: kenward-roger 

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.Lm12 <- tbl_regression(L.m12, exponentiate = TRUE) ## table!
tbl.Lm12
inline_text(tbl.Lm12,  variable = MHW, level = "21") ##in-line text
# "1.14 (95% CI 1.05, 1.24; p=0.003)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

L.m12.plot_byMHW.Tide <- ggpredict(L.m12, terms = c("MHW", "SH_Tide"))
plot(L.m12.plot_byMHW.Tide) +
  theme_classic() +
  scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste("C. sikamea: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Length (cm)")

ggsave(filename = "fig_output/model_Csikamea_Length-SH_Temp.png", width = 5.10, height = 5.77, dpi = 300)

L.m12.plot_bySHtemp <- ggpredict(L.m12, terms = c("SH_Tide", "MHW"))
plot(L.m12.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste("C. sikamea: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = " ", 
       y = "Length (cm)")

ggsave(filename = "fig_output/model_Csikamea_Length-Tide.MHW.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

L.m12.DARKplot_byMHW.Tide <- ggpredict(L.m12, terms = c("MHW", "SH_Tide"))
plot(L.m12.plot_byMHW.Tide) +
  dark_theme_classic() +
  scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste("C. sikamea: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Length (cm)")

ggsave(filename = "fig_output/DARK-SH_Temp.png", width = 5.10, height = 5.77, dpi = 300)

L.m12.DARKplot_bySHtemp <- ggpredict(L.m12, terms = c("SH_Tide", "MHW"))
plot(L.m12.plot_bySHtemp) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste("C. sikamea: lmer(L ~ SH_Temp + SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = " ", 
       y = "Length (cm)")

ggsave(filename = "fig_output/DARKmodel_Csikamea_Length-Tide.MHW.png", width = 5.10, height = 5.77, dpi = 300)

#### WIDTHS ========

#### W.m_null: W ~ 1 ===============
W.m_null <- glm(W ~ 1, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(W.m_null)

# Call:
#glm(formula = W ~ 1, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.68965  -0.11090   0.00285   0.11560   0.54135  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.832647   0.006361   288.1   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.03091806)
#
#Null deviance: 23.59  on 763  degrees of freedom
#Residual deviance: 23.59  on 763  degrees of freedom
#AIC: -484.84
#
#Number of Fisher Scoring iterations: 2


#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### W.m1: W ~ SH_Temp ===============
W.m1 <- glm(W ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(W.m1)

#Call:
#  glm(formula = W ~ SH_Temp, family = gaussian(link = "identity"), 
#      data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.68040  -0.11565   0.00485   0.11485   0.55060  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.841651   0.008932 206.185   <2e-16 ***
#  SH_Temp21   -0.018248   0.012715  -1.435    0.152    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.03087518)
#
#Null deviance: 23.590  on 763  degrees of freedom
#Residual deviance: 23.527  on 762  degrees of freedom
#AIC: -484.91
#
#Number of Fisher Scoring iterations: 2

#### W.m2: W ~ SH_Tide ===============
W.m2 <- glm(W ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(W.m2)

# Call:
#glm(formula = W ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.68619  -0.11239   0.00311   0.11336   0.53811  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.835891   0.008863 207.149   <2e-16 ***
#  SH_TideTide -0.006699   0.012735  -0.526    0.599    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.0309474)
#
#Null deviance: 23.590  on 763  degrees of freedom
#Residual deviance: 23.582  on 762  degrees of freedom
#AIC: -483.12
#
#Number of Fisher Scoring iterations: 2

#### W.m3: W ~ MHW ===============
W.m3 <- glm(W ~ MHW, family = gaussian(link = "identity"), data = Csikamea_LWend_stats)
summary(W.m3)

# Call:
#glm(formula = W ~ MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.71735  -0.11391   0.00337   0.11671   0.51365  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.86035    0.01272 146.288   <2e-16 ***
#  MHW18       -0.04424    0.01789  -2.473   0.0136 *  
#  MHW21       -0.02444    0.01808  -1.352   0.1769    
#MHW24       -0.04144    0.01789  -2.316   0.0208 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.03072766)
#
#Null deviance: 23.590  on 763  degrees of freedom
#Residual deviance: 23.353  on 760  degrees of freedom
#AIC: -486.57
#
#Number of Fisher Scoring iterations: 2

#### W.m4: W ~ MHW + (1|Tank) ===============
W.m4 <- lmer(W ~ MHW +  (1|Tank), data = Csikamea_LWend_stats)
summary(W.m4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: W ~ MHW + (1 | Tank)
#Data: Csikamea_LWend_stats
#
#REML criterion at convergence: -476.1
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-4.3354 -0.6602  0.0241  0.6741  2.7629 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Tank     (Intercept) 0.001017 0.03188 
#Residual             0.029911 0.17295 
#Number of obs: 764, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  1.85968    0.01899 16.03752  97.908   <2e-16 ***
#  MHW18       -0.04361    0.02680 15.89340  -1.627    0.123    
#MHW21       -0.02397    0.02692 16.18822  -0.890    0.386    
#MHW24       -0.04108    0.02680 15.89340  -1.533    0.145    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) MHW18  MHW21 
# MHW18 -0.709              
# MHW21 -0.705  0.500       
# MHW24 -0.709  0.502  0.500

#### AIC/BIC Scores ===============
AIC(W.m_null, W.m1, W.m2, W.m3, W.m4)
BIC(W.m_null, W.m1, W.m2, W.m3, W.m4)

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
# "0.98 (95% CI 0.92, 1.03; p=0.4)"

