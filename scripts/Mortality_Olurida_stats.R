#### ~ O. LURIDA MORTALITY DATA ~ =====

library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### ** SH Mortality ** ===============

### load data sheet
Olurida_SH_Morts <- read_csv("data/O_lurida/logit.mort_Olurida_EndSH.csv")
glimpse(Olurida_SH_Morts)
summary(Olurida_SH_Morts)
View(Olurida_SH_Morts)

### change attributes about statistical factors
Olurida_SH_Morts$SH_Temp <- as.factor(Olurida_SH_Morts$SH_Temp) ## factor
is.factor(Olurida_SH_Morts$SH_Temp) ## TRUE
Olurida_SH_Morts$SH_Tide <- as.factor(Olurida_SH_Morts$SH_Tide) ## factor
is.factor(Olurida_SH_Morts$SH_Tide) ## TRUE

#### O. LURIDA STATS: Stress Hardening mortality ===============
#### Logistic Regression ===============
### https://www.tutorialspoint.com/r/r_logistic_regression.htm
### http://sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

#### m_null: Mortality ~ 1 ===============
m_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m_null)

# Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = Olurida_SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1504  -0.1504  -0.1504  -0.1504   2.9958  
#
#Coefficients:
#  Estimate Std. Error z value
#(Intercept)   -4.476      0.237  -18.88
#Pr(>|z|)    
#(Intercept)   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#
#
#(Dispersion parameter for binomial family taken to be 1)
##
#Null deviance: 197.34  on 1599  degrees of freedom
#Residual deviance: 197.34  on 1599  degrees of freedom
#AIC: 199.34

#### m1: Mortality ~ SH_Temp ===============
m1 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m1)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = Olurida_SH_Morts)

#Deviance Residuals: 
#  Min       1Q   Median       3Q  
#-0.1739  -0.1739  -0.1227  -0.1227  
#Max  
#3.1282  
#
#Coefficients:
#  Estimate Std. Error z value
#(Intercept)  -4.8853     0.4098 -11.922
#SH_Temp21     0.7007     0.5025   1.394
#Pr(>|z|)    
#(Intercept)   <2e-16 ***
#  SH_Temp21      0.163    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’
#0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 197.34  on 1599  degrees of freedom
#Residual deviance: 195.28  on 1598  degrees of freedom
#AIC: 199.28

#Number of Fisher Scoring iterations: 7

#### m1a: Mortality ~ SH_Temp + (1|Tank) ===============
m1a <- glmer(Mortality ~ SH_Temp  + (1|Tank), family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m1a)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + (1 | Tank)
#Data: Olurida_SH_Morts
#
#AIC      BIC   logLik deviance df.resid 
#199.3    215.5    -96.7    193.3     1597 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1989 -0.1109 -0.0867 -0.0867 11.5347 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0.3989   0.6316  
#Number of obs: 1600, groups:  Tank, 20

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.0516     0.4821 -10.478   <2e-16 ***
#  SH_Temp21     0.6423     0.5917   1.085    0.278    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#SH_Temp21 -0.719

#### m2: Mortality ~ SH_Tide ===============
m2 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m2)

#Call:
#  glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#      data = Olurida_SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q  
#-0.1739  -0.1739  -0.1227  -0.1227  
#Max  
#3.1282  
#
#Coefficients:
#  Estimate Std. Error
#(Intercept)  -4.8853     0.4098
#SH_TideTide   0.7007     0.5025
#z value Pr(>|z|)    
#(Intercept) -11.922   <2e-16 ***
#  SH_TideTide   1.394    0.163    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
#‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 197.34  on 1599  degrees of freedom
#Residual deviance: 195.28  on 1598  degrees of freedom
#AIC: 199.28

# Number of Fisher Scoring iterations: 7

#### m2a: Mortality ~ SH_Tide + (1|Tank) ===============
m2a <- glmer(Mortality ~ SH_Tide  + (1|Tank), family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m2a)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Tide + (1 | Tank)
#Data: Olurida_SH_Morts
#
#AIC      BIC   logLik deviance df.resid 
#199.3    215.5    -96.7    193.3     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1989 -0.1109 -0.0867 -0.0867 11.5347 

#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0.3989   0.6316  
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.0516     0.4821 -10.478   <2e-16 ***
#  SH_TideTide   0.6423     0.5917   1.085    0.278    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_TideTide -0.719


#### m3: Mortality ~ SH_Temp + SH_Tide + (1|Tank) ===============
m3 <- glmer(Mortality ~ SH_Temp + SH_Tide + (1|Tank), family = binomial(link = "logit"), data = Olurida_SH_Morts)
summary(m3)

#Generalized linear mixed model fit by maximum likelihood (Laplace
#Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + SH_Tide + (1 | Tank)
#Data: Olurida_SH_Morts
#
#AIC      BIC   logLik deviance df.resid 
#200.1    221.6    -96.1    192.1     1596 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1982 -0.0976 -0.0976 -0.0741 13.4901 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0.2852   0.534   
#Number of obs: 1600, groups:  Tank, 20

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.3644     0.5727  -9.367   <2e-16 ***
#  SH_Temp21     0.6396     0.5724   1.117    0.264    
#SH_TideTide   0.6396     0.5724   1.117    0.264    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) SH_T21
#SH_Temp21   -0.585       
#SH_TideTide -0.585 -0.005

#### ** MHW Mortality ** ===============

### load data sheet
Olurida_MHW_Morts <- read_csv("data/O_lurida/logit.mort_Olurida_EndMHW.csv")
glimpse(Olurida_MHW_Morts)
summary(Olurida_MHW_Morts)
View(Olurida_MHW_Morts)

### change attributes about statistical factors
Olurida_MHW_Morts$SH_Temp <- as.factor(Olurida_MHW_Morts$SH_Temp) ## factor
is.factor(Olurida_MHW_Morts$SH_Temp) ## TRUE
Olurida_MHW_Morts$SH_Tide <- as.factor(Olurida_MHW_Morts$SH_Tide) ## factor
is.factor(Olurida_MHW_Morts$SH_Tide) ## TRUE
Olurida_MHW_Morts$MHW <- as.factor(Olurida_MHW_Morts$MHW) ## character
is.factor(Olurida_MHW_Morts$MHW) ## TRUE

#### m_null2: Mortality ~ 1 ===============
m_null2 <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m_null2)

# Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = Olurida_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1418  -0.1418  -0.1418  -0.1418   3.0349  
#
#Coefficients:
#  Estimate Std. Error z value
#(Intercept)  -4.5951     0.2513  -18.29
#Pr(>|z|)    
#(Intercept)   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 179.2  on 1599  degrees of freedom
#Residual deviance: 179.2  on 1599  degrees of freedom
#AIC: 181.2
#
#Number of Fisher Scoring iterations: 7


#### m3: Mortality ~ SH_Temp ===============
m3 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m3)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = Olurida_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1504  -0.1504  -0.1326  -0.1326   3.0785  
#
#Coefficients:
#  Estimate Std. Error z value
#(Intercept)  -4.4761     0.3352 -13.353
#SH_Temp21    -0.2538     0.5064  -0.501
#Pr(>|z|)    
#(Intercept)   <2e-16 ***
#  SH_Temp21      0.616    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 179.20  on 1599  degrees of freedom
#Residual deviance: 178.95  on 1598  degrees of freedom
#AIC: 182.95
#
#Number of Fisher Scoring iterations: 7

#### m3a: Mortality ~ SH_Temp + (1|Tank) ===============
m3a <- glmer(Mortality ~ SH_Temp + (1|Tank), family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m3a)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#  glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + (1 | Tank)
#Data: Olurida_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#185.0    201.1    -89.5    179.0     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1067 -0.1067 -0.0940 -0.0940 10.6436 
#
#Random effects:
#  Groups Name        Variance  Std.Dev. 
#Tank   (Intercept) 2.866e-16 1.693e-08
#Number of obs: 1600, groups:  Tank, 20

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.4761     0.3352 -13.352   <2e-16 ***
#  SH_Temp21    -0.2538     0.5065  -0.501    0.616    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_Temp21 -0.662
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### m4: Mortality ~ SH_Tide ===============
m4 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m4)

#Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = Olurida_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1664  -0.1664  -0.1120  -0.1120   3.1860  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.0689     0.4485  -11.30   <2e-16 ***
#  SH_TideTide   0.7960     0.5416    1.47    0.142    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 179.20  on 1599  degrees of freedom
#Residual deviance: 176.88  on 1598  degrees of freedom
#AIC: 180.88
#
#Number of Fisher Scoring iterations: 7

#### m4a: Mortality ~ SH_Tide + (1|Tank) ===============
m4a <- glmer(Mortality ~ SH_Tide + (1|Tank), family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m4a)

# Generalized linear mixed model fit by maximum likelihood
#(Laplace Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Tide + (1 | Tank)
#Data: Olurida_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#184.2    200.3    -89.1    178.2     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1125 -0.1125 -0.0869 -0.0869 11.5036 
#
#Random effects:
#  Groups Name        Variance  Std.Dev. 
#Tank   (Intercept) 1.663e-13 4.079e-07
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.8853     0.4098 -11.922   <2e-16 ***
#  SH_TideTide   0.5159     0.5188   0.994     0.32    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_TideTide -0.790
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


#### m5: Mortality ~ MHW ===============
m5 <- glm(Mortality ~ MHW, family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m5)

#Call:
#  glm(formula = Mortality ~ MHW, family = binomial(link = "logit"), 
#      data = Olurida_MHW_Morts)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.17386  -0.15861  -0.15861  -0.00005   2.96041  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -20.57     886.52  -0.023    0.981
#MHW18          16.20     886.52   0.018    0.985
#MHW21          16.20     886.52   0.018    0.985
#MHW24          16.38     886.52   0.018    0.985
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 179.20  on 1599  degrees of freedom
#Residual deviance: 169.82  on 1596  degrees of freedom
#AIC: 177.82
#
#Number of Fisher Scoring iterations: 19

#### m5a: Mortality ~ MHW + (1|Tank) ===============
m5a <- glmer(Mortality ~ MHW + (1|Tank), family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m5a)

#Generalized linear mixed model fit by maximum likelihood
#(Laplace Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ MHW + (1 | Tank)
#Data: Olurida_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#179.8    206.7    -84.9    169.8     1595 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1234 -0.1125 -0.1125  0.0000  8.8882 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0        0       
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -22.57      37.34  -0.604    0.546
#MHW18          18.20      37.34   0.487    0.626
#MHW21          18.20      37.34   0.487    0.626
#MHW24          18.38      37.34   0.492    0.623

#Correlation of Fixed Effects:
#  (Intr) MHW18  MHW21 
#MHW18 -1.000              
#MHW21 -1.000  1.000       
#MHW24 -1.000  1.000  1.000
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### m6: Mortality ~ SH_Temp + SH_Tide + MHW + (1|Tank) ===============
m6 <- glmer(Mortality ~ SH_Temp + SH_Tide + MHW + (1|Tank), family = binomial(link = "logit"), data = Olurida_MHW_Morts)
summary(m6)

# Generalized linear mixed model fit by maximum likelihood (Laplace
#Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + SH_Tide + MHW + (1 | Tank)
#Data: Olurida_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#181.2    218.9    -83.6    167.2     1593 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1540 -0.1236 -0.0942  0.0000 12.0630 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0        0       
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept) -22.9040    65.8258  -0.348    0.728
#SH_Temp21    -0.2552     0.5078  -0.503    0.615
#SH_TideTide   0.7988     0.5426   1.472    0.141
#MHW18        18.1789    65.8275   0.276    0.782
#MHW21        18.1789    65.8268   0.276    0.782
#MHW24        18.3642    65.8261   0.279    0.780
#
#Correlation of Fixed Effects:
#  (Intr) SH_T21 SH_TdT MHW18  MHW21 
#SH_Temp21   -0.002                            
#SH_TideTide -0.003 -0.001                     
#MHW18       -1.000 -0.002 -0.003              
#MHW21       -1.000 -0.002 -0.003  1.000       
#MHW24       -1.000 -0.002 -0.003  1.000  1.000
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### ** Outplanting Mortality ** ===============

### load data sheet
Olurida_Outplant_Morts <- read_csv("data/O_lurida/logit.mort_Olurida_Endoutplanting.csv")
glimpse(Olurida_Outplant_Morts)
summary(Olurida_Outplant_Morts)
View(Olurida_Outplant_Morts)

### change attributes about statistical factors
Olurida_Outplant_Morts$SH_Temp <- as.factor(Olurida_Outplant_Morts$SH_Temp) ## factor
is.factor(Olurida_Outplant_Morts$SH_Temp) ## TRUE
Olurida_MHW_Morts$SH_Tide <- as.factor(Olurida_Outplant_Morts$SH_Tide) ## factor
is.factor(Olurida_Outplant_Morts$SH_Tide) ## TRUE

#### m_null3: Mortality ~ 1 ===============
m_null3 <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Olurida_Outplant_Morts)
summary(m_null3)

#Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = Olurida_Outplant_Morts)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-1.312  -1.312   1.049   1.049   1.049  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.3100     0.0506   6.125 9.04e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 2180.1  on 1599  degrees of freedom
#Residual deviance: 2180.1  on 1599  degrees of freedom
#AIC: 2182.1
#
#Number of Fisher Scoring iterations: 4

#### m7: Mortality ~ SH_Temp ===============
m7 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Olurida_Outplant_Morts)
summary(m7)

#Call:
#glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#    data = Olurida_Outplant_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.2761  -0.7208   0.3949   0.3949   1.7176  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.5123     0.1342   18.72   <2e-16 ***
#  SH_Temp21    -3.7277     0.1584  -23.53   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 2180.1  on 1599  degrees of freedom
#Residual deviance: 1286.6  on 1598  degrees of freedom
#AIC: 1290.6
#
#Number of Fisher Scoring iterations: 5

#### m8: Mortality ~ SH_Tide ===============
m8 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Olurida_Outplant_Morts)
summary(m8)

# Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = Olurida_Outplant_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4564  -1.1774   0.9220   0.9858   1.1774  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.63556    0.07431   8.553  < 2e-16 ***
#  SH_TideTide -0.63556    0.10258  -6.196 5.79e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 2180.1  on 1599  degrees of freedom
#Residual deviance: 2141.2  on 1598  degrees of freedom
#AIC: 2145.2
#
#Number of Fisher Scoring iterations: 4

#### m9: Mortality ~ SH_Temp + SH_Tide ===============
m9 <- glm(Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), data = Olurida_Outplant_Morts)
summary(m9)

# Call:
# glm(formula = Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), 
#    data = Olurida_Outplant_Morts)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.6127  -0.4990   0.2588   0.4990   1.4680  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   3.3797     0.1840  18.371   <2e-16 ***
#  SH_Temp21    -4.0411     0.1772 -22.799   <2e-16 ***
#  SH_TideTide  -1.3592     0.1621  -8.384   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 2180.1  on 1599  degrees of freedom
# Residual deviance: 1206.6  on 1597  degrees of freedom
# AIC: 1212.6
#
# Number of Fisher Scoring iterations: 5

#### m10: Mortality ~ SH_Temp + SH_Tide + SH_Temp*SH_Tide  ===============
m10 <- glm(Mortality ~ SH_Temp + SH_Tide + SH_Temp*SH_Tide, family = binomial(link = "logit"), data = Olurida_Outplant_Morts)
summary(m10)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + SH_Temp * SH_Tide, 
#      family = binomial(link = "logit"), data = Olurida_Outplant_Morts)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.94788  -0.00008   0.00008   0.57012   1.25058  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             1.7346     0.1400  12.388   <2e-16 ***
#  SH_Temp21              -1.9050     0.1723 -11.058   <2e-16 ***
#  SH_TideTide            17.8315   537.7007   0.033    0.974    
#SH_Temp21:SH_TideTide -37.2271   760.4236  -0.049    0.961    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 2180.10  on 1599  degrees of freedom
#Residual deviance:  889.79  on 1596  degrees of freedom
#AIC: 897.79
#
#Number of Fisher Scoring iterations: 18

#### AIC/BIC Scores ===============
AIC(m_null3, m7, m8, m9, m10)
BIC(m_null3, m7, m8, m9, m10)

#### Test Assumptions: m9 ===============
#???

tbl.m9 <- tbl_regression(m9, exponentiate = TRUE) ## table!
tbl.m9

m9.Plot <- ggpredict(m9, terms = c("SH_Temp","SH_Tide"))
plot(m9.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Stress Hardening Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/model_Olurida_Morts-Outplant.png", width = 5.10, height = 5.77, dpi = 300)

m9.TempPlot <- ggpredict(m9, terms = c("SH_Temp"))#))
plot(m9.TempPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Stress Hardening Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/model_Olurida_Morts-Outplant_Temp.png", width = 5.10, height = 5.77, dpi = 300)


m9.TidePlot <- ggpredict(m9, terms = c("SH_Tide"))#))
plot(m9.TidePlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Tide", 
       y = "Mortality")

ggsave(filename = "fig_output/model_Olurida_Morts-Outplant_Tide.png", width = 5.10, height = 5.77, dpi = 300)



##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m9.DARKPlot <- ggpredict(m9, terms = c("SH_Temp","SH_Tide"))
plot(m9.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Stress Hardening Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_Olurida_Morts-Outplant.png", width = 5.10, height = 5.77, dpi = 300)

m9.TempDARKPlot <- ggpredict(m9, terms = c("SH_Temp"))#))
plot(m9.TempDARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Stress Hardening Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_Olurida_Morts-Outplant_Temp.png", width = 5.10, height = 5.77, dpi = 300)


m9.TideDARKPlot <- ggpredict(m9, terms = c("SH_Tide"))#))
plot(m9.TideDARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("O. lurida: glm(Mortality ~ SH_Temp + SH_Tide")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Tide", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_Olurida_Morts-Outplant_Tide.png", width = 5.10, height = 5.77, dpi = 300)
