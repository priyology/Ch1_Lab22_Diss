#### ~ O. LURIDA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(stargazer) ## to print tables for glms

### load data sheet
Olurida_CI_stats <- read_csv("data/O_lurida/Olurida_CI_StatsData.csv")
glimpse(Olurida_CI_stats)
summary(Olurida_CI_stats)
View(Olurida_CI_stats)

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: CI ~ 1 ===============
m_null <- glm(CI ~ 1, family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m_null)
tab_model(m_null)

# Call:
# glm(formula = CI ~ 1, family = gaussian(link = "identity"), data = Olurida_CI_stats)

# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -6.855  -1.917  -0.791   0.764  32.434  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.2718     0.1468   49.54   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 16.24325)
#
# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 12231  on 753  degrees of freedom
# AIC: 4244.7

# Number of Fisher Scoring iterations: 2
anova(m_null)
# Analysis of Deviance Table

# Model: gaussian, link: identity

# Response: CI

# Terms added sequentially (first to last)

# Df Deviance Resid. Df Resid. Dev
# NULL                   753      12231

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: CI ~ SH_Temp ===============
m1 <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m1)
tab_model(m1)

# Call:
# glm(formula = CI ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Olurida_CI_stats)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -6.860  -1.871  -0.779   0.714  32.677  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.5143     0.2073  36.243   <2e-16 ***
#  SH_Temp21˚C  -0.4848     0.2932  -1.654   0.0986 .  
# ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 16.20593)

# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 12187  on 752  degrees of freedom
# AIC: 4243.9

# Number of Fisher Scoring iterations: 2

#### m2: CI ~ SH_Tide ===============
m2 <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m2)
tab_model(m2)

# Call:
# glm(formula = CI ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Olurida_CI_stats)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-6.805  -1.911  -0.792   0.760  32.247  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.0853     0.2072  34.194   <2e-16 ***
#  SH_TideTide   0.3740     0.2934   1.275    0.203    
# ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 16.22979)

# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 12205  on 752  degrees of freedom
# AIC: 4245

# Number of Fisher Scoring iterations: 2 Olurida_CI_stats)

# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -6.860  -1.871  -0.779   0.714  32.677  

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.5143     0.2073  36.243   <2e-16 ***
#  SH_Temp21˚C  -0.4848     0.2932  -1.654   0.0986 .  
# ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 16.20593)

# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 12187  on 752  degrees of freedom
# AIC: 4243.9

# Number of Fisher Scoring iterations: 2


#### m3: CI ~ MHW ===============
m3 <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m3)
tab_model(m3)

# Call:
# glm(formula = CI ~ MHW, family = gaussian(link = "identity"), 
#    data = Olurida_CI_stats)

# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -7.593  -1.948  -0.732   0.758  32.494  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6.1919     0.3153  19.639  < 2e-16 ***
#  MHW18˚C       1.8181     0.4235   4.293 1.99e-05 ***
#  MHW21˚C       1.2714     0.4244   2.996  0.00283 ** 
#  MHW24˚C       1.0203     0.4239   2.407  0.01634 *  
#  ---
# Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 15.90426)
#
# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 11928  on 750  degrees of freedom
# AIC: 4231.8
#
# Number of Fisher Scoring iterations: 2

#### m4: CI ~ SH_Temp + MHW ===============
m4 <- glm(CI ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m4)
tab_model(m4)

#Call:
# glm(formula = CI ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#    data = Olurida_CI_stats)

# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.596  -1.929  -0.703   0.708  32.736  

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#Intercept)   6.4336     0.3467  18.556  < 2e-16 ***
#  SH_Temp21˚C  -0.4834     0.2901  -1.666  0.09611 .  
# MHW18˚C       1.8169     0.4230   4.296 1.97e-05 ***
#  MHW21˚C       1.2726     0.4239   3.002  0.00277 ** 
#  MHW24˚C       1.0203     0.4234   2.409  0.01622 *  
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 15.86669)

# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 11884  on 749  degrees of freedom
# AIC: 4231

# Number of Fisher Scoring iterations: 2

#### Interaction ===============
#### m5: CI ~ SH_Temp + MHW + SH_Temp*MHW ===============
m5 <- glm(CI ~ SH_Temp + MHW + SH_Temp*MHW , family = gaussian(link = "identity"), data = Olurida_CI_stats)
summary(m5)
tab_model(m5)

#Call:
#  glm(formula = CI ~ SH_Temp + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
#      data = Olurida_CI_stats)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.889  -1.894  -0.677   0.706  32.217  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          6.71312    0.44468  15.096  < 2e-16 ***
#  SH_Temp21˚C         -1.04249    0.62888  -1.658  0.09780 .  
# MHW18˚C              1.82975    0.59660   3.067  0.00224 ** 
#  MHW21˚C              0.99011    0.59930   1.652  0.09894 .  
# MHW24˚C              0.22244    0.59794   0.372  0.70999    
#SH_Temp21˚C:MHW18˚C -0.02861    0.84467  -0.034  0.97298    
# SH_Temp21˚C:MHW21˚C  0.56503    0.84658   0.667  0.50471    
# SH_Temp21˚C:MHW24˚C  1.59566    0.84562   1.887  0.05955 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 15.81941)

# Null deviance: 12231  on 753  degrees of freedom
# Residual deviance: 11801  on 746  degrees of freedom
# AIC: 4231.7

# Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m6: CI ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank) ===============
m6 <- lmer(CI ~ SH_Temp + MHW + (1|Tank), data = Olurida_CI_stats)
summary(m6)
tab_model(m6)


# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
# [lmerModLmerTest]
# Formula: CI ~ SH_Temp + MHW + (1 | Tank)
# Data: Olurida_CI_stats

# REML criterion at convergence: 4196.8

# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.1603 -0.4487 -0.1402  0.1956  8.2697 

#Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept)  1.215   1.102   
# Residual             14.902   3.860   
# Number of obs: 754, groups:  Tank, 19

# Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   6.4368     0.6455  16.4623   9.972 2.19e-08 ***
#  SH_Temp21˚C  -0.4898     0.2812 734.0621  -1.742   0.0820 .  
# MHW18˚C       1.8081     0.8454  14.9538   2.139   0.0494 *  
#  MHW21˚C       1.2749     0.8459  14.9857   1.507   0.1525    
# MHW24˚C       1.0290     0.8456  14.9697   1.217   0.2425    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
#  (Intr) SH_T21 MHW18˚ MHW21˚
# SH_Temp21˚C -0.218                     
# MHW18˚C     -0.727  0.001              
# HW21˚C     -0.727 -0.001  0.555       
# MHW24˚C     -0.727  0.000  0.555  0.555

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6)
BIC(m_null, m1, m2, m3, m4, m5, m6)

#### Pairwise Comparisons ===============
## pairwise comparison for m.SH_Temp.SH_TIde.MHW_gauss
# emmeansFIXED_gauss <- emmeans(m8 ~ SH_Temp * SH_Tide | MHW)
# pairwiseFIXED_gauss <- contrast(emmeansFIXED_gauss, interaction = "pairwise")
#pairs(pairwiseFIXED_gauss, by = NULL)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m9), resid(m6))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m6))
qqline(resid(m6))

#### Density Plot of Residuals ===============
plot(density(resid(m6)))

#### Gamma Distribution ========

#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null_Gamma: CI ~ 1 ===============
m_null_Gamma <- glm(CI ~ 1, family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m_null_Gamma)
tab_model(m_null_Gamma)

#Call:
#  glm(formula = CI ~ 1, family = Gamma(link = "identity"), data = Olurida_CI_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.9576  -0.2911  -0.1130   0.1016   2.3506  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   7.2718     0.1468   49.54   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3071739)
#
#Null deviance: 147.45  on 753  degrees of freedom
#Residual deviance: 147.45  on 753  degrees of freedom
#AIC: 3782.3
#
#Number of Fisher Scoring iterations: 3

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m7: CI ~ SH_Temp ===============
m7 <- glm(CI ~ SH_Temp, family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m7)
tab_model(m7)

#Call:
#  glm(formula = CI ~ SH_Temp, family = Gamma(link = "identity"), 
#      data = Olurida_CI_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.94118  -0.28377  -0.11398   0.09213   2.41543  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   7.5143     0.2146   35.01   <2e-16 ***
#  SH_Temp21˚C  -0.4848     0.2939   -1.65   0.0994 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3075159)
#
#Null deviance: 147.45  on 753  degrees of freedom
#Residual deviance: 146.61  on 752  degrees of freedom
#AIC: 3779.8

# Number of Fisher Scoring iterations: 3

#### m8: CI ~ SH_Tide ===============
m8 <- glm(CI ~ SH_Tide, family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m8)
tab_model(m8)

#Call:
#  glm(formula = CI ~ SH_Tide, family = Gamma(link = "identity"), 
#      data = Olurida_CI_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.94502  -0.29054  -0.11372   0.09959   2.30259  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   7.0853     0.2003  35.372   <2e-16 ***
#  SH_TideTide   0.3740     0.2913   1.284      0.2    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3021084)
#
#Null deviance: 147.45  on 753  degrees of freedom
#Residual deviance: 146.95  on 752  degrees of freedom
#AIC: 3781.6

#Number of Fisher Scoring iterations: 3

#### m9: CI ~ MHW ===============
m9 <- glm(CI ~ MHW, family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m9)
tab_model(m9)

#Call:
#  glm(formula = CI ~ MHW, family = Gamma(link = "identity"), data = Olurida_CI_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0037  -0.2886  -0.1063   0.1024   2.3663  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.1919     0.2605  23.765  < 2e-16 ***
#  MHW18˚C       1.8181     0.3990   4.556 6.07e-06 ***
#  MHW21˚C       1.2714     0.3847   3.305 0.000995 ***
#  MHW24˚C       1.0203     0.3772   2.705 0.006995 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for Gamma family taken to be 0.2833053)

# Null deviance: 147.45  on 753  degrees of freedom
# Residual deviance: 141.46  on 750  degrees of freedom
# AIC: 3756

# Number of Fisher Scoring iterations: 3

#### m10: CI ~ SH_Temp + MHW ===============
m10 <- glm(CI ~ SH_Temp + MHW, family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m10)
tab_model(m10)

#Call:
#glm(formula = CI ~ SH_Temp + MHW, family = Gamma(link = "identity"), 
#    data = Olurida_CI_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.98746  -0.28558  -0.10338   0.09377   2.42403  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.4203     0.2999  21.407  < 2e-16 ***
#  SH_Temp21˚C  -0.5008     0.2789  -1.796 0.072950 .  
#MHW18˚C       1.8210     0.3976   4.579 5.46e-06 ***
# MHW21˚C       1.2954     0.3838   3.376 0.000774 ***
#  MHW24˚C       1.0786     0.3773   2.859 0.004367 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.2839702)
#
#Null deviance: 147.45  on 753  degrees of freedom
#Residual deviance: 140.55  on 749  degrees of freedom
#AIC: 3753

# Number of Fisher Scoring iterations: 6

#### Interactions ===============
#### m11: SH_Temp + MHW + SH_Temp*MHW ===============
m11 <- glm(CI ~ SH_Temp + MHW + SH_Temp*MHW , family = Gamma(link = "identity"), data = Olurida_CI_stats)
summary(m11)
tab_model(m11)

#Call:
# glm(formula = CI ~ SH_Temp + MHW + SH_Temp * MHW, family = Gamma(link = "identity"), 
#    data = Olurida_CI_stats)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.9706  -0.2839  -0.0976   0.0932   2.2952  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          6.71312    0.39266  17.096  < 2e-16 ***
#  SH_Temp21˚C         -1.04249    0.51400  -2.028  0.04290 *  
#  MHW18˚C              1.82974    0.59492   3.076  0.00218 ** 
#  MHW21˚C              0.99011    0.56561   1.751  0.08044 .  
#MHW24˚C              0.22244    0.53588   0.415  0.67819    
#SH_Temp21˚C:MHW18˚C -0.02861    0.78632  -0.036  0.97098    
#SH_Temp21˚C:MHW21˚C  0.56503    0.75781   0.746  0.45614    
#SH_Temp21˚C:MHW24˚C  1.59566    0.74312   2.147  0.03210 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.2737023)

#Null deviance: 147.45  on 753  degrees of freedom
#Residual deviance: 138.94  on 746  degrees of freedom
#AIC: 3750

#Number of Fisher Scoring iterations: 3


#### Random Factor: (1|Tank), lmer ===============
#### m12: CI ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank) ===============
m12 <- glmer(CI ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank), family = Gamma(link="identity"), data = Olurida_CI_stats)
summary(m12)
tab_model(m12)

#Generalized linear mixed model fit by maximum likelihood (Laplace
#Approximation) [glmerMod]
#Family: Gamma  ( identity )
#Formula: CI ~ SH_Temp + MHW + SH_Temp * MHW + (1 | Tank)
#Data: Olurida_CI_stats

#AIC      BIC   logLik deviance df.resid 
#3699.5   3745.7  -1839.7   3679.5      744 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.9596 -0.5201 -0.1518  0.2290  8.1871 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Tank     (Intercept) 0.7449   0.8631  
#Residual             0.2260   0.4754  
#Number of obs: 754, groups:  Tank, 19

#Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
#(Intercept)          6.74666    0.73798   9.142   <2e-16 ***
#  SH_Temp21˚C         -0.99865    0.39909  -2.502   0.0123 *  
#  MHW18˚C              1.79193    0.98668   1.816   0.0694 .  
#MHW21˚C              1.00809    0.98172   1.027   0.3045    
#MHW24˚C              0.35057    0.97758   0.359   0.7199    
#SH_Temp21˚C:MHW18˚C -0.09515    0.58910  -0.162   0.8717    
#SH_Temp21˚C:MHW21˚C  0.52530    0.58430   0.899   0.3686    
#SH_Temp21˚C:MHW24˚C  1.34455    0.57186   2.351   0.0187 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SH_Tm21˚C MHW18˚ MHW21˚ MHW24˚ SH_T21˚C:MHW1
#SH_Temp21˚C    -0.313                                             
#MHW18˚C        -0.746  0.234                                      
#MHW21˚C        -0.751  0.235     0.562                            
#MHW24˚C        -0.754  0.236     0.567  0.567                     
#SH_T21˚C:MHW1   0.212 -0.677    -0.343 -0.159 -0.160              
#SH_T21˚C:MHW21  0.214 -0.683    -0.160 -0.330 -0.161  0.463       
#SH_T21˚C:MHW24  0.218 -0.698    -0.165 -0.164 -0.310  0.473       
#SH_T21˚C:MHW21
#SH_Temp21˚C                  
#MHW18˚C                      
#MHW21˚C                      
#MHW24˚C                      
#SH_T21˚C:MHW1                
#SH_T21˚C:MHW21               
#SH_T21˚C:MHW24  0.477 

#### Publication-ready table? =============
### https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m7, m8, m9, m10, m11, m12,
          type="html",
          out="star_linear_3.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2)

#### AIC/BIC Scores ===============
AIC(m_null_Gamma, m7, m8, m9, m10, m11, m12)
BIC(m_null_Gamma, m7, m8, m9, m10, m11, m12)

#### Pairwise Comparisons ===============
## pairwise comparison for m12
emm_m12a <-  emmeans(m12, specs = ~ SH_Temp|MHW)
emm_m12a
pairwise_m12a <- contrast(emm_m12a, interaction = "pairwise")
pairwise_m12a

#MHW = 15˚C:
#SH_Temp_pairwise estimate    SE  df z.ratio p.value
#15˚C - 21˚C         0.999 0.399 Inf   2.502  0.0123
#
#MHW = 18˚C:
#  SH_Temp_pairwise estimate    SE  df z.ratio p.value
#15˚C - 21˚C         1.094 0.433 Inf   2.524  0.0116
#
#MHW = 21˚C:
#  SH_Temp_pairwise estimate    SE  df z.ratio p.value
#15˚C - 21˚C         0.473 0.427 Inf   1.109  0.2674
#
#MHW = 24˚C:
#  SH_Temp_pairwise estimate    SE  df z.ratio p.value
#15˚C - 21˚C        -0.346 0.409 Inf  -0.845  0.3983

emm_m12b <-  emmeans(m12, specs = ~ MHW|SH_Temp)
emm_m12b
pairwise_m12b <- contrast(emm_m12b, interaction = "pairwise")
pairwise_m12b

# SH_Temp = 15˚C:
# MHW_pairwise estimate    SE  df z.ratio
# 15˚C - 18˚C  -1.79194 0.987 Inf  -1.816
# 15˚C - 21˚C  -1.00809 0.982 Inf  -1.027
# 15˚C - 24˚C  -0.35057 0.978 Inf  -0.359
# 18˚C - 21˚C   0.78385 0.921 Inf   0.851
# 18˚C - 24˚C   1.44136 0.914 Inf   1.577
# 21˚C - 24˚C   0.65752 0.911 Inf   0.722
# p.value
# 0.0694
# 0.3045
# 0.7199
# 0.3946
# 0.1149
# 0.4705

# SH_Temp = 21˚C:
#  MHW_pairwise estimate    SE  df z.ratio
# 15˚C - 18˚C  -1.69679 0.960 Inf  -1.767
# 15˚C - 21˚C  -1.53338 0.962 Inf  -1.593
# 15˚C - 24˚C  -1.69512 0.968 Inf  -1.752
# 18˚C - 21˚C   0.16341 0.899 Inf   0.182
# 18˚C - 24˚C   0.00167 0.904 Inf   0.002
# 21˚C - 24˚C  -0.16174 0.908 Inf  -0.178
# p.value
# 0.0772
# 0.1111
# 0.0798
# 0.8558
# 0.9985
# 0.8586

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m12), resid(m12))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m12))
qqline(resid(m12))

#### Density Plot of Residuals ===============
plot(density(resid(m12)))


#### Plot Model ========
Olurida_CI_stats$fit <- predict(m12)

## By CI ~ SH_Temp
ggplot(Olurida_CI_stats, aes(x = SH_Temp, y = CI, group = interaction(Tank, SH_Tide), col = MHW)) +  #, shape = MHW )) + 
  #facet_grid(~ SH_Tide) +
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(CI ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Olurida_CI-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Olurida_CI_stats, aes(x = SH_Temp, y = CI, group = interaction(Tank, SH_Tide), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(CI ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Olurida_CI_byMHW.png",width = 5.10, height = 5.77, dpi = 300)

