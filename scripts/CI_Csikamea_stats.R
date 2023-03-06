#### ~ C. SIKAMEA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(sjmisc)
library(sjlabelled)
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ## another model plotting option

### load data sheet
Csikamea_CI_stats <- read_csv("data/C_sikamea/Csikamea_CI_StatsData.csv")
glimpse(Csikamea_CI_stats)
summary(Csikamea_CI_stats)
View(Csikamea_CI_stats)

#### C. SIKAMEA CONDITION INDEX STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: CI ~ 1 ===============
m_null <- glm(CI ~ 1, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m_null)
tab_model(m_null)

# Call:
# glm(formula = CI ~ 1, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -3.9951  -0.8252  -0.1573   0.6690   6.0094  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.13298    0.05071    81.5   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.818089)
#
# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1283.6  on 706  degrees of freedom
# AIC: 2432
#
# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: CI ~ SH_Temp ===============
m1 <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m1)
tab_model(m1, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = CI ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Csikamea_CI_stats)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -3.8663  -0.8262  -0.1362   0.6266   6.1382  

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.26284    0.07159   59.55   <2e-16 ***
#  SH_Temp21˚C -0.25862    0.10103   -2.56   0.0107 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 1.8039)

# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1271.7  on 705  degrees of freedom
# AIC: 2427.5

# Number of Fisher Scoring iterations: 2

#### m2: CI ~ SH_Tide ===============
m2 <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m2)
tab_model(m2, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = CI ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Csikamea_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -3.9779  -0.8385  -0.1552   0.6703   6.0266  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.11585    0.07024  58.600   <2e-16 ***
#  SH_TideTide  0.03583    0.10158   0.353    0.724    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.820347)
#
# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1283.3  on 705  degrees of freedom
# AIC: 2433.9
#
# Number of Fisher Scoring iterations: 2

#### m3: CI ~ MHW ===============
m3 <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m3)
tab_model(m3, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = CI ~ MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -4.0715  -0.8652  -0.1318   0.6746   6.0604  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.41207    0.09971  44.250  < 2e-16 ***
#  MHW18˚C     -0.33003    0.14121  -2.337   0.0197 *  
#  MHW21˚C     -0.20268    0.14141  -1.433   0.1522    
# MHW24˚C     -0.59364    0.14222  -4.174 3.37e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.779539)
#
# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1251.0  on 703  degrees of freedom
# AIC: 2419.9
#
# Number of Fisher Scoring iterations: 2


