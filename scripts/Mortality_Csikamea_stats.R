#### ~ C. SIKAMEA MORTALITY STATS ~ =====

library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### ** SH Mortality ** ===============

### load data sheet
Csikamea_SH_Morts <- read_csv("data/C_sikamea/logit.mort_Csikamea_EndSH.csv")
glimpse(Csikamea_SH_Morts)
summary(Csikamea_SH_Morts)
View(Csikamea_SH_Morts)

### change attributes about statistical factors
Csikamea_SH_Morts$SH_Temp <- as.factor(Csikamea_SH_Morts$SH_Temp) ## factor
is.factor(Csikamea_SH_Morts$SH_Temp) ## TRUE
Csikamea_SH_Morts$SH_Tide <- as.factor(Csikamea_SH_Morts$SH_Tide) ## factor
is.factor(Csikamea_SH_Morts$SH_Tide) ## TRUE

#### C. SIKAMEA STATS: Stress Hardening mortality ===============
#### Logistic Regression ===============
### https://www.tutorialspoint.com/r/r_logistic_regression.htm
### http://sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

#### m_null: Mortality ~ 1 ===============
m_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Csikamea_SH_Morts)
summary(m_null)

#Call:
#  glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#      data = Csikamea_SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1775  -0.1775  -0.1775  -0.1775   2.8840  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.1431     0.2016  -20.55   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 257.55  on 1599  degrees of freedom
#Residual deviance: 257.55  on 1599  degrees of freedom
#AIC: 259.55
#
#Number of Fisher Scoring iterations: 7

#### m1: Mortality ~ SH_Temp ===============
m1 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Csikamea_SH_Morts)
summary(m1)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = Csikamea_SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.2010  -0.2010  -0.1504  -0.1504   2.9958  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.4761     0.3352 -13.353   <2e-16 ***
#  SH_Temp21     0.5843     0.4197   1.392    0.164    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 257.55  on 1599  degrees of freedom
#Residual deviance: 255.53  on 1598  degrees of freedom
#AIC: 259.53
#
#Number of Fisher Scoring iterations: 7

#### m1a: Mortality ~ SH_Temp + (1|Tank) ===============
m1a <- glmer(Mortality ~ SH_Temp  + (1|Tank), family = binomial(link = "logit"), data = Csikamea_SH_Morts)
summary(m1a)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + (1 | Tank)
#Data: Csikamea_SH_Morts
#
#AIC      BIC   logLik deviance df.resid 
#261.5    277.7   -127.8    255.5     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1429 -0.1429 -0.1067 -0.1067  9.3749 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0        0       
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.4761     0.3352 -13.353   <2e-16 ***
#  SH_Temp21     0.5843     0.4197   1.392    0.164    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_Temp21 -0.799
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


#### m2: Mortality ~ SH_Tide ===============
m2 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Csikamea_SH_Morts)
summary(m2)

#Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = Csikamea_SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1810  -0.1810  -0.1739  -0.1739   2.8982  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -4.18459    0.29086 -14.387   <2e-16 ***
#  SH_TideTide  0.08131    0.40348   0.202     0.84    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 257.55  on 1599  degrees of freedom
#Residual deviance: 257.51  on 1598  degrees of freedom
#AIC: 261.51
#
#Number of Fisher Scoring iterations: 7

#### m2a: Mortality ~ SH_Tide + (1|Tank) ===============
m2a <- glmer(Mortality ~ SH_Tide  + (1|Tank), family = binomial(link = "logit"), data = Csikamea_SH_Morts)
summary(m2a)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Tide + (1 | Tank)
#Data: Csikamea_SH_Morts
#
#AIC      BIC   logLik deviance df.resid 
#263.5    279.6   -128.8    257.5     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1285 -0.1285 -0.1234 -0.1234  8.1035 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 0        0       
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -4.18459    0.29087 -14.387   <2e-16 ***
#  SH_TideTide  0.08131    0.40349   0.202     0.84    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_TideTide -0.721
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### ** MHW Mortality ** ===============

### load data sheet
Csikamea_MHW_Morts <- read_csv("data/C_sikamea/logit.mort_Csikamea_EndMHW.csv")
glimpse(Csikamea_MHW_Morts)
summary(Csikamea_MHW_Morts)
View(Csikamea_MHW_Morts)

### change attributes about statistical factors
Csikamea_MHW_Morts$SH_Temp <- as.factor(Csikamea_MHW_Morts$SH_Temp) ## factor
is.factor(Csikamea_MHW_Morts$SH_Temp) ## TRUE
Csikamea_MHW_Morts$SH_Tide <- as.factor(Csikamea_MHW_Morts$SH_Tide) ## factor
is.factor(Csikamea_MHW_Morts$SH_Tide) ## TRUE
Csikamea_MHW_Morts$MHW <- as.factor(Csikamea_MHW_Morts$MHW) ## character
is.factor(Csikamea_MHW_Morts$MHW) ## TRUE

#### m_null2: Mortality ~ 1 ===============
m_null2 <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Csikamea_MHW_Morts)
summary(m_null2)

# Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = Csikamea_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.2645  -0.2645  -0.2645  -0.2645   2.5963  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.3354     0.1372  -24.31   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 478.83  on 1599  degrees of freedom
#Residual deviance: 478.83  on 1599  degrees of freedom
#AIC: 480.83
#
#Number of Fisher Scoring iterations: 6

#### m3: Mortality ~ SH_Temp ===============
m3 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Csikamea_MHW_Morts)
summary(m3)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = Csikamea_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.2857  -0.2857  -0.2415  -0.2415   2.6643  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.1781     0.1804  -17.61   <2e-16 ***
#  SH_Temp21    -0.3419     0.2781   -1.23    0.219    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 478.83  on 1599  degrees of freedom
#Residual deviance: 477.30  on 1598  degrees of freedom
#AIC: 481.3
#
#Number of Fisher Scoring iterations: 6

#### m4: Mortality ~ SH_Tide ===============
m4 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Csikamea_MHW_Morts)
summary(m4)

# Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = Csikamea_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.2857  -0.2857  -0.2415  -0.2415   2.6643  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.5199     0.2116  -16.64   <2e-16 ***
#  SH_TideTide   0.3419     0.2781    1.23    0.219    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 478.83  on 1599  degrees of freedom
#Residual deviance: 477.30  on 1598  degrees of freedom
#AIC: 481.3
#
#Number of Fisher Scoring iterations: 6

#### m5: Mortality ~ MHW ===============
m5 <- glm(Mortality ~ MHW, family = binomial(link = "logit"), data = Csikamea_MHW_Morts)
summary(m5)

#Call:
#  glm(formula = Mortality ~ MHW, family = binomial(link = "logit"), 
#      data = Csikamea_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.3442  -0.2571  -0.2362  -0.2010   2.7971  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.5657     0.3057 -11.662   <2e-16 ***
#  MHW18         0.1722     0.4159   0.414   0.6788    
#MHW21        -0.3261     0.4701  -0.694   0.4879    
#MHW24         0.7689     0.3736   2.058   0.0396 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 478.83  on 1599  degrees of freedom
#Residual deviance: 469.87  on 1596  degrees of freedom
#AIC: 477.87

#Number of Fisher Scoring iterations: 6

#### m6: Mortality ~ MHW + (1|Tank) ===============
m6 <- glmer(Mortality ~ MHW + (1|Tank), family = binomial(link = "logit"), data = Csikamea_MHW_Morts)
summary(m6)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ MHW + (1 | Tank)
#Data: Csikamea_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#479.9    506.8   -234.9    469.9     1595 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.2470 -0.1833 -0.1682 -0.1429  7.0000 
#
#Random effects:
#  Groups Name        Variance Std.Dev.
#Tank   (Intercept) 4e-14    2e-07   
#Number of obs: 1600, groups:  Tank, 20

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.5657     0.3057 -11.663   <2e-16 ***
#  MHW18         0.1722     0.4159   0.414   0.6788    
#MHW21        -0.3261     0.4701  -0.694   0.4878    
#MHW24         0.7689     0.3736   2.058   0.0396 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) MHW18  MHW21 
#MHW18 -0.735              
#MHW21 -0.650  0.478       
#MHW24 -0.818  0.602  0.532
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### Pairwise Comparisons ===============
## pairwise comparison for m12
emm_m6 <-  emmeans(m6, specs = ~ MHW)
emm_m6
pairwise_m6 <- contrast(emm_m6, interaction = "pairwise")
pairwise_m6

#### AIC/BIC Scores ===============
AIC(m_null2, m3, m4, m5, m6)
BIC(m_null2, m3, m4, m5, m6)

#### Test Assumptions: m9 ===============
#???

#### Plot Model ========
tbl.m6 <- tbl_regression(m6, exponentiate = TRUE) ## table!
tbl.m6


library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

m6.Plot <- ggpredict(m6, terms = c("MHW"))
plot(m6.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("C. sikamea: glmer(Mortality ~ MHW + (1|Tank)")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Marine Heatwave Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/model_Csikamea_Morts-MHW.png", width = 5.10, height = 5.77, dpi = 300)


##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m6.DARKPlot <- ggpredict(m6, terms = c("MHW"))
plot(m6.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("C. sikamea: glmer(Mortality ~ MHW + (1|Tank)")), 
       subtitle = "Post-Outplanting Mortality",
       x = "Marine Heatwave Temp (°C)", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_Csikamea_Morts-MHW.png", width = 5.10, height = 5.77, dpi = 300)
