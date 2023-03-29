#### ~ M. GIGAS MORTALITY STATS ~ =====

library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### ** SH Mortality ** ===============

## NA, no mortality

#### ** MHW Mortality ** ===============

### load data sheet
Mgigas_MHW_Morts <- read_csv("data/M_gigas/logit.mort_Mgigas_EndMHW.csv")
glimpse(Mgigas_MHW_Morts)
summary(Mgigas_MHW_Morts)
View(Mgigas_MHW_Morts)

### change attributes about statistical factors
Mgigas_MHW_Morts$SH_Temp <- as.factor(Mgigas_MHW_Morts$SH_Temp) ## factor
is.factor(Mgigas_MHW_Morts$SH_Temp) ## TRUE
Mgigas_MHW_Morts$SH_Tide <- as.factor(Mgigas_MHW_Morts$SH_Tide) ## factor
is.factor(Mgigas_MHW_Morts$SH_Tide) ## TRUE
Mgigas_MHW_Morts$MHW <- as.factor(Mgigas_MHW_Morts$MHW) ## character
is.factor(Mgigas_MHW_Morts$MHW) ## TRUE

#### m_null: Mortality ~ 1 ===============
m_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m_null)

# Call:
# glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = Mgigas_MHW_Morts)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-0.112  -0.112  -0.112  -0.112   3.186  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.0689     0.3172  -15.98   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 121.44  on 1599  degrees of freedom
#Residual deviance: 121.44  on 1599  degrees of freedom
#(1566 observations deleted due to missingness)
#AIC: 123.44
#
#Number of Fisher Scoring iterations: 8

#### m1: Mortality ~ SH_Temp ===============
m1 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m1)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = Mgigas_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1227  -0.1227  -0.1001  -0.1001   3.2552  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.2933     0.5013  -10.56   <2e-16 ***
#  SH_Temp21     0.4080     0.6474    0.63    0.529    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 121.44  on 1599  degrees of freedom
#Residual deviance: 121.04  on 1598  degrees of freedom
#(1566 observations deleted due to missingness)
#AIC: 125.04
#
#Number of Fisher Scoring iterations: 8
#

#### m1a: Mortality ~ SH_Temp + (1|Tank) ===============
m1a <- glmer(Mortality ~ SH_Temp + (1|Tank), family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m1a)

# Generalized linear mixed model fit by maximum likelihood
#(Laplace Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + (1 | Tank)
#Data: Mgigas_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#127.0    143.2    -60.5    121.0     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.0869 -0.0869 -0.0709 -0.0709 14.1067 
#
#Random effects:
#  Groups Name        Variance  Std.Dev. 
#Tank   (Intercept) 4.537e-13 6.735e-07
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.2933     0.5012  -10.56   <2e-16 ***
#  SH_Temp21     0.4080     0.6474    0.63    0.529    
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
##SH_Temp21 -0.774
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


#### m2: Mortality ~ SH_Tide ===============
m2 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m2)

# Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = Mgigas_MHW_Morts)
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


#### m2a: Mortality ~ SH_Tide + (1|Tank) ===============
m2a <- glmer(Mortality ~ SH_Tide + (1|Tank), family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m2a)

# Generalized linear mixed model fit by maximum likelihood
#(Laplace Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Tide + (1 | Tank)
#Data: Mgigas_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#127.4    143.6    -60.7    121.4     1597 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.0793 -0.0793 -0.0793 -0.0793 12.6095 
#
#Random effects:
#  Groups Name        Variance  Std.Dev. 
#Tank   (Intercept) 5.338e-13 7.306e-07
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.069e+00  4.486e-01   -11.3   <2e-16 ***
#  SH_TideTide  1.144e-14  6.344e-01     0.0        1    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#SH_TideTide -0.707
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#### m3: Mortality ~ MHW ===============
m3 <- glm(Mortality ~ MHW, family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m3)

#Call:
#  glm(formula = Mortality ~ MHW, family = binomial(link = "logit"), 
#      data = Mgigas_MHW_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.1418  -0.1001  -0.1001  -0.1001   3.2552  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.293e+00  7.089e-01  -7.467  8.2e-14 ***
#  MHW18        6.982e-01  8.689e-01   0.804    0.422    
#MHW21        2.812e-15  1.003e+00   0.000    1.000    
#MHW24        3.214e-15  1.003e+00   0.000    1.000    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 121.44  on 1599  degrees of freedom
#Residual deviance: 120.35  on 1596  degrees of freedom
#(1566 observations deleted due to missingness)
#AIC: 128.35
#
#Number of Fisher Scoring iterations: 8

#### m3a: Mortality ~ MHW + (1|Tank) ===============
m3a <- glmer(Mortality ~ MHW + (1|Tank), family = binomial(link = "logit"), data = Mgigas_MHW_Morts)
summary(m3a)

# Generalized linear mixed model fit by maximum likelihood
#(Laplace Approximation) [glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ MHW + (1 | Tank)
#Data: Mgigas_MHW_Morts
#
#AIC      BIC   logLik deviance df.resid 
#130.4    157.2    -60.2    120.4     1595 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.1005 -0.0709 -0.0709 -0.0709 14.1067 
#
#Random effects:
# Groups Name        Variance  Std.Dev. 
#Tank   (Intercept) 1.139e-12 1.067e-06
#Number of obs: 1600, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.293e+00  7.088e-01  -7.468 8.11e-14 ***
#  MHW18        6.982e-01  8.688e-01   0.804    0.422    
#MHW21       -2.806e-13  1.002e+00   0.000    1.000    
#MHW24       -2.132e-13  1.002e+00   0.000    1.000    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) MHW18  MHW21 
#MHW18 -0.816              
#MHW21 -0.707  0.577       
#MHW24 -0.707  0.577  0.500
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

ggsave(filename = "fig_output/model_Mgigas_Morts-MHW.png", width = 5.10, height = 5.77, dpi = 300)


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

ggsave(filename = "fig_output/DARKmodel_Mgigas_Morts-MHW.png", width = 5.10, height = 5.77, dpi = 300)
