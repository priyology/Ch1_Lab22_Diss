#### ~ O. LURIDA MORTALITY DATA ~ =====

library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### O. LURIDA STATS: Stress Hardening mortality ===============

### load data sheet
SH_Morts <- read_csv("data/O_lurida/Olurida_SHmorts.csv")
glimpse(SH_Morts)
summary(SH_Morts)
View(SH_Morts)

### change attributes about statistical factors
SH_Morts$SH_Temp <- as.factor(SH_Morts$SH_Temp) ## factor
is.factor(SH_Morts$SH_Temp) ## TRUE
SH_Morts$SH_Tide <- as.factor(SH_Morts$SH_Tide) ## factor
is.factor(SH_Morts$SH_Tide) ## TRUE
#SH_Morts$MHW <- as.factor(SH_Morts$MHW) ## character
#is.factor(SH_Morts$MHW) ## TRUE

#### O. LURIDA STATS: Stress Hardening mortality ===============
#### Logistic Regression ===============









################ CODE GRAVEYARD ======================

#### Poisson Distribution ========

#### m_null: Mortality ~ 1 ===============
m_null <- glm(Mortality ~ 1, family = poisson, data = SH_Morts)
summary(m_null)

# Call:
#glm(formula = Mortality ~ 1, family = poisson, data = SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.5941  -0.5941  -0.5941  -0.5941   3.3693  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.7346     0.2357  -7.359 1.85e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 71.81  on 101  degrees of freedom
#Residual deviance: 71.81  on 101  degrees of freedom
#AIC: 105.42
#
#Number of Fisher Scoring iterations: 6


#### m1: Mortality ~ SH_Temp ===============
m1 <- glm(Mortality ~ SH_Temp, family = poisson, data = SH_Morts)
summary(m1)

# Call:
# glm(formula = Mortality ~ SH_Temp, family = poisson, data = SH_Morts)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.6794  -0.6794  -0.4899  -0.4899   3.1387  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.1203     0.4082  -5.194 2.06e-07 ***
#  SH_Temp21     0.6539     0.5000   1.308    0.191    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
# Null deviance: 71.81  on 101  degrees of freedom
# Residual deviance: 70.00  on 100  degrees of freedom
#AIC: 105.61
#
#Number of Fisher Scoring iterations: 6

#### m2: Mortality ~ SH_Tide ===============
m2 <- glm(Mortality ~ SH_Tide, family = poisson, data = SH_Morts)
summary(m2)

#Call:
#glm(formula = Mortality ~ SH_Tide, family = poisson, data = SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6928  -0.6928  -0.4804  -0.4804   3.1039  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.1595     0.4082  -5.290 1.23e-07 ***
#  SH_TideTide   0.7324     0.5000   1.465    0.143    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 71.810  on 101  degrees of freedom
#Residual deviance: 69.529  on 100  degrees of freedom
#AIC: 105.13
#
#Number of Fisher Scoring iterations: 6

#### Interaction ===============
#### N/A

#### Random Factor: (1|Tank), lmer ===============
#### m3: Mortality ~ SH_Temp + SH_Tide + (1|Tank)

m3 <- glm(Mortality ~ SH_Temp + SH_Tide + (1|Tank), family = poisson, data = SH_Morts)
summary(m3)

# Call:
# glm(formula = Mortality ~ SH_Temp + SH_Tide + (1 | Tank), family = poisson, 
#    data = SH_Morts)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7966  -0.5705  -0.5490  -0.3931   2.8485  
#
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -2.5604     0.5307  -4.825  1.4e-06 ***
#  SH_Temp21      0.6677     0.5001   1.335    0.182    
#SH_TideTide    0.7447     0.5001   1.489    0.136    
#1 | TankTRUE       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 71.81  on 101  degrees of freedom
#Residual deviance: 67.64  on  99  degrees of freedom
#AIC: 105.25
#
#Number of Fisher Scoring iterations: 6

#### Test Assumptions ===============
#### Chi-square test (p < 0.05) ===============
## https://sscc.wisc.edu/sscc/pubs/glm-r/

pchisq(deviance(m3), df.residual(m3))
## p = 0.0067 <-- overdispersion not by chace

drop1(m3, test = "F")

# Single term deletions
#
#Model:
#  Mortality ~ SH_Temp + SH_Tide + (1 | Tank)
#Df Deviance    AIC F value  Pr(>F)  
#<none>        67.640 105.25                  
#SH_Temp   1   69.529 105.13  2.7641 0.09957 .
#SH_Tide   1   70.000 105.61  3.4529 0.06611 .
#1 | Tank  0   67.640 105.25                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### both SH_Temp & SH_Tide are weakly significant and *could* be retained in the model!

#### m4: m3 w/ quasipoission distribution ===============

m4 <- glm(Mortality ~ SH_Temp + SH_Tide + (1|Tank), family = "quasipoisson", data = SH_Morts)
summary(m4)

#### check if variance of residuals is proportional to the mean ===============

m3Diag <- data.frame(SH_Morts,
                     link = predict(m3, type = "link"),
                     fit = predict(m3, type = "response"),
                     pearson = residuals(m3, type = "pearson"),
                     resid = residuals(m3, type = "response"),
                     residSqr = residuals(m3, type = "response")^2
)

ggplot(m3Diag, aes(x = fit, y = residSqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_abline(intercept = 0, slope = summary(m4)$dispersion,
              color = "darkgreen", linetype = 2, linewidth = 1) +
  geom_smooth(se = F, size = 1) +
  theme_bw() ### yellow line is somewhat straight; but this may not be proportional to mean


#### m5: m3 w/ negative non-binomial distribution ===============
library(MASS)
m5 <- glm.nb(Mortality ~ SH_Temp + SH_Tide + (1|Tank), data = SH_Morts)
summary(m5)

# Call:
#glm.nb(formula = Mortality ~ SH_Temp + SH_Tide + (1 | Tank), 
#       data = SH_Morts, init.theta = 1.099396629, link = log)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7405  -0.5522  -0.5320  -0.3914   2.2016  
#
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -2.5339     0.5564  -4.554 5.26e-06 ***
#  SH_Temp21      0.6437     0.5373   1.198    0.231    
#SH_TideTide    0.7233     0.5368   1.348    0.178    
#1 | TankTRUE       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Negative Binomial(1.0994) family taken to be 1)
#
#Null deviance: 58.031  on 101  degrees of freedom
#Residual deviance: 54.566  on  99  degrees of freedom
#AIC: 106.04
#
#Number of Fisher Scoring iterations: 1
#
#
#Theta:  1.10 
#Std. Err.:  1.36 
#
#2 x log-likelihood:  -98.036 

drop1(m5, test = "LRT")
# Single term deletions
#
#Model:
#  Mortality ~ SH_Temp + SH_Tide + (1 | Tank)
#Df Deviance    AIC    LRT Pr(>Chi)
#<none>        54.566 104.04                
#SH_Temp   1   56.060 103.53 1.4940   0.2216
#SH_Tide   1   56.466 103.94 1.8998   0.1681
#1 | Tank  0   54.566 104.04 0.0000        

### no terms are significant and so don't need to be included in the model



#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.m3 <- tbl_regression(m3, exponentiate = TRUE) ## table!
tbl.m3
inline_text(tbl.m3,  variable = SH_Temp, level = "21") ##in-line text
# "1.95 (95% CI 0.76, 5.60; p=0.2)"

m5Diag <- data.frame(SH_Morts,
                     link = predict(m5, type = "link"),
                     fit = predict(m5, type = "response"),
                     pearson = residuals(m5, type = "pearson"),
                     resid = residuals(m5, type = "response"),
                     residSqr = residuals(m5, type = "response")^2
)

ggplot(m5Diag, aes(x = fit, y = residSqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_abline(intercept = 0, slope = summary(m4)$dispersion,
              color = "darkgreen", linetype = 2, linewidth = 1) +
  stat_function(fun = function(fit) {fit + fit^2 / 11.53}, 
               color = "red", size = 1) +
  geom_smooth(se = F, size = 1) +
  theme_bw() ### yellow line is somewhat straight; but this may not be proportional to mean

 
#### predicted probabilities of 0 through 7 when the mean is predicted to be 4 for m3 v m5  ====

data.frame(number=0:8,
           prob_Poisson = round(dpois(0:8, 
                                      (4*summary(m3)$dispersion)), 
                                3),
           prob_NBinom = round(dnbinom(0:8, 
                                       mu=4, 
                                       size=summary(m5)$theta), 
                               3)
)

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5)
BIC(m_null, m1, m2, m3, m4, m5)

#### O. LURIDA STATS: End MHW mortality ===============







#### O. LURIDA STATS: Outplanting (12/20/2022) ===============


##################