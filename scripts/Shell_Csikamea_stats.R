#### ~ C. SIKAMEA SHELL ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ##another model plotting option

### load data sheet
Csikamea_Shell_stats <- read_csv("data/C_sikamea/Csikamea_CI_statsData.csv")
glimpse(Csikamea_Shell_stats)
summary(Csikamea_Shell_stats)
View(Csikamea_Shell_stats)

### change attributes about statistical factors
Csikamea_Shell_stats$SH_Temp <- as.factor(Csikamea_Shell_stats$SH_Temp) ## factor
is.factor(Csikamea_Shell_stats$SH_Temp) ## TRUE
Csikamea_Shell_stats$SH_Tide <- as.factor(Csikamea_Shell_stats$SH_Tide) ## factor
is.factor(Csikamea_Shell_stats$SH_Tide) ## TRUE
Csikamea_Shell_stats$MHW <- as.factor(Csikamea_Shell_stats$MHW) ## character
is.factor(Csikamea_Shell_stats$MHW) ## TRUE

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Shell.g ~ 1 ===============
m_null <- glm(Shell.g ~ 1, family = gaussian(link = "identity"), data = Csikamea_Shell_stats)
summary(m_null)
tab_model(m_null)

# Call:
# glm(formula = Shell.g ~ 1, family = gaussian(link = "identity"), 
#    data = Csikamea_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.31313  -0.35848  -0.03143   0.34452   2.27647  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.30853    0.02001   115.4   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.2830586)
#
# Null deviance: 199.84  on 706  degrees of freedom
# Residual deviance: 199.84  on 706  degrees of freedom
# AIC: 1117.1
#
# Number of Fisher Scoring iterations: 2
#

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Shell.g ~ SH_Temp ===============
m1 <- glm(Shell.g ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_Shell_stats)
summary(m1)
tab_model(m1, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
#  glm(formula = Shell.g ~ SH_Temp, family = gaussian(link = "identity"), 
#      data = Csikamea_Shell_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.3145  -0.3572  -0.0301   0.3432   2.2778  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.307197   0.028377  81.304   <2e-16 ***
#  SH_Temp21˚C 0.002662   0.040047   0.066    0.947    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.2834584)
#
# Null deviance: 199.84  on 706  degrees of freedom
# Residual deviance: 199.84  on 705  degrees of freedom
# AIC: 1119.1
#
# Number of Fisher Scoring iterations: 2

#### m2: Shell.g ~ SH_Tide ===============
m2 <- glm(Shell.g ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_Shell_stats)
summary(m2)
tab_model(m2, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
#  glm(formula = Shell.g ~ SH_Tide, family = gaussian(link = "identity"), 
#     data = Csikamea_Shell_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.32584  -0.36452  -0.03854   0.34901   2.28811  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.29689    0.02771  82.894   <2e-16 ***
#  SH_TideTide  0.02435    0.04007   0.608    0.544    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.2833117)
#
# Null deviance: 199.84  on 706  degrees of freedom
# Residual deviance: 199.73  on 705  degrees of freedom
# AIC: 1118.7
#
# Number of Fisher Scoring iterations: 2

#### m3: Shell.g ~ MHW ===============
m3 <- glm(Shell.g ~ MHW, family = gaussian(link = "identity"), data = Csikamea_Shell_stats)
summary(m3)
tab_model(m3, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
#  glm(formula = Shell.g ~ MHW, family = gaussian(link = "identity"), 
#     data = Csikamea_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.32896  -0.36575  -0.02944   0.33937   2.28462  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.30038    0.03983  57.751   <2e-16 ***
#  MHW18˚C     -0.01347    0.05641  -0.239    0.811    
# MHW21˚C      0.01916    0.05649   0.339    0.735    
# MHW24˚C      0.02758    0.05682   0.485    0.628    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 0.2840079)
#
# Null deviance: 199.84  on 706  degrees of freedom
# Residual deviance: 199.66  on 703  degrees of freedom
# AIC: 1122.4

# Number of Fisher Scoring iterations: 2


#### Interaction ===============
## N/A ==

#### Random Factor: (1|Tank), lmer ===============
#### m4: Shell.g ~ (1|Tank) ===============
m4 <- lmer(Shell.g ~ SH_Temp + SH_Tide + MHW + (1|Tank), data = Csikamea_Shell_stats)
summary(m4)
tab_model(m4, show.reflvl = TRUE, prefix.labels = "varname")
AIC(m4) ## 1155.64

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: Shell.g ~ SH_Temp + SH_Tide + MHW + (1 | Tank)
# Data: Csikamea_Shell_stats
#
# REML criterion at convergence: 1139.6
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.4722 -0.6870 -0.0525  0.6476  4.3060 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.0000   0.0000  
# Residual             0.2847   0.5335  
# Number of obs: 707, groups:  Tank, 20
#
# Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   2.287560   0.048446 701.000000  47.219   <2e-16 ***
#  SH_Temp21˚C   0.002637   0.040139 701.000000   0.066    0.948    
# SH_TideTide   0.024240   0.040172 701.000000   0.603    0.546    
# MHW18˚C      -0.013573   0.056479 701.000000  -0.240    0.810    
# MHW21˚C       0.019042   0.056557 701.000000   0.337    0.736    
# MHW24˚C       0.027430   0.056886 701.000000   0.482    0.630    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_T21 SH_TdT MHW18˚ MHW21˚
# SH_Temp21˚C -0.409                            
# SH_TideTide -0.391 -0.007                     
# MHW18˚C     -0.576 -0.010 -0.002              
# MHW21˚C     -0.581  0.004 -0.004  0.498       
# MHW24˚C     -0.572 -0.008 -0.003  0.495  0.494
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4)
BIC(m_null, m1, m2, m3, m4)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m4), resid(m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m4))
qqline(resid(m4))

#### Density Plot of Residuals ===============
plot(density(resid(m4)))





#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m4.plot_byMHW <- ggpredict(m4, terms = "MHW")
plot(m4.plot_byMHW) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  #scale_color_manual(values = c("#4575B4", "#ABD9E9",  "#FDAE61", "#D73027")) +
  labs(title = expression(paste("glmer(Shell.g ~ MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Shell(g)")

ggsave(filename = "fig_output/model_Csikamea_Shell-MHW_b&w.png",width = 5.10, height = 5.77, dpi = 300)

#library(RColorBrewer)
#brewer.pal(11, "RdYlBu")

####### With individual tanks plotted
Csikamea_Shell_stats$fit <- predict(m4)

## By MHW
ggplot(Csikamea_Shell_stats, aes(x = MHW, y = Shell.g, col = MHW, group = Tank)) + 
  #facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  #geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(Shell.g ~ MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Shell (g)")

ggsave(filename = "fig_output/model_Csikamea_Shell_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)