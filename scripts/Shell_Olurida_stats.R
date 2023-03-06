#### ~ O. LURIDA SHELL ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ##another model plotting option

### load data sheet
Olurida_Shell_stats <- read_csv("data/O_lurida/Olurida_CI_statsData.csv")
glimpse(Olurida_Shell_stats)
summary(Olurida_Shell_stats)
View(Olurida_Shell_stats)

### change attributes about statistical factors
Olurida_Shell_stats$SH_Temp <- as.factor(Olurida_Shell_stats$SH_Temp) ## factor
is.factor(Olurida_Shell_stats$SH_Temp) ## TRUE
Olurida_Shell_stats$SH_Tide <- as.factor(Olurida_Shell_stats$SH_Tide) ## factor
is.factor(Olurida_Shell_stats$SH_Tide) ## TRUE
Olurida_Shell_stats$MHW <- as.factor(Olurida_Shell_stats$MHW) ## character
is.factor(Olurida_Shell_stats$MHW) ## TRUE

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Shell.mg ~ 1 ===============
m_null <- glm(Shell.mg ~ 1, family = gaussian(link = "identity"), data = Olurida_Shell_stats)
summary(m_null)
tab_model(m_null)

# Call:
# glm(formula = Shell.mg ~ 1, family = gaussian(link = "identity"), 
#    data = Olurida_Shell_stats)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -11.169   -4.644   -1.069    3.806   34.831  

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  13.9695     0.2301   60.72   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 39.91118)

# Null deviance: 30053  on 753  degrees of freedom
# Residual deviance: 30053  on 753  degrees of freedom
# AIC: 4922.5

# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Shell.mg ~ SH_Temp ===============
m1 <- glm(Shell.mg ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_Shell_stats)
summary(m1)
tab_model(m1, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
#  glm(formula = Shell.mg ~ SH_Temp, family = gaussian(link = "identity"), 
#      data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -10.942   -4.683   -1.197    3.758   35.058  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  14.1971     0.3254  43.633   <2e-16 ***
#  SH_Temp21˚C  -0.4552     0.4601  -0.989    0.323    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 39.91232)
##
# Null deviance: 30053  on 753  degrees of freedom
# Residual deviance: 30014  on 752  degrees of freedom
# AIC: 4923.5

# Number of Fisher Scoring iterations: 2

#### m2: Shell.mg ~ SH_Tide ===============
m2 <- glm(Shell.mg ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_Shell_stats)
summary(m2)
tab_model(m2, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = Shell.mg ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -11.456   -4.637   -1.169    3.638   34.544  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  14.2556     0.3248  43.888   <2e-16 ***
#  SH_TideTide  -0.5736     0.4600  -1.247    0.213    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 39.88177)
#
# Null deviance: 30053  on 753  degrees of freedom
# Residual deviance: 29991  on 752  degrees of freedom
# AIC: 4922.9

# Number of Fisher Scoring iterations: 2


#### m3: Shell.mg ~ MHW ===============
m3 <- glm(Shell.mg ~ MHW, family = gaussian(link = "identity"), data = Olurida_Shell_stats)
summary(m3)
tab_model(m3, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = Shell.mg ~ MHW, family = gaussian(link = "identity"), 
#    data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -11.424   -4.753   -1.154    3.828   34.576  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  12.7544     0.4979  25.616   <2e-16 ***
#  MHW18˚C       1.4692     0.6688   2.197   0.0283 *  
#  MHW21˚C       1.6603     0.6703   2.477   0.0135 *  
#  MHW24˚C       1.4987     0.6695   2.238   0.0255 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 39.66541)
#
# Null deviance: 30053  on 753  degrees of freedom
# Residual deviance: 29749  on 750  degrees of freedom
# AIC: 4920.8
#
# Number of Fisher Scoring iterations: 2


#### Interaction ===============
## N/A ==

#### Random Factor: (1|Tank), lmer ===============
#### m4: Shell.mg ~ MHW + (1|Tank) ===============
m4 <- lmer(Shell.mg ~ MHW + (1|Tank), data = Olurida_Shell_stats)
summary(m4)
tab_model(m4, show.reflvl = TRUE, prefix.labels = "varname")
AIC(m4) ## 4921.375

# Linear mixed model fit by REML. t-tests use Satterthwaite's
# method [lmerModLmerTest]
# Formula: Shell.mg ~ MHW + (1 | Tank)
# Data: Olurida_Shell_stats
#
# REML criterion at convergence: 4909.4
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.8276 -0.7472 -0.1780  0.5946  5.4685 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept)  0.2275  0.477   
# Residual             39.4849  6.284   
# Number of obs: 754, groups:  Tank, 19
#
# Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  12.7544     0.5511 14.7832  23.145 5.05e-13 ***
#  MHW18˚C       1.4703     0.7400 14.8362   1.987   0.0657 .  
# MHW21˚C       1.6606     0.7414 14.9452   2.240   0.0407 *  
# MHW24˚C       1.4997     0.7407 14.8902   2.025   0.0612 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) MHW18˚ MHW21˚
# MHW18˚C -0.745              
# MHW21˚C -0.743  0.554       
# MHW24˚C -0.744  0.554  0.553

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


#### Gamma Distribution ========

#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null_Gamma: Shell.mg ~ 1 ===============
m_null_Gamma <- glm(Shell.mg ~ 1, family = Gamma(link = "identity"), data = Olurida_Shell_stats)
summary(m_null_Gamma)
tab_model(m_null_Gamma)

# Call:
# glm(formula = Shell.mg ~ 1, family = Gamma(link = "identity"), 
#    data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.27098  -0.37870  -0.07861   0.25099   1.57637  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  13.9695     0.2301   60.72   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.2045187)
#
# Null deviance: 148.31  on 753  degrees of freedom
# Residual deviance: 148.31  on 753  degrees of freedom
# AIC: 4770.5
#
# Number of Fisher Scoring iterations: 3
#
#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m5: Shell.mg ~ SH_Temp ===============
m5 <- glm(Shell.mg ~ SH_Temp, family = Gamma(link = "identity"), data = Olurida_Shell_stats)
summary(m5)
tab_model(m5, show.reflvl = TRUE, prefix.labels = "varname")

#Call:
# glm(formula = Shell.mg ~ SH_Temp, family = Gamma(link = "identity"), 
#    data = Olurida_Shell_stats)

# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.26062  -0.37655  -0.08681   0.25070   1.60244  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  14.1971     0.3311  42.879   <2e-16 ***
#  SH_Temp21˚C  -0.4552     0.4608  -0.988    0.324    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.2050477)
#
# Null deviance: 148.31  on 753  degrees of freedom
# Residual deviance: 148.11  on 752  degrees of freedom
# AIC: 4771.4
#
# Number of Fisher Scoring iterations: 3
#
#### m6: Shell.mg ~ SH_Tide ===============
m6 <- glm(Shell.mg ~ SH_Tide, family = Gamma(link = "identity"), data = Olurida_Shell_stats)
summary(m6)
tab_model(m6, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = Shell.mg ~ SH_Tide, family = Gamma(link = "identity"), 
#    data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.28370  -0.38144  -0.08618   0.23663   1.54444  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  14.2556     0.3314  43.021   <2e-16 ***
#  SH_TideTide  -0.5736     0.4599  -1.247    0.213    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.2042388)
#
# Null deviance: 148.31  on 753  degrees of freedom
# Residual deviance: 147.99  on 752  degrees of freedom
# AIC: 4770.8

# Number of Fisher Scoring iterations: 3


#### m7: Shell.mg ~ MHW ===============
m7 <- glm(Shell.mg ~ MHW, family = Gamma(link = "identity"), data = Olurida_Shell_stats)
summary(m7)
tab_model(m7, show.reflvl = TRUE, prefix.labels = "varname")

# Call:
# glm(formula = Shell.mg ~ MHW, family = Gamma(link = "identity"), 
#    data = Olurida_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.28229  -0.38578  -0.08676   0.24901   1.54796  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  12.7544     0.4541  28.086   <2e-16 ***
#  MHW18˚C       1.4692     0.6422   2.288   0.0224 *  
#  MHW21˚C       1.6603     0.6482   2.561   0.0106 *  
#  MHW24˚C       1.4987     0.6437   2.328   0.0202 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.2028369)
# 
# Null deviance: 148.31  on 753  degrees of freedom
# Residual deviance: 146.68  on 750  degrees of freedom
# AIC: 4767.9
# 
# Number of Fisher Scoring iterations: 3

#### Interactions ===============
## N?A ==

#### Random Factor: (1|Tank), lmer ===============
#### m8: Shell.mg ~ MHW + (1|Tank) ===============
m8 <- glmer(Shell.mg ~ MHW + (1|Tank), family = Gamma(link="identity"), data = Olurida_Shell_stats)
summary(m8)
tab_model(m8, show.reflvl = TRUE, prefix.labels = "varname")
AIC(m8) ## 4768.359
plot_model(m8, type = "eff", terms = "MHW") ## via sjplot

# Generalized linear mixed model fit by maximum likelihood
# (Laplace Approximation) [glmerMod]
# Family: Gamma  ( identity )
# Formula: Shell.mg ~ MHW + (1 | Tank)
# Data: Olurida_Shell_stats
#
# AIC      BIC   logLik deviance df.resid 
# 4768.9   4796.6  -2378.4   4756.9      748 
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.7976 -0.7367 -0.1711  0.5837  5.2698 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.4351   0.6596  
# Residual             0.2001   0.4474  
# Number of obs: 754, groups:  Tank, 19
#
# Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
# (Intercept)  12.7279     0.6363  20.002   <2e-16 ***
#  MHW18˚C       1.4656     0.8726   1.680   0.0930 .  
# MHW21˚C       1.6856     0.8761   1.924   0.0543 .  
# MHW24˚C       1.5244     0.8732   1.746   0.0809 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) MHW18˚ MHW21˚
# MHW18˚C -0.729              
# MHW21˚C -0.726  0.529       
# MHW24˚C -0.728  0.531  0.529

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8), resid(m8))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8))
qqline(resid(m8))

#### Density Plot of Residuals ===============
plot(density(resid(m8)))



#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m8.plot_byMHW <- ggpredict(m8, terms = "MHW")
plot(m8.plot_byMHW) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  #scale_color_manual(values = c("#4575B4", "#ABD9E9",  "#FDAE61", "#D73027")) +
    labs(title = expression(paste("glmer(Shell.mg ~ MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Shell(mg)")

ggsave(filename = "fig_output/model_Olurida_Shell-MHW_b&w.png",width = 5.10, height = 5.77, dpi = 300)

#library(RColorBrewer)
#brewer.pal(11, "RdYlBu")

####### With individual tanks plotted
Olurida_Shell_stats$fit <- predict(m8)

## By MHW
ggplot(Olurida_Shell_stats, aes(x = MHW, y = Shell.mg, col = MHW, group = Tank)) + 
  #facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  #geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(Shell.mg ~ MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Shell (mg)")

ggsave(filename = "fig_output/model_Olurida_Shell_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

