#### ~ O. LURIDA TISSUE ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ##another model plotting option

### load data sheet
Olurida_Tissue_stats <- read_csv("data/O_lurida/Olurida_CI_statsData.csv")
glimpse(Olurida_Tissue_stats)
summary(Olurida_Tissue_stats)
View(Olurida_Tissue_stats)

### change attributes about statistical factors
Olurida_Tissue_stats$SH_Temp <- as.factor(Olurida_Tissue_stats$SH_Temp) ## factor
is.factor(Olurida_Tissue_stats$SH_Temp) ## TRUE
Olurida_Tissue_stats$SH_Tide <- as.factor(Olurida_Tissue_stats$SH_Tide) ## factor
is.factor(Olurida_Tissue_stats$SH_Tide) ## TRUE
Olurida_Tissue_stats$MHW <- as.factor(Olurida_Tissue_stats$MHW) ## character
is.factor(Olurida_Tissue_stats$MHW) ## TRUE

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Tissue.mg ~ 1 ===============
m_null <- glm(Tissue.mg ~ 1, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
summary(m_null)
tab_model(m_null)

#Call:
# glm(formula = Tissue.mg ~ 1, family = gaussian(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.8804  -0.3804  -0.0804   0.1196   5.6196  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.98037    0.02417   40.55   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.4406501)
#
# Null deviance: 331.81  on 753  degrees of freedom
# Residual deviance: 331.81  on 753  degrees of freedom
# AIC: 1524.9

# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Tissue.mg ~ SH_Temp ===============
m1 <- glm(Tissue.mg ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
summary(m1)
tab_model(m1)

# Call:
# glm(formula = Tissue.mg ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.9223  -0.3385  -0.1223   0.1615   5.5777  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.02228    0.03414  29.942   <2e-16 ***
#  SH_Temp21˚C -0.08382    0.04828  -1.736    0.083 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.4394749)
#
# Null deviance: 331.81  on 753  degrees of freedom
# Residual deviance: 330.49  on 752  degrees of freedom
# AIC: 1523.8
# 
# Number of Fisher Scoring iterations: 2

#### m2: Tissue.mg ~ SH_Tide ===============
m2 <- glm(Tissue.mg ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
summary(m2)
tab_model(m2)

# Call:
# glm(formula = Tissue.mg ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.8968  -0.3638  -0.0968   0.1362   5.6362  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.99683    0.03416  29.185   <2e-16 ***
#  SH_TideTide -0.03300    0.04837  -0.682    0.495    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.4409631)
#
# Null deviance: 331.81  on 753  degrees of freedom
# Residual deviance: 331.60  on 752  degrees of freedom
# AIC: 1526.4
#
# Number of Fisher Scoring iterations: 2


#### m3: Tissue.mg ~ MHW ===============
 m3 <- glm(Tissue.mg ~ MHW, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
 summary(m3)
 tab_model(m3)

# Call:
# glm(formula = Tissue.mg ~ MHW, family = gaussian(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.9779  -0.3606  -0.1411   0.1589   5.5221  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.76063    0.05175  14.698  < 2e-16 ***
#  MHW18˚C      0.31726    0.06951   4.564 5.85e-06 ***
#  MHW21˚C      0.28049    0.06967   4.026 6.25e-05 ***
#  MHW24˚C      0.23887    0.06959   3.433 0.000631 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.4285221)
# 
# Null deviance: 331.81  on 753  degrees of freedom
# Residual deviance: 321.39  on 750  degrees of freedom
# AIC: 1506.8
#
# Number of Fisher Scoring iterations: 2

 #### m4: Tissue.mg ~ SH_Temp + MHW ===============
 m4 <- glm(Tissue.mg ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
 summary(m4)
 tab_model(m4)

# Call:
# glm(formula = Tissue.mg ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#     data = Olurida_Tissue_stats)
 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0195  -0.3576  -0.1277   0.1642   5.4805  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.80249    0.05690  14.104  < 2e-16 ***
#   SH_Temp21˚C -0.08372    0.04761  -1.758  0.07909 .  
# MHW18˚C      0.31705    0.06941   4.568 5.77e-06 ***
#   MHW21˚C      0.28070    0.06957   4.035 6.03e-05 ***
#   MHW24˚C      0.23887    0.06949   3.437  0.00062 ***
#   ---
 #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
 # (Dispersion parameter for gaussian family taken to be 0.4273301)
 # 
 # Null deviance: 331.81  on 753  degrees of freedom
 # deviance: 320.07  on 749  degrees of freedom
 # AIC: 1505.7
 #
 # Number of Fisher Scoring iterations: 2
 
#### Interaction: Tissue.mg ~ SH_Temp + MHW + SH_Temp*MHW ===============
 m5 <- glm(Tissue.mg ~ SH_Temp + MHW + SH_Temp*MHW, family = gaussian(link = "identity"), data = Olurida_Tissue_stats)
 summary(m5)
 tab_model(m5)
 
 # Call:
 # glm(formula = Tissue.mg ~ SH_Temp + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
  #     data = Olurida_Tissue_stats)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0120  -0.3495  -0.1348   0.1566   5.4880  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.82625    0.07321  11.286  < 2e-16 ***
#   SH_Temp21˚C         -0.13125    0.10354  -1.268  0.20531    
# MHW18˚C              0.28575    0.09822   2.909  0.00373 ** 
#   MHW21˚C              0.23702    0.09867   2.402  0.01654 *  
#   MHW24˚C              0.22324    0.09844   2.268  0.02363 *  
#   SH_Temp21˚C:MHW18˚C  0.06268    0.13906   0.451  0.65230    
# SH_Temp21˚C:MHW21˚C  0.08718    0.13938   0.625  0.53186    
# SH_Temp21˚C:MHW24˚C  0.03125    0.13922   0.224  0.82246    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.4287908)
# 
# Null deviance: 331.81  on 753  degrees of freedom
# Residual deviance: 319.88  on 746  degrees of freedom
# AIC: 1511.2
#
# Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m6: Tissue.mg ~ MHW + (1|Tank) ===============
m6 <- lmer(Tissue.mg ~ SH_Temp + MHW + (1|Tank), data = Olurida_Tissue_stats)
summary(m6)
tab_model(m6)
AIC(m6) ## 1509.262

# Linear mixed model fit by REML. t-tests use Satterthwaite's
# method [lmerModLmerTest]
# Formula: Tissue.mg ~ SH_Temp + MHW + (1 | Tank)
# Data: Olurida_Tissue_stats
# 
# REML criterion at convergence: 1495.3
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.8514 -0.5573 -0.2174  0.2783  8.5010 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.02749  0.1658  
# Residual             0.40550  0.6368  
# Number of obs: 754, groups:  Tank, 19
#
# Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.80236    0.09972  16.68070   8.046 3.85e-07 ***
#  SH_Temp21˚C  -0.08347    0.04638 734.06838  -1.800   0.0723 .  
# MHW18˚C       0.31615    0.13017  14.94425   2.429   0.0282 *  
#  MHW21˚C       0.28100    0.13024  14.98086   2.157   0.0476 *  
#  MHW24˚C       0.24037    0.13021  14.96248   1.846   0.0848 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_T21 MHW18˚ MHW21˚
# SH_Temp21˚C -0.233                     
# MHW18˚C     -0.725  0.001              
# MHW21˚C     -0.724 -0.001  0.555       
# MHW24˚C     -0.724  0.000  0.555  0.555

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6)
BIC(m_null, m1, m2, m3, m4, m5, m6)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m6), resid(m6))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m6))
qqline(resid(m6))

#### Density Plot of Residuals ===============
plot(density(resid(m6)))


#### Gamma Distribution ========

#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null_Gamma: Tissue.mg ~ 1 ===============
m_null_Gamma <- glm(Tissue.mg ~ 1, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m_null_Gamma)
tab_model(m_null_Gamma)

# Call:
# glm(formula = Tissue.mg ~ 1, family = Gamma(link = "identity"), 
#   data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -1.66419  -0.45390  -0.08433   0.11739   2.76595  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.98037    0.02417   40.55   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.4584718)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 242.17  on 753  degrees of freedom
# AIC: 1055.6
# 
# Number of Fisher Scoring iterations: 3

#
#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m5: Tissue.mg ~ SH_Temp ===============
m5 <- glm(Tissue.mg ~ SH_Temp, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m5)
tab_model(m5)

# Call:
# glm(formula = Tissue.mg ~ SH_Temp, family = Gamma(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.6867  -0.4163  -0.1247   0.1631   2.6800  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.02228    0.03578  28.568   <2e-16 ***
#  SH_Temp21˚C -0.08382    0.04858  -1.726   0.0848 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.4619315)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 240.79  on 752  degrees of freedom
# AIC: 1053
#
# Number of Fisher Scoring iterations: 3


#### m6: Tissue.mg ~ SH_Tide ===============
m6 <- glm(Tissue.mg ~ SH_Tide, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m6)
tab_model(m6)

# Call:
# glm(formula = Tissue.mg ~ SH_Tide, family = Gamma(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.6732  -0.4393  -0.1005   0.1351   2.8013  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.99683    0.03473  28.698   <2e-16 ***
#  SH_TideTide -0.03300    0.04838  -0.682    0.495    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.4589607)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 241.95  on 752  degrees of freedom
# AIC: 1056.9
#
# Number of Fisher Scoring iterations: 3

#### m7: Tissue.mg ~ MHW ===============
m7 <- glm(Tissue.mg ~ MHW, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m7)
tab_model(m7)

# Call:
# glm(formula = Tissue.mg ~ MHW, family = Gamma(link = "identity"), 
#    data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.7149  -0.4027  -0.1422   0.1731   2.5733  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.76063    0.03904  19.484  < 2e-16 ***
#  MHW18˚C      0.31726    0.06313   5.026 6.27e-07 ***
#  MHW21˚C      0.28049    0.06199   4.525 7.03e-06 ***
#  MHW24˚C      0.23887    0.06042   3.953 8.43e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.4214855)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 230.03  on 750  degrees of freedom
# AIC: 1020.8
#
# Number of Fisher Scoring iterations: 3

#### m8: Tissue.mg ~ SH_Temp + MHW ===============
m8 <- glm(Tissue.mg ~ SH_Temp + MHW, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m8)
tab_model(m8)

# Call:
# glm(formula = Tissue.mg ~ SH_Temp + MHW, family = Gamma(link = "identity"), 
#    data = Olurida_Tissue_stats)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.7372  -0.4298  -0.1457   0.1486   2.6286  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.80413    0.04638  17.338  < 2e-16 ***
#  SH_Temp21˚C -0.09177    0.04530  -2.026   0.0431 *  
#  MHW18˚C      0.32040    0.06304   5.082 4.72e-07 ***
#  MHW21˚C      0.28520    0.06194   4.605 4.85e-06 ***
#  MHW24˚C      0.24087    0.06026   3.997 7.05e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.4240136)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 228.29  on 749  degrees of freedom
# AIC: 1016.8
# 
# Number of Fisher Scoring iterations: 5

#### Interactions: Tissue.mg ~ SH_Temp + MHW + SH_Temp*MHW ===============
m9 <- glm(Tissue.mg ~ SH_Temp + MHW + MHW + SH_Temp*MHW, family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
summary(m9)
tab_model(m9)

# Call:
# glm(formula = Tissue.mg ~ SH_Temp + MHW + MHW + SH_Temp * MHW, 
#    family = Gamma(link = "identity"), data = Olurida_Tissue_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.7313  -0.4264  -0.1498   0.1508   2.6365  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.82625    0.06025  13.714  < 2e-16 ***
#  SH_Temp21˚C         -0.13125    0.07873  -1.667  0.09590 .  
# MHW18˚C              0.28575    0.09428   3.031  0.00252 ** 
#  MHW21˚C              0.23702    0.09239   2.565  0.01050 *  
#  MHW24˚C              0.22324    0.09144   2.441  0.01486 *  
#  SH_Temp21˚C:MHW18˚C  0.06268    0.12702   0.493  0.62182    
# SH_Temp21˚C:MHW21˚C  0.08718    0.12477   0.699  0.48496    
# SH_Temp21˚C:MHW24˚C  0.03125    0.12167   0.257  0.79737    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.4253467)
#
# Null deviance: 242.17  on 753  degrees of freedom
# Residual deviance: 228.05  on 746  degrees of freedom
# AIC: 1021.9
#
# Number of Fisher Scoring iterations: 3

#### Random Factor: (1|Tank), lmer ===============
#### m10: Tissue.mg ~ SH_Temp + MHW + (1|Tank) ===============
m10 <- glmer(Tissue.mg ~ SH_Temp + MHW + (1|Tank), family = Gamma(link="identity"), data = Olurida_Tissue_stats)
summary(m10)
tab_model(m10)
AIC(m10) ## 986.5
plot_model(m10, type = "eff", terms = "MHW") ## via sjplot

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

m8.plot_byMHW <- ggpredict(m8, terms = c("MHW", "SH_Temp"))
plot(m8.plot_byMHW) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("glmer(Tissue ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Olurida_Tissue-MHW.png",width = 5.10, height = 5.77, dpi = 300)

m8.plot_bySHtemp <- ggpredict(m8, terms = c("SH_Temp", "MHW"))
plot(m8.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(Tissue ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Olurida_Tissue-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)


####### With individual tanks plotted
Olurida_Tissue_stats$fit <- predict(m8)

## By SH_Temp
ggplot(Olurida_Tissue_stats, aes(x = SH_Temp, y = Tissue.mg, group = interaction(Tank, SH_Temp), col = MHW)) +  #, shape = MHW )) + 
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(Tissue ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Tissue (mg)")

ggsave(filename = "fig_output/model_Olurida_Tissue-SH_Temp_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Olurida_Tissue_stats, aes(x = SH_Temp, y = Tissue.mg, group = interaction(Tank, SH_Temp), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("glmer(Tissue ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank)")), 
       subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Tissue (mg)")

ggsave(filename = "fig_output/model_Olurida_Tissue_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)


