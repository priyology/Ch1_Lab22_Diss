#### ~ C. SIKAMEA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
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

#### m4: CI ~ SH_Temp + MHW ===============
m4 <- glm(CI ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m4)

# Call:
#  glm(formula = CI ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#      data = Csikamea_CI_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.9423  -0.8284  -0.1256   0.6339   6.1845  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.53837    0.11107  40.862  < 2e-16 ***
#  SH_Temp21˚C -0.25400    0.09997  -2.541   0.0113 *  
#  MHW18˚C     -0.32647    0.14067  -2.321   0.0206 *  
#  MHW21˚C     -0.20412    0.14086  -1.449   0.1478    
#MHW24˚C     -0.59073    0.14168  -4.169 3.44e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.765835)
#
# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1239.6  on 702  degrees of freedom
# AIC: 2415.4
#
# Number of Fisher Scoring iterations: 2

#### Interaction ===============
#### m5: CI ~ SH_Temp + MHW + SH_Temp*MHW ===============
m5 <- glm(CI ~ SH_Temp + MHW + SH_Temp*MHW , family = gaussian(link = "identity"), data = Csikamea_CI_stats)
summary(m5)

# Call:
#  glm(formula = CI ~ SH_Temp + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
#      data = Csikamea_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -3.8691  -0.8273  -0.1409   0.6356   6.1445  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.59752    0.14018  32.796  < 2e-16 ***
#  SH_Temp21˚C         -0.37297    0.19881  -1.876 0.061061 .  
# MHW18˚C             -0.42744    0.19995  -2.138 0.032884 *  
#  MHW21˚C             -0.19252    0.19825  -0.971 0.331843    
# MHW24˚C             -0.74463    0.20114  -3.702 0.000231 ***
#  SH_Temp21˚C:MHW18˚C  0.20078    0.28158   0.713 0.476055    
# SH_Temp21˚C:MHW21˚C -0.02499    0.28196  -0.089 0.929405    
# SH_Temp21˚C:MHW24˚C  0.30524    0.28360   1.076 0.282165    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.768624)
#
# Null deviance: 1283.6  on 706  degrees of freedom
# Residual deviance: 1236.3  on 699  degrees of freedom
# AIC: 2419.5
# 
# Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m6: CI ~ SH_Temp + MHW + (1|Tank) ===============
m6 <- lmer(CI ~ SH_Temp + MHW + (1|Tank), data = Csikamea_CI_stats)
summary(m6)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: CI ~ SH_Temp + MHW + (1 | Tank)
# Data: Csikamea_CI_stats
#
# REML criterion at convergence: 2417.2
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.9667 -0.6234 -0.0945  0.4770  4.6540 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.000    0.000   
# Residual             1.766    1.329   
# Number of obs: 707, groups:  Tank, 20
#
# Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   4.53837    0.11107 702.00000  40.862  < 2e-16 ***
#  SH_Temp21˚C  -0.25400    0.09997 702.00000  -2.541   0.0113 *  
#  MHW18˚C      -0.32647    0.14067 702.00000  -2.321   0.0206 *  
#  MHW21˚C      -0.20412    0.14086 702.00000  -1.449   0.1478    
# MHW24˚C      -0.59073    0.14168 702.00000  -4.169 3.44e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_T21 MHW18˚ MHW21˚
# SH_Temp21˚C -0.448                     
# MHW18˚C     -0.627 -0.010              
# MHW21˚C     -0.632  0.004  0.498       
# MHW24˚C     -0.623 -0.008  0.495  0.494
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

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

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.m6 <- tbl_regression(m6, exponentiate = TRUE) ## table!
tbl.m6
inline_text(tbl.m6,  variable = SH_Temp, level = "21˚C") ##in-line text
# "0.78 (95% CI 0.64, 0.94; p=0.011)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m6.plot_byMHW <- ggpredict(m6, terms = c("MHW", "SH_Temp"))
plot(m6.plot_byMHW) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Csikamea_CI-MHW.png",width = 5.10, height = 5.77, dpi = 300)

m6.plot_bySHtemp <- ggpredict(m6, terms = c("SH_Temp", "MHW"))
plot(m6.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Csikamea_CI-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m6.DARKplot_byMHW <- ggpredict(m6, terms = c("MHW", "SH_Temp"))
plot(m6.plot_byMHW) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/DARKmodel_Csikamea_CI-MHW.png",width = 5.10, height = 5.77, dpi = 300)

m6.DARKplot_bySHtemp <- ggpredict(m6, terms = c("SH_Temp", "MHW"))
plot(m6.plot_bySHtemp) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/DARKmodel_Csikamea_CI-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)


####### With individual tanks plotted
Csikamea_CI_stats$fit <- predict(m6)

## By CI ~ SH_Temp
ggplot(Csikamea_CI_stats, aes(x = SH_Temp, y = CI, group = interaction(Tank, SH_Temp), col = MHW)) +  #, shape = MHW )) + 
  #facet_grid(~ SH_Tide) +
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Csikamea_CI-SH_Temp_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Csikamea_CI_stats, aes(x = SH_Temp, y = CI, group = interaction(Tank, SH_Temp), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("C. sikamea: lmer(CI ~ SH_Temp + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Csikamea_CI_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)
