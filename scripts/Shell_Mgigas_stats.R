#### ~ M. GIGAS SHELL ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ##another model plotting option

### load data sheet
Mgigas_Shell_stats <- read_csv("data/M_gigas/Mgigas_CI_statsData.csv")
glimpse(Mgigas_Shell_stats)
summary(Mgigas_Shell_stats)
View(Mgigas_Shell_stats)

### change attributes about statistical factors
Mgigas_Shell_stats$SH_Temp <- as.factor(Mgigas_Shell_stats$SH_Temp) ## factor
is.factor(Mgigas_Shell_stats$SH_Temp) ## TRUE
Mgigas_Shell_stats$SH_Tide <- as.factor(Mgigas_Shell_stats$SH_Tide) ## factor
is.factor(Mgigas_Shell_stats$SH_Tide) ## TRUE
Mgigas_Shell_stats$MHW <- as.factor(Mgigas_Shell_stats$MHW) ## character
is.factor(Mgigas_Shell_stats$MHW) ## TRUE

#### M. GIGAS SHELL STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Shell.g ~ 1 ===============
m_null <- glm(Shell.g ~ 1, family = gaussian(link = "identity"), data = Mgigas_Shell_stats)
summary(m_null)

# Call:
# glm(formula = Shell.g ~ 1, family = gaussian(link = "identity"), 
#    data = Mgigas_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.50498  -0.14423  -0.02718   0.13242   1.64092  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.128876   0.008087   139.6   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.05069023)
#
# Null deviance: 39.234  on 774  degrees of freedom
# Residual deviance: 39.234  on 774  degrees of freedom
# AIC: -108.71
#
# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Shell.g ~ SH_Temp ===============
m1 <- glm(Shell.g ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_Shell_stats)
summary(m1)

# Call:
# glm(formula = Shell.g ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Mgigas_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.50743  -0.14327  -0.02443   0.13803   1.63457  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.13523    0.01143  99.296   <2e-16 ***
#  SH_Temp21˚C -0.01273    0.01618  -0.787    0.432    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 0.05071521)

# Null deviance: 39.234  on 774  degrees of freedom
# Residual deviance: 39.203  on 773  degrees of freedom
# AIC: -107.33

# Number of Fisher Scoring iterations: 2

#### m2: Shell.g ~ SH_Tide ===============
m2 <- glm(Shell.g ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_Shell_stats)
summary(m2)

# Call:
# glm(formula = Shell.g ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.50559  -0.14319  -0.02519   0.13356   1.63641  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.133394   0.011465  98.860   <2e-16 ***
#  SH_TideTide -0.009002   0.016182  -0.556    0.578    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 0.05073549)

# Null deviance: 39.234  on 774  degrees of freedom
# Residual deviance: 39.219  on 773  degrees of freedom
# AIC: -107.02

# Number of Fisher Scoring iterations: 2

#### m3: Shell.g ~ MHW ===============
m3 <- glm(Shell.g ~ MHW, family = gaussian(link = "identity"), data = Mgigas_Shell_stats)
summary(m3)

# Call:
#  Call:
# glm(formula = Shell.g ~ MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_Shell_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.50713  -0.14589  -0.02837   0.13584   1.63953  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.124455   0.016071  69.968   <2e-16 ***
#  MHW18˚C     0.005387   0.022967   0.235    0.815    
# MHW21˚C     0.005817   0.022728   0.256    0.798    
# MHW24˚C     0.006571   0.022875   0.287    0.774    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0508806)
#
# Null deviance: 39.234  on 774  degrees of freedom
# Residual deviance: 39.229  on 771  degrees of freedom
# AIC: -102.82
#
# Number of Fisher Scoring iterations: 2

#### Interaction ===============
## N/A ==

#### Random Factor: (1|Tank), lmer ===============
#### m4: Shell.g ~ (1|Tank) ===============

#### NON-SIGNIFICANT
 m4 <- lmer(Shell.g ~ SH_Temp + SH_Tide + MHW + (1|Tank), data = Mgigas_Shell_stats)
summary(m4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#   method [lmerModLmerTest]
# Formula: Shell.g ~ SH_Temp + SH_Tide + MHW + (1 | Tank)
# Data: Mgigas_Shell_stats
#
# REML criterion at convergence: -75.5
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.2842 -0.6414 -0.1123  0.6015  7.2024 
#
# Random effects:
#  Groups   Name        Variance  Std.Dev.
# Tank     (Intercept) 0.0002196 0.01482 
# Residual             0.0507741 0.22533 
# Number of obs: 775, groups:  Tank, 20
# 
# Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)   1.135304   0.020790  31.986352  54.608   <2e-16
# SH_Temp21˚C  -0.012796   0.016189 753.604793  -0.790    0.430
# SH_TideTide  -0.009034   0.016191 753.780256  -0.558    0.577
# MHW18˚C       0.005569   0.024786  16.210480   0.225    0.825
# MHW21˚C       0.005902   0.024563  15.641664   0.240    0.813
# MHW24˚C       0.006605   0.024699  15.988597   0.267    0.793
#
# (Intercept) ***
#  SH_Temp21˚C    
# SH_TideTide    
# MHW18˚C        
# MHW21˚C        
# MHW24˚C        
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_T21 SH_TdT MHW18˚ MHW21˚
# SH_Temp21˚C -0.390                            
# SH_TideTide -0.390  0.007                     
# MHW18˚C     -0.580 -0.003 -0.010              
# MHW21˚C     -0.589 -0.003  0.000  0.496       
# MHW24˚C     -0.587  0.002 -0.002  0.493  0.497

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

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.m4 <- tbl_regression(m4, exponentiate = TRUE) ## table!
tbl.m4 ### NON-SIGNIFICANT
inline_text(tbl.m4,  variable = MHW, level = "21˚C") ##in-line text
# "1.02 (95% CI 0.91, 1.14; p=0.7)"


#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m4.plot_byMHW <- ggpredict(m4, terms = "MHW")
plot(m4.plot_byMHW) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  #scale_color_manual(values = c("#4575B4", "#ABD9E9",  "#FDAE61", "#D73027")) +
  labs(title = expression(paste("M. gigas: lmer(Shell.g ~ MHW + (1|Tank)")), 
       subtitle = "NON-SIGNIFICANT",
       x = "Marine Heatwave (°C)", 
       y = "Shell(g)")

ggsave(filename = "fig_output/NSmodel_Mgigas_Shell-MHW_b&w.png",width = 5.10, height = 5.77, dpi = 300)

#library(RColorBrewer)
#brewer.pal(11, "RdYlBu")

####### With individual tanks plotted
Mgigas_Shell_stats$fit <- predict(m4)

## By MHW
ggplot(Mgigas_Shell_stats, aes(x = MHW, y = Shell.g, col = MHW, group = Tank)) + 
  #facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  #geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("M. gigas: lmer(Shell.g ~ MHW + (1|Tank)")), 
       subtitle = "NON-SIGNIFICANT",
       x = "Stress Hardening Temperature (°C)", 
       y = "Shell (g)")

ggsave(filename = "fig_output/NSmodel_Mgigas_Shell_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)