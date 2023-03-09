#### ~ C. SIKAMEA TISSUE ~ =====

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
Csikamea_Tissue_stats <- read_csv("data/C_sikamea/Csikamea_CI_statsData.csv")
glimpse(Csikamea_Tissue_stats)
summary(Csikamea_Tissue_stats)
View(Csikamea_Tissue_stats)

### change attributes about statistical factors
Csikamea_Tissue_stats$SH_Temp <- as.factor(Csikamea_Tissue_stats$SH_Temp) ## factor
is.factor(Csikamea_Tissue_stats$SH_Temp) ## TRUE
Csikamea_Tissue_stats$SH_Tide <- as.factor(Csikamea_Tissue_stats$SH_Tide) ## factor
is.factor(Csikamea_Tissue_stats$SH_Tide) ## TRUE
Csikamea_Tissue_stats$MHW <- as.factor(Csikamea_Tissue_stats$MHW) ## character
is.factor(Csikamea_Tissue_stats$MHW) ## TRUE

#### C. SIKAMEA STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Tissue.g ~ 1 ===============
m_null <- glm(Tissue.g ~ 1, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m_null)

# Call:
# glm(formula = Tissue.g ~ 1, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.091801  -0.026901  -0.003701   0.020699   0.127499  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.095701   0.001419   67.44   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001423841)
#
# Null deviance: 1.0052  on 706  degrees of freedom
# Residual deviance: 1.0052  on 706  degrees of freedom
# AIC: -2624.6
#
# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Tissue.g ~ SH_Temp ===============
m1 <- glm(Tissue.g ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m1)

# Call:
# glm(formula = Tissue.g ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.088651  -0.027479  -0.003179   0.020985   0.124321  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.098879   0.002006  49.303   <2e-16 ***
#  SH_Temp21˚C -0.006329   0.002830  -2.236   0.0257 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001415819)
#
# Null deviance: 1.00523  on 706  degrees of freedom
# Residual deviance: 0.99815  on 705  degrees of freedom
# AIC: -2627.6
#
# Number of Fisher Scoring iterations: 2

#### m2: Tissue.g ~ SH_Tide ===============
m2 <- glm(Tissue.g ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m2)

#Call:
# glm(formula = Tissue.g ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.090913  -0.026942  -0.004013   0.021487   0.126529  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.094813   0.001965  48.248   <2e-16 ***
#  SH_TideTide 0.001857   0.002842   0.654    0.514    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001424997)
#
# Null deviance: 1.0052  on 706  degrees of freedom
# Residual deviance: 1.0046  on 705  degrees of freedom
# AIC: -2623

# Number of Fisher Scoring iterations: 2

#### m3: Tissue.g ~ MHW ===============
m3 <- glm(Tissue.g ~ MHW, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m3)

# Call:
# glm(formula = Tissue.g ~ MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.09473  -0.02646  -0.00326   0.02116   0.12614  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.102026   0.002807  36.350  < 2e-16 ***
#  MHW18˚C     -0.008517   0.003975  -2.143  0.03247 *  
#  MHW21˚C     -0.004966   0.003981  -1.248  0.21262    
# MHW24˚C     -0.012002   0.004004  -2.998  0.00281 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001410117)
#
# Null deviance: 1.00523  on 706  degrees of freedom
# Residual deviance: 0.99131  on 703  degrees of freedom
# AIC: -2628.4
#
# Number of Fisher Scoring iterations: 2

#### m4: Tissue.g ~ SH_Temp + MHW ===============
m4 <- glm(Tissue.g ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m4)

# Call:
# (formula = Tissue.g ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.091595  -0.026543  -0.003092   0.020986   0.126035  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.105122   0.003130  33.586   <2e-16 ***
#  SH_Temp21˚C -0.006228   0.002817  -2.211   0.0274 *  
#  MHW18˚C     -0.008430   0.003964  -2.127   0.0338 *  
#  MHW21˚C     -0.005001   0.003970  -1.260   0.2081    
# MHW24˚C     -0.011931   0.003993  -2.988   0.0029 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001402363)

# Null deviance: 1.00523  on 706  degrees of freedom
# Residual deviance: 0.98446  on 702  degrees of freedom
# AIC: -2631.3
#
# Number of Fisher Scoring iterations: 2

#### Interaction: Tissue.g ~ SH_Temp + MHW + SH_Temp*MHW ===============
m5 <- glm(Tissue.g ~ SH_Temp + MHW + SH_Temp*MHW, family = gaussian(link = "identity"), data = Csikamea_Tissue_stats)
summary(m5)

# Call:
# glm(formula = Tissue.g ~ SH_Temp + MHW + SH_Temp * MHW, family = gaussian(link = "identity"), 
#    data = Csikamea_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.091663  -0.026125  -0.002819   0.021325   0.125629  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.106119   0.003937  26.953  < 2e-16 ***
#  SH_Temp21˚C         -0.008232   0.005584  -1.474  0.14085    
# MHW18˚C             -0.009852   0.005616  -1.754  0.07980 .  
# MHW21˚C             -0.001356   0.005568  -0.244  0.80763    
# MHW24˚C             -0.018461   0.005649  -3.268  0.00114 ** 
#  SH_Temp21˚C:MHW18˚C  0.002837   0.007908   0.359  0.71992    
# SH_Temp21˚C:MHW21˚C -0.007439   0.007919  -0.939  0.34789    
# SH_Temp21˚C:MHW24˚C  0.012883   0.007965   1.617  0.10623    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.001395116)
#
# Null deviance: 1.00523  on 706  degrees of freedom
# Residual deviance: 0.97519  on 699  degrees of freedom
# AIC: -2632
#
#Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m6: Tissue.g ~ SH_Temp + MHW + (1|Tank) ===============
m6 <- lmer(Tissue.g ~ SH_Temp + MHW + (1|Tank), data =Csikamea_Tissue_stats)
summary(m6)
AIC(m6) ## -2579.794

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: Tissue.g ~ SH_Temp + MHW + (1 | Tank)
# Data: Csikamea_Tissue_stats
#
# REML criterion at convergence: -2593.8
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.4459 -0.7088 -0.0826  0.5604  3.3656 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.000000 0.00000 
# Residual             0.001402 0.03745 
# Number of obs: 707, groups:  Tank, 20
#
# Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   0.105122   0.003130 702.000000  33.586   <2e-16 ***
#  SH_Temp21˚C  -0.006228   0.002817 702.000000  -2.211   0.0274 *  
#  MHW18˚C      -0.008430   0.003964 702.000000  -2.127   0.0338 *  
#  MHW21˚C      -0.005001   0.003970 702.000000  -1.260   0.2081    
# MHW24˚C      -0.011931   0.003993 702.000000  -2.988   0.0029 ** 
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

#### m7: Tissue.g ~ SH_Temp + MHW + SH_Temp*MHW + (1|Tank) ===============
m7 <- lmer(Tissue.g ~ SH_Temp +  + SH_Temp*MHW + (1|Tank), data =Csikamea_Tissue_stats)
summary(m7)
AIC(m7) ## -2556.243

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: Tissue.g ~ SH_Temp + +SH_Temp * MHW + (1 | Tank)
# Data: Csikamea_Tissue_stats
#
# REML criterion at convergence: -2576.2
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.4541 -0.6994 -0.0755  0.5709  3.3634 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.000000 0.00000 
# Residual             0.001395 0.03735 
# Number of obs: 707, groups:  Tank, 20
#
# Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)           0.106119   0.003937 699.000000  26.953  < 2e-16 ***
#  SH_Temp21˚C          -0.008232   0.005584 699.000000  -1.474  0.14085    
# MHW18˚C              -0.009852   0.005616 699.000000  -1.754  0.07980 .  
# MHW21˚C              -0.001356   0.005568 699.000000  -0.244  0.80763    
# MHW24˚C              -0.018461   0.005649 699.000000  -3.268  0.00114 ** 
#  SH_Temp21˚C:MHW18˚C   0.002837   0.007908 699.000000   0.359  0.71992    
# SH_Temp21˚C:MHW21˚C  -0.007439   0.007919 699.000000  -0.939  0.34789    
# SH_Temp21˚C:MHW24˚C   0.012883   0.007965 699.000000   1.617  0.10623    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_Tm21˚C MHW18˚ MHW21˚ MHW24˚ SH_T21˚C:MHW1
# SH_Temp21˚C    -0.705                                             
# MHW18˚C        -0.701  0.494                                      
# MHW21˚C        -0.707  0.499     0.496                            
# MHW24˚C        -0.697  0.491     0.489  0.493                     
# SH_T21˚C:MHW1   0.498 -0.706    -0.710 -0.352 -0.347              
# SH_T21˚C:MHW21  0.497 -0.705    -0.349 -0.703 -0.346  0.498       
# SH_T21˚C:MHW24  0.494 -0.701    -0.347 -0.350 -0.709  0.495       
# SH_T21˚C:MHW21
# SH_Temp21˚C                  
# MHW18˚C                      
# MHW21˚C                      
# MHW24˚C                      
# SH_T21˚C:MHW1                
# SH_T21˚C:MHW21               
# SH_T21˚C:MHW24  0.494        
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6, m7)
BIC(m_null, m1, m2, m3, m4, m5, m6, m7)

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
# "0.99 (95% CI 0.99, 1.00; p=0.027)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m6.plot_byMHW <- ggpredict(m6, terms = c("MHW", "SH_Temp"))
plot(m6.plot_byMHW) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Csikamea_Tissue-MHW.png",width = 5.10, height = 5.77, dpi = 300)

m6.plot_bySHtemp <- ggpredict(m6, terms = c("SH_Temp", "MHW"))
plot(m6.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Csikamea_Tissue-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)


####### With individual tanks plotted
Csikamea_Tissue_stats$fit <- predict(m6)

## By SH_Temp
ggplot(Csikamea_Tissue_stats, aes(x = SH_Temp, y = Tissue.g, group = interaction(Tank, SH_Temp), col = MHW)) +  #, shape = MHW )) + 
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Csikamea_Tissue-SH_Temp_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Csikamea_Tissue_stats, aes(x = SH_Temp, y = Tissue.g, group = interaction(Tank, SH_Temp), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Csikamea_Tissue_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)


