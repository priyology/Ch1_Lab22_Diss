#### ~ M. GIGAS CONDITION INDEX ~ =====

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
Mgigas_CI_stats <- read_csv("data/M_gigas/Mgigas_CI_StatsData.csv")
glimpse(Mgigas_CI_stats)
summary(Mgigas_CI_stats)
View(Mgigas_CI_stats)

#### M. GIGAS CONDITION INDEX STATS ===============
#### m_null: CI ~ 1 ===============
m_null <- glm(CI ~ 1, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m_null)

# Call:
# glm(formula = CI ~ 1, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.4461  -0.6648  -0.1504   0.4583  24.3082  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.77091    0.04753   79.34   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.7505)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1354.9  on 774  degrees of freedom
# AIC: 2636.3
#
# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: CI ~ SH_Temp ===============
m1 <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m1)

# Call:
# glm(formula = CI ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.3679  -0.6464  -0.1323   0.4672  24.3863  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.84885    0.06709  57.364   <2e-16 ***
#  SH_Temp21˚C -0.15607    0.09495  -1.644    0.101    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.746659)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1350.2  on 773  degrees of freedom
# AIC: 2635.6

# Number of Fisher Scoring iterations: 2

#### m2: CI ~ SH_Tide ===============
m2 <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m2)

# Call:
# glm(formula = CI ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.6407  -0.6816  -0.1075   0.4573  24.1135  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.57474    0.06665  53.638  < 2e-16 ***
#  SH_TideTide  0.39084    0.09407   4.155 3.62e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.714477)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1325.3  on 773  degrees of freedom
# AIC: 2621.2
#
# Number of Fisher Scoring iterations: 2

#### m3: CI ~ MHW ===============
m3 <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m3)

# Call:
# glm(formula = CI ~ MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.3630  -0.6729  -0.1015   0.4751  23.9227  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.15633    0.09289  44.747  < 2e-16 ***
#  MHW18˚C     -0.46855    0.13274  -3.530 0.000441 ***
#  MHW21˚C     -0.43336    0.13136  -3.299 0.001015 ** 
#  MHW24˚C     -0.64985    0.13221  -4.915 1.08e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.699661)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1310.4  on 771  degrees of freedom
# AIC: 2616.4
#
# Number of Fisher Scoring iterations: 2

#### m4: CI ~ SH_Tide + MHW ===============
m4 <- glm(CI ~ SH_Tide + MHW, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m4)

# Call:
# glm(formula = CI ~ SH_Tide + MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.5542  -0.6705  -0.0992   0.4925  23.7253  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.96090    0.10279  38.533  < 2e-16 ***
#  SH_TideTide  0.39285    0.09266   4.240 2.51e-05 ***
#  MHW18˚C     -0.47474    0.13131  -3.615 0.000320 ***
#  MHW21˚C     -0.43336    0.12994  -3.335 0.000893 ***
#  MHW24˚C     -0.65085    0.13078  -4.977 7.99e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.663043)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1280.5  on 770  degrees of freedom
# AIC: 2600.5
#
# Number of Fisher Scoring iterations: 2

#### Interaction ===============
#### m5: CI ~ SH_Tide + MHW + SH_Tide*MHW ===============
m5 <- glm(CI ~ SH_Tide + MHW + SH_Tide*MHW, family = gaussian(link = "identity"), data = Mgigas_CI_stats)
summary(m5)

# Call:
# glm(formula = CI ~ SH_Tide + MHW + SH_Tide * MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_CI_stats)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.4983  -0.6490  -0.0975   0.4791  23.6032  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.8400     0.1295  29.654  < 2e-16 ***
#  SH_TideTide           0.6358     0.1836   3.463 0.000564 ***
#  MHW18˚C              -0.2949     0.1866  -1.581 0.114354    
# MHW21˚C              -0.1888     0.1831  -1.031 0.302780    
# MHW24˚C              -0.5893     0.1846  -3.193 0.001464 ** 
#  SH_TideTide:MHW18˚C  -0.3578     0.2624  -1.363 0.173144    
# SH_TideTide:MHW21˚C  -0.4915     0.2596  -1.893 0.058722 .  
# SH_TideTide:MHW24˚C  -0.1243     0.2613  -0.476 0.634545    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.660096)
#
# Null deviance: 1354.9  on 774  degrees of freedom
# Residual deviance: 1273.3  on 767  degrees of freedom
# AIC: 2602.1
#
# Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m6: CI ~ SH_Tide + MHW + SH_Tide*MHW (1|Tank) ===============
m6 <- lmer(CI ~ SH_Tide + MHW + SH_Tide*MHW (1|Tank), data = Mgigas_CI_stats)
summary(m6)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: CI ~ SH_Tide + MHW + (1 | Tank)
# Data: Mgigas_CI_stats
#
# REML criterion at convergence: 2588.2
#
# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.9018 -0.4903 -0.0939  0.3365 18.2604 
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
# Tank     (Intercept) 0.08589  0.2931  
# Residual             1.59329  1.2623  
# Number of obs: 775, groups:  Tank, 20
#
# Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   3.95787    0.16524  18.62953  23.953 1.87e-15 ***
#  SH_TideTide   0.39120    0.09071 754.39466   4.313 1.83e-05 ***
#  MHW18˚C      -0.47126    0.22557  16.17708  -2.089   0.0528 .  
# MHW21˚C      -0.42790    0.22480  15.95786  -1.903   0.0752 .  
# MHW24˚C      -0.64857    0.22527  16.09171  -2.879   0.0109 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SH_TdT MHW18˚ MHW21˚
# SH_TideTide -0.273                     
# MHW18˚C     -0.676 -0.006              
# MHW21˚C     -0.680  0.000  0.498       
# MHW24˚C     -0.679 -0.001  0.497  0.499

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
inline_text(tbl.m6,  variable = SH_Tide, level = "Tide") ##in-line text
# "1.48 (95% CI 1.24, 1.77; p<0.001)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m6.plot_byMHW <- ggpredict(m6, terms = c("MHW", "SH_Tide"))
plot(m6.plot_byMHW) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("lmer(CI ~ SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Mgigas_CI-MHW.png",width = 5.10, height = 5.77, dpi = 300)

m6.plot_bySHtide <- ggpredict(m6, terms = c("SH_Tide", "MHW"))
plot(m6.plot_bySHtide) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(CI ~ SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Tide", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Mgigas_CI-SH_Tide.png",width = 5.10, height = 5.77, dpi = 300)


####### With individual tanks plotted
Mgigas_CI_stats$fit <- predict(m6)

## By CI ~ SH_Tide
ggplot(Mgigas_CI_stats, aes(x = SH_Tide, y = CI, group = interaction(Tank, SH_Tide), col = MHW)) +  #, shape = MHW )) + 
  #facet_grid(~ SH_Tide) +
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(CI ~ SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Mgigas_CI-SH_Tide_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Mgigas_CI_stats, aes(x = SH_Tide, y = CI, group = interaction(Tank, SH_Tide), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("lmer(CI ~ SH_Tide + MHW + SH_Tide*MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Tide", 
       y = "Condition Index")

ggsave(filename = "fig_output/model_Mgigas_CI_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

