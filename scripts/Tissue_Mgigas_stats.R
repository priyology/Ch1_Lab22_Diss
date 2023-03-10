#### ~ M. GIGAS TISSUE ~ =====

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
Mgigas_Tissue_stats <- read_csv("data/M_gigas/Mgigas_CI_statsData.csv")
glimpse(Mgigas_Tissue_stats)
summary(Mgigas_Tissue_stats)
View(Mgigas_Tissue_stats)

### change attributes about statistical factors
Mgigas_Tissue_stats$SH_Temp <- as.factor(Mgigas_Tissue_stats$SH_Temp) ## factor
is.factor(Mgigas_Tissue_stats$SH_Temp) ## TRUE
Mgigas_Tissue_stats$SH_Tide <- as.factor(Mgigas_Tissue_stats$SH_Tide) ## factor
is.factor(Mgigas_Tissue_stats$SH_Tide) ## TRUE
Mgigas_Tissue_stats$MHW <- as.factor(Mgigas_Tissue_stats$MHW) ## character
is.factor(Mgigas_Tissue_stats$MHW) ## TRUE

#### M. GIGAS STATS ===============

#### Gaussian Distribution ========
#### Fixed Factors: SH_Temp, SH_Tide, MHW ========

#### m_null: Tissue.g ~ 1 ===============
m_null <- glm(Tissue.g ~ 1, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m_null)

# Call:
# (formula = Tissue.g ~ 1, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.027789  -0.008389  -0.001989   0.006261   0.173811  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.0420888  0.0005024   83.77   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001956274)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.15142  on 774  degrees of freedom
# AIC: -4415.6
#
# Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### m1: Tissue.g ~ SH_Temp ===============
m1 <- glm(Tissue.g ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m1)

# Call:
# glm(formula = Tissue.g ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.026668  -0.008487  -0.001806   0.006732   0.174932  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0432065  0.0007082  61.005   <2e-16 ***
#  SH_Temp21˚C -0.0022383  0.0010023  -2.233   0.0258 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001946247)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.15044  on 773  degrees of freedom
# AIC: -4418.6
#
# Number of Fisher Scoring iterations: 2

#### m2: Tissue.g ~ SH_Tide ===============
m2 <- glm(Tissue.g ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m2)

# Call:
# glm(formula = Tissue.g ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.029001  -0.008381  -0.001361   0.006389   0.171799  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.0400607  0.0007049  56.833  < 2e-16 ***
#  SH_TideTide 0.0040406  0.0009949   4.061 5.38e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001917883)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14825  on 773  degrees of freedom
# AIC: -4430
#
# Number of Fisher Scoring iterations: 2

#### m3: Tissue.g ~ MHW ===============
m3 <- glm(Tissue.g ~ MHW, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m3)

# Call:
# (formula = Tissue.g ~ MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.028943  -0.008200  -0.001612   0.006580   0.169957  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0459426  0.0009831  46.733  < 2e-16 ***
#   MHW18˚C     -0.0045304  0.0014049  -3.225  0.00131 ** 
#   MHW21˚C     -0.0042629  0.0013903  -3.066  0.00224 ** 
#  MHW24˚C     -0.0067223  0.0013993  -4.804 1.87e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001903897)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14679  on 771  degrees of freedom
# AIC: -4433.6
#
# Number of Fisher Scoring iterations: 2

#### 4: Tissue.g ~ SH_Temp + SH_Tide ===============
m4 <- glm(Tissue.g ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m4)

# Call:
#  glm(formula = Tissue.g ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), 
#      data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.027887  -0.008123  -0.001487   0.005970   0.172913  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0411726  0.0008620  47.762  < 2e-16 ***
#  SH_Temp21˚C -0.0022123  0.0009924  -2.229   0.0261 *  
#  SH_TideTide  0.0040263  0.0009924   4.057 5.47e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001908085)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14730  on 772  degrees of freedom
# AIC: -4432.9
#
# Number of Fisher Scoring iterations: 2

#### m5: Tissue.g ~ SH_Temp + MHW ===============
m5 <- glm(Tissue.g ~ SH_Temp + MHW, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m5)

# Call:
# glm(formula = Tissue.g ~ SH_Temp + MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.027817  -0.007932  -0.001665   0.006362   0.171083  
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0470572  0.0010969  42.900  < 2e-16 ***
#  SH_Temp21˚C -0.0022404  0.0009887  -2.266  0.02372 *  
#  MHW18˚C     -0.0045188  0.0014012  -3.225  0.00131 ** 
#  MHW21˚C     -0.0042516  0.0013866  -3.066  0.00224 ** 
#  MHW24˚C     -0.0067283  0.0013956  -4.821 1.72e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.000189374)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14582  on 770  degrees of freedom
# AIC: -4436.8
#
# Number of Fisher Scoring iterations: 2

#### m6: Tissue.g ~ SH_Temp + SH_Tide + MHW ===============
m6 <- glm(Tissue.g ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m6)

# Call:
# glm(formula = Tissue.g ~ SH_Temp + SH_Tide + MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.027180  -0.008209  -0.001394   0.005998   0.169038  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0450321  0.0011911  37.808  < 2e-16 ***
#  SH_Temp21˚C -0.0022142  0.0009785  -2.263 0.023926 *  
#  SH_TideTide  0.0040446  0.0009786   4.133 3.97e-05 ***
#  MHW18˚C     -0.0045827  0.0013869  -3.304 0.000996 ***
#  MHW21˚C     -0.0042517  0.0013723  -3.098 0.002018 ** 
#  MHW24˚C     -0.0067385  0.0013812  -4.879 1.30e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001854996)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14265  on 769  degrees of freedom
# AIC: -4451.8
#
# Number of Fisher Scoring iterations: 2

#### Interaction: Tissue.g ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + SH_Temp*SH_Tide + SH_Tide*MHW + SH_Temp*SH_Tide*MHW ===============
m7 <- glm(Tissue.g ~ SH_Temp + SH_Tide + MHW + SH_Temp*MHW + SH_Temp*SH_Tide + SH_Tide*MHW + SH_Temp*SH_Tide*MHW, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
summary(m7)

# Call:
#  glm(formula = Tissue.g ~ SH_Temp + SH_Tide + MHW + SH_Temp * 
#      MHW + SH_Temp * SH_Tide + SH_Tide * MHW + SH_Temp * SH_Tide * 
#      MHW, family = gaussian(link = "identity"), data = Mgigas_Tissue_stats)
#
# Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
# -0.025638  -0.008248  -0.001406   0.006140   0.166142  
#
# Coefficients:
#  Estimate Std. Error t value
# (Intercept)                      0.0442388  0.0019494  22.694
# SH_Temp21˚C                     -0.0023148  0.0027430  -0.844
# SH_TideTide                      0.0037292  0.0027430   1.360
# MHW18˚C                         -0.0020183  0.0028174  -0.716
# ˚C                         -0.0028308  0.0027430  -1.032
# MHW24˚C                         -0.0050429  0.0027711  -1.820
# SH_Temp21˚C:MHW18˚C             -0.0017035  0.0039527  -0.431
# SH_Temp21˚C:MHW21˚C             -0.0010320  0.0038792  -0.266
# SH_Temp21˚C:MHW24˚C             -0.0017290  0.0039093  -0.442
# SH_Temp21˚C:SH_TideTide          0.0041051  0.0038894   1.055
# SH_TideTide:MHW18˚C             -0.0015395  0.0039321  -0.392
# SH_TideTide:MHW21˚C              0.0004378  0.0038894   0.113
# SH_TideTide:MHW24˚C             -0.0024638  0.0038991  -0.632
# SH_Temp21˚C:SH_TideTide:MHW18˚C -0.0037595  0.0055593  -0.676
# SH_Temp21˚C:SH_TideTide:MHW21˚C -0.0045753  0.0055004  -0.832
# SH_Temp21˚C:SH_TideTide:MHW24˚C  0.0015838  0.0055360   0.286

# Pr(>|t|)    
# (Intercept)                       <2e-16 ***
#  SH_Temp21˚C                       0.3990    
# SH_TideTide                       0.1744    
# MHW18˚C                           0.4740    
# MHW21˚C                           0.3024    
# MHW24˚C                           0.0692 .  
# SH_Temp21˚C:MHW18˚C               0.6666    
# SH_Temp21˚C:MHW21˚C               0.7903    
# SH_Temp21˚C:MHW24˚C               0.6584    
# SH_Temp21˚C:SH_TideTide           0.2915    
# SH_TideTide:MHW18˚C               0.6955    
# SH_TideTide:MHW21˚C               0.9104    
# SH_TideTide:MHW24˚C               0.5276    
# SH_Temp21˚C:SH_TideTide:MHW18˚C   0.4991    
# SH_Temp21˚C:SH_TideTide:MHW21˚C   0.4058    
# SH_Temp21˚C:SH_TideTide:MHW24˚C   0.7749    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.0001861996)
#
# Null deviance: 0.15142  on 774  degrees of freedom
# Residual deviance: 0.14133  on 759  degrees of freedom
# AIC: -4439
#
# Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### m8: Tissue.g ~ SH_Temp + SH_Tide + MHW + (1|Tank) ===============
m8 <- lmer(Tissue.g ~ SH_Temp + SH_Tide + MHW + (1|Tank), data =Mgigas_Tissue_stats)
summary(m8)

#Linear mixed model fit by REML. t-tests use Satterthwaite's
#  method [lmerModLmerTest]
#Formula: Tissue.g ~ SH_Temp + SH_Tide + MHW + (1 | Tank)
#   Data: Mgigas_Tissue_stats
#
# REML criterion at convergence: -4411.8
#
# Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
# -1.9954 -0.5926 -0.1052  0.4459 12.1715 
#
# Random effects:
# Groups   Name        Variance  Std.Dev.
# Tank     (Intercept) 0.0000109 0.003301
# Residual             0.0001766 0.013290
# Number of obs: 775, groups:  Tank, 20
#
# Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)  4.500e-02  1.879e-03  2.102e+01  23.946  < 2e-16
# SH_Temp21˚C -2.218e-03  9.550e-04  7.533e+02  -2.323   0.0205
# SH_TideTide  4.039e-03  9.551e-04  7.534e+02   4.229 2.63e-05
# MHW18˚C     -4.555e-03  2.488e-03  1.617e+01  -1.831   0.0857
# MHW21˚C     -4.193e-03  2.481e-03  1.597e+01  -1.690   0.1104
# MHW24˚C     -6.712e-03  2.485e-03  1.609e+01  -2.701   0.0157
#             
# (Intercept) ***
# SH_Temp21˚C *  
# SH_TideTide ***
# MHW18˚C     .  
# MHW21˚C        
# MHW24˚C     *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#            (Intr) SH_T21 SH_TdT MHW18˚ MHW21˚
# SH_Temp21˚C -0.254                            
# SH_TideTide -0.255  0.007                     
# MHW18˚C     -0.656 -0.002 -0.006              
# MHW21˚C     -0.660 -0.002  0.000  0.498       
# MHW24˚C     -0.659  0.001 -0.001  0.497  0.499

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6, m7, m8)
BIC(m_null, m1, m2, m3, m4, m5, m6, m7, m8)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8), resid(m8))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8))
qqline(resid(m8))

#### Density Plot of Residuals ===============
plot(density(resid(m8)))

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.m8 <- tbl_regression(m8, exponentiate = TRUE) ## table!
tbl.m8
inline_text(tbl.m8,  variable = SH_Temp, level = "21˚C") ##in-line text
# "0.99 (95% CI 0.99, 1.00; p=0.027)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m8.plot_byMHW <- ggpredict(m8, terms = c("MHW", "SH_Temp", "SH_Tide"))
plot(m8.plot_byMHW) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("M. gigas: lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Mgigas_Tissue-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)

m8.plot_bySHtemp <- ggpredict(m8, terms = c("SH_Temp", "SH_Tide", "MHW"))
plot(m8.plot_bySHtemp) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("M. gigas:lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Mgigas_Tissue-SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)


####### With individual tanks plotted
Mgigas_Tissue_stats$fit <- predict(m8)

## By SH_Temp
ggplot(Mgigas_Tissue_stats, aes(x = SH_Temp, y = Tissue.g, group = interaction(Tank, SH_Temp), col = MHW)) +  #, shape = MHW )) + 
  geom_line(aes(y = fit, lty = MHW), size=0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("M. gigas: lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Marine Heatwave (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Mgigas_Tissue-SH_Temp_Tanks.png",width = 5.10, height = 5.77, dpi = 300)

## By MHW
ggplot(Mgigas_Tissue_stats, aes(x = SH_Temp, y = Tissue.g, group = interaction(Tank, SH_Temp), col = MHW)) +  #=, shape = MHW )) + 
  facet_grid(~ MHW) +
  geom_line(aes(y = fit, lty = MHW), size = 0.8) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = expression(paste("M. gigas: lmer(Tissue ~ SH_Temp + MHW + (1|Tank)")), 
       subtitle = "Gaussian distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Tissue (g)")

ggsave(filename = "fig_output/model_Mgigas_Tissue_byMHW_Tanks.png",width = 5.10, height = 5.77, dpi = 300)


