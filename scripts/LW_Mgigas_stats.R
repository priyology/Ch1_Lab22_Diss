#### ~ M. GIGAS Length (cm) & Width (cm) STATS ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(pbkrtest) ## to use with emmeans
library(ggeffects) ## another model plotting option


#### XXXXXXXXXx =================


#### DAY 1 =====

### load data sheet

Mgigas_LWd1 <- read_csv("data/M_gigas/LWd1_stats.csv")
glimpse(Mgigas_LWd1)
summary(Mgigas_LWd1)
View(Mgigas_LWd1)

#### Model selection ====
m.LWd1 <- lm(L ~ W, data = Mgigas_LWd1)
summary(m.LWd1)

# Call:
#lm(formula = L ~ W, data = Mgigas_LWd1)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.75222 -0.20258 -0.01866  0.20122  0.82972 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.0452     0.3837   7.937 3.45e-12 ***
#  W            -0.2438     0.2173  -1.122    0.265    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3091 on 98 degrees of freedom
#Multiple R-squared:  0.01268,	Adjusted R-squared:  0.002607 
#F-statistic: 1.259 on 1 and 98 DF,  p-value: 0.2646

### plot model

library(ggdark)
m.LWd1.plot <- ggpredict(m.LWd1, terms = "W")
plot(m.LWd1.plot) +
  dark_theme_classic() +
  #scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("M. gigas: L ~ W")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Width (cm)", 
       y = "Length (cm)")

#### *** ======================================================

#### End MHW =====
Mgigas_LWend_stats <- read_csv("data/M_gigas/LW_EndMHW_stats.csv")
glimpse(Mgigas_LWend_stats)
summary(Mgigas_LWend_stats)
View(Mgigas_LWend_stats)

### change attributes about statistical factors
Mgigas_LWend_stats$SH_Temp <- as.factor(Mgigas_LWend_stats$SH_Temp) ## factor
is.factor(Mgigas_LWend_stats$SH_Temp) ## TRUE
Mgigas_LWend_stats$SH_Tide <- as.factor(Mgigas_LWend_stats$SH_Tide) ## factor
is.factor(Mgigas_LWend_stats$SH_Tide) ## TRUE
Mgigas_LWend_stats$MHW <- as.factor(Mgigas_LWend_stats$MHW) ## character
is.factor(Mgigas_LWend_stats$MHW) ## TRUE

#### M. GIGAS STATS ===============

#### Gaussian Distribution ========

#### W ~ L ====

m.LW <- glm(W ~ L, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(m.LW)

# Call:
#glm(formula = W ~ L, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4941  -0.1349  -0.0207   0.1104   0.9459  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.49429    0.05128  29.141  < 2e-16 ***
#  L            0.12766    0.01839   6.941 8.17e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.04010846)
#
#Null deviance: 33.377  on 785  degrees of freedom
#Residual deviance: 31.445  on 784  degrees of freedom
#AIC: -293.34
#
#Number of Fisher Scoring iterations: 2


### plot model

library(ggdark)
m.LW.plot <- ggpredict(m.LW, terms = "L")
plot(m.LW.plot) +
  dark_theme_classic() +
  #scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("M. gigas: W ~ L")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Length (cm)", 
       y = "Width (cm)")

#### LENGTHS ========

#### L.m_null: L ~ 1 ===============
L.m_null <- glm(L ~ 1, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(L.m_null)

# Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.06178  -0.27878  -0.02728   0.25872   1.30622  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.76078    0.01386   199.2   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1510391)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 118.57  on 785  degrees of freedom
#AIC: 747.86
#
#Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### L.m1: L ~ SH_Temp ===============
L.m1 <- glm(L ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(L.m1)

# Call:
#glm(formula = L ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.01663  -0.28045  -0.02463   0.25939   1.29837  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.71563    0.01948 139.380  < 2e-16 ***
#  SH_Temp21    0.09032    0.02755   3.278  0.00109 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1491873)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 116.96  on 784  degrees of freedom
#AIC: 739.16
#
#Number of Fisher Scoring iterations: 2

#### L.m2: L ~ SH_Tide ===============
L.m2 <- glm(L ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(L.m2)

# Call:
#glm(formula = L ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.16667  -0.26849  -0.02605   0.24407   1.36457  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.86567    0.01892 151.497  < 2e-16 ***
#  SH_TideTide -0.20924    0.02672  -7.832 1.56e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1402584)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 109.96  on 784  degrees of freedom
#AIC: 690.65


#### L.m3: L ~ MHW ===============
#L.m3 <- glm(L ~ MHW, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
#summary(L.m3)
#
#Call:
#  glm(formula = L ~ MHW, family = gaussian(link = "identity"), 
#      data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.0824  -0.2794  -0.0285   0.2674   1.2856  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2.747385   0.027518  99.840   <2e-16 ***
#  MHW18       0.015750   0.039267   0.401    0.688    
#MHW21       0.004484   0.038965   0.115    0.908    
#MHW24       0.034017   0.039216   0.867    0.386    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1514456)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 118.43  on 782  degrees of freedom
#AIC: 752.96
#
#Number of Fisher Scoring iterations: 2


#### L.m4: L ~ SH_Temp + SH_Tide ===============
L.m4 <- glm(L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(L.m4)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.12182  -0.26628  -0.01992   0.25696   1.31972  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.82082    0.02305 122.402  < 2e-16 ***
#  SH_Temp21    0.08925    0.02654   3.363  0.00081 ***
#  SH_TideTide -0.20879    0.02654  -7.866 1.22e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1384384)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 108.40  on 783  degrees of freedom
#AIC: 681.38
#
#Number of Fisher Scoring iterations: 2

#### Interactions ===============

#### L.m5: L ~ SH_Temp + SH_Tide + SH_Temp*SH_Tide ===============
L.m5 <- glm(L ~ SH_Temp + SH_Tide + SH_Temp*SH_Tide, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(L.m5)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + SH_Temp * SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.11862  -0.26537  -0.01962   0.25453   1.32290  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            2.81762    0.02666 105.684  < 2e-16 ***
#  SH_Temp21              0.09562    0.03761   2.543   0.0112 *  
#  SH_TideTide           -0.20243    0.03756  -5.389 9.36e-08 ***
#  SH_Temp21:SH_TideTide -0.01271    0.05312  -0.239   0.8110    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1386053)
#
#Null deviance: 118.57  on 785  degrees of freedom
#Residual deviance: 108.39  on 782  degrees of freedom
#AIC: 683.33
#
#Number of Fisher Scoring iterations: 2

#### Random Factor: (1|Tank), lmer ===============
#### FINAL MODEL: L.m6: SH_Temp + SH_Tide + (1|Tank) ===============
L.m6 <- lmer(L ~ SH_Temp + SH_Tide + (1|Tank), data = Mgigas_LWend_stats)
summary(L.m6)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: L ~ SH_Temp + SH_Tide + (1 | Tank)
#Data: Mgigas_LWend_stats
#
#REML criterion at convergence: 687.6
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.9135 -0.7238 -0.0460  0.6477  3.4875 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Tank     (Intercept) 0.002566 0.05066 
#Residual             0.135990 0.36877 
#Number of obs: 786, groups:  Tank, 20
#
#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   2.82079    0.02550  85.55116 110.627  < 2e-16 ***
#  SH_Temp21     0.08937    0.02631 764.42687   3.397 0.000717 ***
#  SH_TideTide  -0.20860    0.02631 764.42136  -7.929 7.85e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SH_T21
#SH_Temp21   -0.519       
#SH_TideTide -0.520  0.005

#### AIC/BIC Scores ===============
AIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6)
BIC(L.m_null, L.m1, L.m2, L.m3, L.m4, L.m5, L.m6)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(L.m6), resid(L.m6))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(L.m6))
qqline(resid(L.m6))

#### Density Plot of Residuals ===============
plot(density(resid(L.m6)))

#### Pairwise Comparisons ===============
## pairwise comparison for m12
emm_Lm6a <-  emmeans(L.m6, specs = ~ SH_Tide|SH_Temp)
emm_Lm6a
pairwise_Lm6a <- contrast(emm_Lm6a, interaction = "pairwise")
pairwise_Lm6a

# SH_Temp = 15:
#SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide      0.209 0.0263 764   7.928  <.0001
#
#SH_Temp = 21:
#  SH_Tide_pairwise estimate     SE  df t.ratio p.value
#No Tide - Tide      0.209 0.0263 764   7.928  <.0001
#
#Degrees-of-freedom method: kenward-roger 
#
#emm_Lm6b <-  emmeans(L.m6, specs = ~ SH_Temp|SH_Tide)
#emm_Lm6b
#pairwise_Lm6b <- contrast(emm_Lm6b, interaction = "pairwise")
#pairwise_Lm6b

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.Lm6 <- tbl_regression(L.m6, exponentiate = TRUE) ## table!
tbl.Lm6
inline_text(tbl.Lm6,  variable = SH_Temp, level = "21") ##in-line text
# "1.14 (95% CI 1.05, 1.24; p=0.003)"

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

L.m6.plot_bySH_Temp.Tide <- ggpredict(L.m6, terms = c("SH_Temp", "SH_Tide"))
plot(L.m6.plot_bySH_Temp.Tide) +
  theme_classic() +
  scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste("M. gigas: lmer(L ~ SH_Temp + SH_Tide W + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Length (cm)")

ggsave(filename = "fig_output/model_Mgigas_Length-SH_Temp.png", width = 5.10, height = 5.77, dpi = 300)

L.m6.plot_bySHtide <- ggpredict(L.m6, terms = c("SH_Tide", "SH_Temp"))
plot(L.m6.plot_bySHtide) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("M. gigas: lmer(L ~ SH_Temp + SH_Tide + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = " ", 
       y = "Length (cm)")

ggsave(filename = "fig_output/model_Mgigas_Length-Tide.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

L.m6.DARKplot_bySH_Temp.Tide <- ggpredict(L.m6, terms = c("SH_Temp", "SH_Tide"))

plot(L.m6.DARKplot_bySH_Temp.Tide) +
  dark_theme_classic() +
  scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste("M. gigas: lmer(L ~ SH_Temp + SH_Tide W + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Stress Hardening Temperature (°C)", 
       y = "Length (cm)")

ggsave(filename = "fig_output/DARKmodel_Mgigas_Length-SH_Temp.png", width = 5.10, height = 5.77, dpi = 300)

L.m6.DARKplot_bySHtide <- ggpredict(L.m6, terms = c("SH_Tide", "SH_Temp"))

plot(L.m6.DARKplot_bySHtide) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste("M. gigas: lmer(L ~ SH_Temp + SH_Tide + (1|Tank)")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = " ", 
       y = "Length (cm)")

ggsave(filename = "fig_output/DARKmodel_Mgigas_Length-Tide.png", width = 5.10, height = 5.77, dpi = 300)


#### WIDTHS ========

#### W.m_null: W ~ 1 ===============
W.m_null <- glm(W ~ 1, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(W.m_null)

# Call:
# glm(formula = W ~ 1, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
# -0.45973  -0.14048  -0.02873   0.11302   0.84427  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.846729   0.007355   251.1   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.04251883)
#
#Null deviance: 33.377  on 785  degrees of freedom
#Residual deviance: 33.377  on 785  degrees of freedom
#AIC: -248.47
#
#Number of Fisher Scoring iterations: 2

#### Fixed Factors: SH_Temp, SH_Tide, MHW ===============
#### W.m1: W ~ SH_Temp ===============
W.m1 <- glm(W ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(W.m1)

# Call:
#glm(formula = W ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.46241  -0.13705  -0.02823   0.11186   0.85295  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.83805    0.01040 176.755   <2e-16 ***
#  SH_Temp21    0.01737    0.01471   1.181    0.238    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.04249747)
#
#Null deviance: 33.377  on 785  degrees of freedom
#Residual deviance: 33.318  on 784  degrees of freedom
#AIC: -247.86
#
#Number of Fisher Scoring iterations: 2

#### W.m2: W ~ SH_Tide ===============
W.m2 <- glm(W ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(W.m2)

# Call:
#glm(formula = W ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4863  -0.1353  -0.0233   0.1132   0.8708  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.87334    0.01033 181.271  < 2e-16 ***
#  SH_TideTide -0.05310    0.01460  -3.638 0.000293 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.04186648)
#
#Null deviance: 33.377  on 785  degrees of freedom
#Residual deviance: 32.823  on 784  degrees of freedom
#AIC: -259.62
#
#Number of Fisher Scoring iterations: 2

#### W.m3: W ~ MHW ===============
W.m3 <- glm(W ~ MHW, family = gaussian(link = "identity"), data = Mgigas_LWend_stats)
summary(W.m3)

# Call:
#glm(formula = W ~ MHW, family = gaussian(link = "identity"), 
#    data = Mgigas_LWend_stats)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4707  -0.1355  -0.0254   0.1134   0.8204  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.83255    0.01453 126.079   <2e-16 ***
#  MHW18        0.03115    0.02074   1.502   0.1335    
#MHW21       -0.01129    0.02058  -0.549   0.5833    
#MHW24        0.03804    0.02071   1.836   0.0667 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.04225307)
#
#Null deviance: 33.377  on 785  degrees of freedom
#Residual deviance: 33.042  on 782  degrees of freedom
#AIC: -250.4
#
#Number of Fisher Scoring iterations: 2

#### W.m4: W ~ SH_Tide + (1|Tank) ===============
W.m4 <- lmer(W ~ SH_Tide +  (1|Tank), data = Mgigas_LWend_stats)
summary(W.m4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: W ~ SH_Tide + (1 | Tank)
#Data: Mgigas_LWend_stats
#
#REML criterion at convergence: -263.4
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.4636 -0.6745 -0.0964  0.5602  3.9888 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Tank     (Intercept) 0.001696 0.04118 
#Residual             0.040245 0.20061 
#Number of obs: 786, groups:  Tank, 20
#
#Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   1.87345    0.01369  36.14885 136.815  < 2e-16 ***
#  SH_TideTide  -0.05322    0.01431 765.36498  -3.718 0.000215 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SH_TideTide -0.524

#### AIC/BIC Scores ===============
AIC(W.m_null, W.m1, W.m2, W.m3, W.m4)
BIC(W.m_null, W.m1, W.m2, W.m3, W.m4)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(W.m4), resid(W.m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(W.m4))
qqline(resid(W.m4))

#### Density Plot of Residuals ===============
plot(density(resid(W.m4)))

#### Publication-ready table =============
## https://education.rstudio.com/blog/2020/07/gtsummary/
tbl.Wm4 <- tbl_regression(W.m4, exponentiate = TRUE) ## table!
tbl.Wm4
inline_text(tbl.Wm4,  variable = SH_Tide, level = "Tide") ##in-line text
# "0.95 (95% CI 0.92, 0.98; p<0.001)"