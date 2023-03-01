#### ~ C. SIKAMEA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(glmmTMB) ## to do model diagnostics w/ sjPlot
library(ggeffects) ## another model plotting option

#### c. SIKAMEA CONDITION INDEX STATS ===============

## All Factors

## Fixed Factors: SH_Temp, SH_Tide, MHW
m.SHtemp <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea_CI2)
summary(m.SHtemp) #AIC: 2427.5
tab_model(m.SHtemp)

m.SHtide <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea_CI2)
summary(m.SHtide) #AIC: 2433.9
tab_model(m.SHtide)

m.MHW <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Csikamea_CI2)
summary(m.MHW) #AIC: 2419.9
tab_model(m.MHW)

## Fixed + Random (Tank) Factors
m.SHtemp.Tank <- lmer(CI ~ SH_Temp + (1|Tank), data = Csikamea_CI2)
summary(m.SHtemp.Tank) #model output
tab_model(m.SHtemp.Tank)
AIC(m.SHtemp.Tank) #AIC: 2433.157

m.MHW.Tank <- lmer(CI ~ MHW + (1|Tank), data = Csikamea_CI2)
summary(m.MHW.Tank) #model output
tab_model(m.MHW.Tank)
AIC(m.MHW.Tank) # AIC: 2432.903

m.SHtemp.MHW.Tank <- lmer(CI ~ SH_Temp * MHW + (1|Tank), data = Csikamea_CI2)
summary(m.SHtemp.MHW.Tank) #model output
tab_model(m.SHtemp.MHW.Tank)
AIC(m.SHtemp.MHW.Tank) # AIC: 2438.098

AIC(m.SHtemp.MHW.Tank, m.SHtemp.Tank, m.MHW.Tank, m.SHtemp, m.SHtide, m.MHW)
## m.MHW.Tank has lowest AIC (2432.903)

## Plot of Residuals
plot(fitted(m.MHW.Tank), resid(m.MHW.Tank))
abline(0,0)

#plot(m.all)

## Q-Q plot of Residuals
qqnorm(resid(m.MHW.Tank))
qqline(resid(m.MHW.Tank)) 

## Density Plot of Residuals
plot(density(resid(m.MHW.Tank)))
