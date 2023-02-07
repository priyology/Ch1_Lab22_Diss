#### ~ O. LURIDA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values
install.packages("emmeans")

### load data sheet
Olurida_CI_og <- read_csv("data/O_lurida/CI_Olurida.csv")
glimpse(Olurida_CI_og)
summary(Olurida_CI_og)
View(Olurida_CI_og)

## setting these as.character/as.numeric

Olurida_CI_og$CI <- as.numeric(Olurida_CI_og$CI) ## make CI numeric
is.numeric(Olurida_CI_og$CI) ## True

Olurida_CI_og$SH_Temp <- as.character(Olurida_CI_og$SH_Temp) ## make SH_Temp a character
is.character(Olurida_CI_og$SH_Temp) ## True

Olurida_CI_og$MHW <- as.character(Olurida_CI_og$MHW) ## make MHW is a character
is.character(Olurida_CI_og$MHW) ## True


### change attributes about statistical factors

#Olurida_CI_og$fSH_Temp <- as.factor(Olurida_CI_og$SH_Temp)
#is.factor(Olurida_CI_og$SH_Temp) ## True

#Olurida_CI_og$fSH_Tide <- as.factor(Olurida_CI_og$SH_Tide)
#is.factor(Olurida_CI_og$SH_Tide) ## True

#Olurida_CI_og$fMHW <- as.factor(Olurida_CI_og1$MHW)
#is.factor(Olurida_CI_og$MHW) ## True





#### remove Tank 1 b/c foil boats incinerated in muffle furnace =======
Olurida_CI1 <- filter(Olurida_CI_og, Tank > 1)
summary(Olurida_CI1)

### CSV of data with Tank 1 excluded
write_csv(Olurida_CI1, file = "data/O_lurida/CI_Olurida_noTank1.csv")

#### Check for DEAD oysters =========
DEAD_Olurida_CI <- filter(Olurida_CI1, Notes == "*DEAD*")
nrow(DEAD_Olurida_CI)
glimpse(DEAD_Olurida_CI)
summary(DEAD_Olurida_CI)

### CSV of dead oysters
write_csv(DEAD_Olurida_CI , file = "data/O_lurida/CI_Olurida_Dead.csv") #Dead Oysters

DEAD_count <- DEAD_Olurida_CI %>% group_by(SH_Temp, SH_Tide, MHW) %>% 
  summarize("Numb_Dead" = n())
DEAD_count

## CSV of counts dead oysters (NOTE: going into data_output)
write_csv(DEAD_count, file = "data_output/O_lurida/DEAD_Olurida_CI.csv")

#### Omit NAs=========

#### now remove NAs from data sheet
colSums(is.na(Olurida_CI1)) ## find NAs in each column, 4 under CI, 754 under Notes

## omit Notes column which is all NAs / info
Olurida_CI2 <- Olurida_CI1 %>% 
  filter(!is.na(CI)) # omit the 93 NAs in CI
colSums(is.na(Olurida_CI2)) ## All NAs under CI

glimpse(Olurida_CI2)
summary(Olurida_CI2)

## CSV without NAs (empty cells / dead oysters)
write_csv(Olurida_CI2 , file = "data/O_lurida/Olurida_CI_noTank1_noNAs.csv")

#histogram of all CIs
ggplot(Olurida_CI2, aes(CI)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "CI Histogram",
       x = "CI",
       y = "Counts of CI")

Olurida_HighCI <- filter(Olurida_CI2, CI > 10)
nrow(Olurida_HighCI) #76

#### Outlier RESOLVED Feb 6, 2023 ========
## filter out CI > 50
#Olurida_CI.outliers <- filter(Olurida_CI2, CI < 0 | CI > 50)
#Olurida_CI.outliers
#View(Olurida_CI.outliers)

#Olurida_CI2 <- filter(Olurida_CI2, between(CI, 0, 50))
#View(Olurida_CI2)

#ggplot(Olurida_CI2, aes(x = MHW, y = CI)) + 
#  geom_point() +
#  theme_classic()

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Olurida_CI2 %>%
    summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_ALL

#### Grouped by MHW ====
Stats_MHW <- Olurida_CI2 %>%
  group_by(MHW) %>% 
  summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_MHW

## plot
ggplot(Olurida_CI2, aes(x = MHW, y = CI, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

#### Grouped by SH_Tide ====
Stats_SH_Tide <- Olurida_CI2 %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_Tide

## plot
ggplot(Olurida_CI2, aes(x=SH_Tide, y=CI, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)

#### Grouped by SH_Temp ====

Stats_SHTemp <- Olurida_CI2 %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SHTemp

## plot
ggplot(Olurida_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp ====
Stats_SH_TideTemp <- Olurida_CI2 %>%
  group_by(SH_Tide, SH_Temp) %>% 
  summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_TideTemp

## plot
ggplot(Olurida_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Olurida_CI2 %>%
  group_by(SH_Tide, SH_Temp, MHW) %>% 
  summarize(
    Mean_Shell.mg = mean(Shell.mg),
    SD_Shell.mg = sd(Shell.mg),
    SE_Shell.mg = SD_Shell.mg/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.mg),
    SD_Tissue.mg = sd(Tissue.mg),
    SE_Tissue.mg = SD_Tissue.mg/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_TideTemp_MHW

View(Stats_SH_TideTemp_MHW)

ggplot(Olurida_CI2, aes(x=MHW, y=CI, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title =expression(paste("Condition Indices of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Condition Index") #, fill = " Marine Heatwave")

ggsave(filename = "fig_output/Olurida_CI.png",width = 5.10, height = 5.77, dpi = 300)

## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/O_lurida/Olurida_CI_MeanSDSE.csv")

#### O. LURIDA STATS ===============

#### Gaussian Distribution ========

## Fixed Factors: SH_Temp, SH_Tide, MHW
m.SHtemp_gauss <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Olurida_CI2)
summary(m.SHtemp_gauss) #AIC: 4243.9
tab_model(m.SHtemp_gauss) # p = 0.098

m.SHtemp_gauss <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Olurida_CI2)
summary(m.SHtemp_gauss) #AIC: 4245
tab_model(m.SHtemp_gauss) # p = 0.202

m.MHW_gauss <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Olurida_CI2)
summary(m.MHW_gauss) #AIC: 4231.8
tab_model(m.MHW_gauss) #18: p = 0.001 | 21: p = 0.003 | p = 0.016 

m.SH_Temp.MHW_gauss <- glm(CI ~ SH_Temp * MHW, family = gaussian(link = "identity"), data = Olurida_CI2)
summary(m.SH_Temp.MHW_gauss) #AIC: 4231.7
tab_model(m.SH_Temp.MHW_gauss) # SH Temp 21: p = 0.097 | SH Temp 15 x MHW 18: p = 0.002 | SH Temp 15 x MHW 18: p = 0.099 | SH Temp 21 x MHW 24: p = 0.059


## Fixed + Random (1|Tank) Factors

#SH_Temp + (1|Tank)
m.SH_Temp.Tank_gauss <- lmer(CI ~ SH_Temp + (1|Tank), data = Olurida_CI2)
summary(m.SH_Temp.Tank_gauss) #model output
tab_model(m.SH_Temp.Tank_gauss) #p = 0.082
AIC(m.SH_Temp.Tank_gauss) #AIC: 4213.099

#MHW + (1|Tank)
m.MHW.Tank_gauss <- lmer(CI ~ MHW + (1|Tank), data = Olurida_CI2)
summary(m.MHW.Tank_gauss) #model output
tab_model(m.MHW.Tank_gauss) #18: p = 0.033
AIC(m.MHW.Tank_gauss) #AIC: 4211.142

#SH_Temp * MHW + (1|Tank)
m.SH_Temp.MHW.Tank_gauss <- lmer(CI ~ SH_Temp * MHW + (1|Tank), data = Olurida_CI2)
summary(m.SH_Temp.MHW.Tank_gauss) #model output
tab_model(m.SH_Temp.MHW.Tank_gauss)
AIC(m.SH_Temp.MHW.Tank_gauss) #AIC: 4207.719

AIC(m.SH_Temp.MHW.Tank_gauss, m.SH_Temp.Tank, m.MHW.Tank, m.SHtemp, m.SHtide, m.MHW) #AIC: 4207.719
#m.SH_Temp.MHW.TANK has LOWEST AIC: 4207.719

## Plot of Residuals
plot(fitted(m.SH_Temp.MHW.Tank_gauss), resid(m.SH_Temp.MHW.Tank_gauss))
abline(0,0)

#plot(m.all)

## Q-Q plot of Residuals
qqnorm(resid(m.SH_Temp.MHW.Tank_gauss))
qqline(resid(m.SH_Temp.MHW.Tank_gauss))

## Density Plot of Residuals
plot(density(resid(m.SH_Temp.MHW.Tank_gauss)))

#### Gamma Distribution ========

## Fixed Factors: SH_Temp, SH_Tide, MHW
m.SHtemp_gamma <- glm(CI ~ SH_Temp, family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.SHtemp_gamma) #AIC: 3779.8
tab_model(m.SHtemp_gamma) # p = 0.099

m.SHtide_gamma <- glm(CI ~ SH_Tide, family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.SHtide_gamma) #AIC: 3781.6
tab_model(m.SHtide_gamma) # p = 0.199

m.MHW_gamma <- glm(CI ~ MHW, family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.MHW_gamma) #AIC: 3756
tab_model(m.MHW_gamma) #18: p = 0.001 | 21: p = 0.001 | p = 0.007

m.SH_Temp.MHW_gamma <- glm(CI ~ SH_Temp * MHW, family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.SH_Temp.MHW_gamma) #AIC: 3750
tab_model(m.SH_Temp.MHW_gamma) # SH Temp 21: p = 0.043 | SH Temp 15 x MHW 18: p = 0.002 | SH Temp 15 x MHW 21: p = 0.080 | SH Temp 21 x MHW 24: p = 0.032

## Fixed + Random (1|Tank) Factors

#SH_Temp + (1|Tank)
m.SH_Temp.Tank_gamma <- glmer(CI ~ SH_Temp + (1|Tank), family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.SH_Temp.Tank_gamma) #model output
tab_model(m.SH_Temp.Tank_gamma) #p = 0.008
AIC(m.SH_Temp.Tank_gamma) #AIC: 3698.456

#MHW + (1|Tank)
m.MHW.Tank_gamma <- glmer(CI ~ MHW + (1|Tank), family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.MHW.Tank_gamma) #model output
tab_model(m.MHW.Tank_gamma) #18: p = 0.062
AIC(m.MHW.Tank_gamma) #AIC: 3706.057

#SH_Temp * MHW + (1|Tank)
m.SH_Temp.MHW.Tank_gamma <- glmer(CI ~ SH_Temp * MHW + (1|Tank), family = Gamma(link = "identity"), data = Olurida_CI2)
summary(m.SH_Temp.MHW.Tank_gamma) #model output
tab_model(m.SH_Temp.MHW.Tank_gamma) #SH Temp: p = 0.013, # SH Temp 15 x MHW 18: 0.013, SH Temp 21 x 0.019
AIC(m.SH_Temp.MHW.Tank_gamma) # AIC:3699.464
AIC(m.SH_Temp.MHW.Tank_gamma, m.SH_Temp.MHW_gamma, m.SH_Temp.Tank, m.MHW.Tank, m.SHtemp, m.SHtide, m.MHW) #AIC: 3699.464
#m.SH_Temp.MHW.TANK has LOWEST AIC: 3699.464

## Plot of Residuals
plot(fitted(m.SH_Temp.MHW.Tank_gamma), resid(m.SH_Temp.MHW.Tank_gamma))
abline(0,0)

#plot(m.all)

## Q-Q plot of Residuals
qqnorm(resid(m.SH_Temp.MHW.Tank_gamma))
qqline(resid(m.SH_Temp.MHW.Tank_gamma))

## Density Plot of Residuals
plot(density(resid(m.SH_Temp.MHW.Tank)))

### plot model ## DOESN'T WORK FOR CATEOGRICAL VARIABLES
#ggplot(Olurida_CI2, aes(x = MHW, y = CI)) + 
#  geom_point(color = "red", size = 6) +
#  geom_smooth(method = lm, level = 0.9) +
#  xlab("X") +
#  ylab("Y")

