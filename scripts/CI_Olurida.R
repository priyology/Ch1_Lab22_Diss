#### ~ O. LURIDA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot)
library(lme4)

### load data sheet
Olurida_CI_og <- read_csv("data/O_lurida/CI_Olurida.csv")
glimpse(Olurida_CI_og)
summary(Olurida_CI_og)
View(Olurida_CI_og)

### change attributes about statistical factors
Olurida_CI1$CI <- as.numeric(Olurida_CI1$CI) ## make CI numeric
is.numeric(Olurida_CI1$CI) ## True
Olurida_CI3$fSH_Temp <- as.factor(Olurida_CI3$SH_Temp)
is.factor(Olurida_CI3$fSH_Temp)
Olurida_CI3$fSH_Tide <- as.factor(Olurida_CI3$SH_Tide)
is.factor(Olurida_CI3$fSH_Tide)
Olurida_CI3$fMHW <- as.factor(Olurida_CI3$MHW)
is.factor(Olurida_CI3$fMHW)

## setting these as.character doesn't matter, since I've created factors above
#Olurida_CI1$MHW <- as.character(Olurida_CI1$MHW) ## make MHW is a character
#is.character(Olurida_CI1$MHW)
#Olurida_CI1$SH_Temp <- as.character(Olurida_CI1$SH_Temp) ## make SH_Temp a character
#is.character(Olurida_CI1$SH_Temp)


#### remove Tank 1 b/c foil boats incinerated in muffle furnace =======
Olurida_CI1 <- filter(Olurida_CI_og, Tank > 1)
summary(Olurida_CI1)


### CSV of data with Tank 1 excluded
write_csv(Olurida_CI1, file = "data/O_lurida/CI_Olurida_noTank1.csv")

#### Check for DEAD oysters =========
DEAD_Olurida_CI <- filter(Olurida_CI_og, Notes == "*DEAD*")
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
Olurida_CI2 <- Olurida_CI1 %>% select(-Notes) ## omit Notes column which is all NAs / info
colSums(is.na(Olurida_CI2)) ## 6 NAs under CI
Olurida_CI3 <- na.omit(Olurida_CI2) # omit the 6 NAs in CI
colSums(is.na(Olurida_CI3)) ## all NAs omitted

glimpse(Olurida_CI3)
summary(Olurida_CI3)

## CSV without NAs (empty cells / dead oysters)
write_csv(Olurida_CI3 , file = "data/O_lurida/Olurida_CI_noTank1_noNAs.csv")

#histogram of all CIs
ggplot(Olurida_CI3, aes(CI)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "CI Histogram",
       x = "CI",
       y = "Counts of CI")


Olurida_HighCI <- filter(Olurida_CI3, CI > 25)
nrow(Olurida_HighCI)

#### Outlier RESOLVED Feb 6, 2023 ========
## filter out CI > 50
#Olurida_CI.outliers <- filter(Olurida_CI3, CI < 0 | CI > 50)
#Olurida_CI.outliers
#View(Olurida_CI.outliers)

#Olurida_CI3 <- filter(Olurida_CI3, between(CI, 0, 50))
#View(Olurida_CI3)

#ggplot(Olurida_CI3, aes(x = MHW, y = CI)) + 
#  geom_point() +
#  theme_classic()

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Olurida_CI3 %>%
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
Stats_MHW <- Olurida_CI3 %>%
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
ggplot(Olurida_CI3, aes(x=fMHW, y=CI, fill = fMHW)) + #factor MHW
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

#### Grouped by SH_Tide ====
Stats_SH_Tide <- Olurida_CI3 %>%
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

## plot with more reasonable outliers
ggplot(Olurida_CI3, aes(x=fSH_Tide, y=CI, fill = fSH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)


#### Grouped by SH_Temp ====

Stats_SHTemp <- Olurida_CI3 %>%
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

Stats_SH_Temp

## plot with more reasonable outliers
ggplot(Olurida_CI3, aes(x=fSH_Temp, y=CI, fill = fSH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp ====
Stats_SH_TideTemp <- Olurida_CI3 %>%
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

## plot with more reasonable outliers
ggplot(Olurida_CI3, aes(x=fSH_Temp, y=CI, fill = fSH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Olurida_CI3 %>%
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

ggplot(Olurida_CI3, aes(x=fMHW, y=CI, fill = fMHW)) +
  geom_boxplot() +
  facet_grid(fSH_Tide ~ fSH_Temp) +
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


### 

## All Factors

## Fixed Factors: SH_Temp, SH_Tide, MHW
m.SHtemp <- glm(CI ~ fSH_Temp, family = gaussian(link = "identity"), data = Olurida_CI3)
summary(m.SHtemp) #AIC: 4225.4

m.SHtide <- glm(CI ~ fSH_Tide, family = gaussian(link = "identity"), data = Olurida_CI3)
summary(m.SHtide) #AIC: 4225.8

m.MHW <- glm(CI ~ fMHW, family = gaussian(link = "identity"), data = Olurida_CI3)
summary(m.MHW) #AIC: 4206.1


## Fixed + Random (Tank) Factors
m.all <- lmer(CI ~ fSH_Temp + fSH_Tide + fMHW + (1|Tank), data = Olurida_CI3)
summary(m.all) #model output
anova(m.all)
AIC(m.all, m.SHtemp, m.SHtide, m.MHW) #m.all has LOWEST AIC: 4186.888
tab_model(m.all)

## Plot of Residuals
plot(fitted(m.all), resid(m.all))
abline(0,0)

#plot(m.all)

## Q-Q plot of Residuals
qqnorm(resid(m.all))
qqline(resid(m.all)) 

## Density Plot of Residuals
plot(density(resid(m.all)))


