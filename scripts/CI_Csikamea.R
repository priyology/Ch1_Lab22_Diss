#### ~ C. SIKAMEA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)
library(sjPlot) ## manuscript-quality tables
library(lme4) ##glm
library(lmerTest) ##p-values

### load data sheet
Csikamea_CI_og <- read_csv("data/C_sikamea/CI_Csikamea.csv")
glimpse(Csikamea_CI_og)
summary(Csikamea_CI_og)
View(Csikamea_CI_og)

### new variable for new factors
Csikamea_CI1 <- Csikamea_CI_og

### change attributes about statistical factors
Csikamea_CI1$CI <- as.numeric(Csikamea_CI1$CI) ## make CI numeric
is.numeric(Csikamea_CI1$CI) ## True
Csikamea_CI1$SH_Temp <- as.character(Csikamea_CI1$SH_Temp) ## character
is.character(Csikamea_CI1$SH_Temp)
Csikamea_CI1$SH_Tide <- as.character(Csikamea_CI1$SH_Tide) ## character
is.character(Csikamea_CI1$SH_Tide)
Csikamea_CI1$MHW <- as.character(Csikamea_CI1$MHW) ## character
is.character(Csikamea_CI1$MHW)

#### Check for DEAD oysters =========
DEAD_Csikamea_CI <- filter(Csikamea_CI1, DEAD == "DEAD")
nrow(DEAD_Csikamea_CI)
glimpse(DEAD_Csikamea_CI)
summary(DEAD_Csikamea_CI)

### CSV of dead oysters
write_csv(DEAD_Csikamea_CI , file = "data/C_sikamea/CI_Csikamea_Dead.csv") #Dead Oysters

DEAD_count <- DEAD_Csikamea_CI %>% group_by(SH_Temp, SH_Tide, MHW) %>% 
  summarize("Numb_Dead" = n())
DEAD_count

## CSV of counts dead oysters (NOTE: going into data_output)
write_csv(DEAD_count, file = "data_output/C_sikamea/DEAD_Csikamea_CI.csv")


#### Omit NAs=========

#### now remove NAs from data sheet
colSums(is.na(Csikamea_CI1)) ## find NAs in each column, 93 under CI
Csikamea_CI2 <- Csikamea_CI1 %>% 
  filter(!is.na(CI)) # omit the 93 NAs in CI

colSums(is.na(Csikamea_CI2)) ## 0 NAs under CI

glimpse(Csikamea_CI2)
summary(Csikamea_CI2)

## CSV without NAs (empty cells / dead oysters)
write_csv(Csikamea_CI2 , file = "data/C_sikamea/Csikamea_CI_noNAs.csv")

#histogram of all CIs
ggplot(Csikamea_CI2, aes(CI)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "CI Histogram",
       x = "CI",
       y = "Counts of CI")

Csikamea_HighCI <- filter(Csikamea_CI2, CI > 10)
nrow(Csikamea_HighCI)

Csikamea_LowCI <- filter(Csikamea_CI2, CI < 0)
nrow(Csikamea_LowCI)

#Csikamea_CI2

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Csikamea_CI2 %>%
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_g = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_ALL

#### Grouped by MHW ====
Stats_MHW <- Csikamea_CI2 %>%
  group_by(MHW) %>% 
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_MHW

## plot
ggplot(Csikamea_CI2, aes(x=MHW, y=CI, fill = MHW)) + #factor MHW
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

#### Grouped by SH_Tide ====
Stats_SH_Tide <- Csikamea_CI2 %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_Tide

## plot
ggplot(Csikamea_CI2, aes(x=SH_Tide, y = CI, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)

#### Grouped by SH_Temp ====

Stats_SHTemp <- Csikamea_CI2 %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SHTemp

## plot
ggplot(Csikamea_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp ====
Stats_SH_TideTemp <- Csikamea_CI2 %>%
  group_by(SH_Tide, SH_Temp) %>% 
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_TideTemp

## plot
ggplot(Csikamea_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))
#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Csikamea_CI2 %>%
  group_by(SH_Tide, SH_Temp, MHW) %>% 
  summarize(
    Mean_Shell.g = mean(Shell.g),
    SD_Shell.g = sd(Shell.g),
    SE_Shell.g = SD_Shell.g/sqrt(n()),
    Mean_Tissue_mg = mean(Tissue.g),
    SD_Tissue.g = sd(Tissue.g),
    SE_Tissue.g = SD_Tissue.g/sqrt(n()),
    Mean_CI = mean(CI),
    SD_CI = sd(CI),
    SE_CI = SD_CI/sqrt(n()))

Stats_SH_TideTemp_MHW

View(Stats_SH_TideTemp_MHW)

ggplot(Csikamea_CI2, aes(x=MHW, y=CI, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title =expression(paste("Condition Indices of ", italic("C.sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Condition Index") #, fill = " Marine Heatwave")

ggsave(filename = "fig_output/Csikamea_CI.png",width = 5.10, height = 5.77, dpi = 300)

## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/C_sikamea/Csikamea_CI_MeanSDSE.csv")

#### c. SIKAMEA STATS ===============

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
summary(m.MHW.Tank) #model output
tab_model(m.MHW.Tank)
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
