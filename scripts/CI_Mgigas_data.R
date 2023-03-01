#### ~ M GIGAS CONDITION INDEX ~ =====

## load libraries
library(tidyverse)

### load data sheet
Mgigas_CI_og <- read_csv("data/M_gigas/CI_Mgigas.csv")
glimpse(Mgigas_CI_og)
summary(Mgigas_CI_og)
View(Mgigas_CI_og)

### new variable for new factors
Mgigas_CI1 <- Mgigas_CI_og

### change attributes about statistical factors
Mgigas_CI1$CI <- as.numeric(Mgigas_CI1$CI) ## make CI numeric
is.numeric(Mgigas_CI1$CI) ## True
Mgigas_CI1$SH_Temp <- as.character(Mgigas_CI1$SH_Temp) ## character
is.character(Mgigas_CI1$SH_Temp)
Mgigas_CI1$SH_Tide <- as.character(Mgigas_CI1$SH_Tide) ## character
is.character(Mgigas_CI1$SH_Tide)
Mgigas_CI1$MHW <- as.character(Mgigas_CI1$MHW) ## character
is.character(Mgigas_CI1$MHW)

#### Check for DEAD oysters =========
DEAD_Mgigas_CI <- filter(Mgigas_CI1, DEAD == "DEAD")
nrow(DEAD_Mgigas_CI)
glimpse(DEAD_Mgigas_CI)
summary(DEAD_Mgigas_CI)

### CSV of dead oysters
write_csv(DEAD_Mgigas_CI , file = "data/M_gigas/CI_Mgigas_Dead.csv") #Dead Oysters

DEAD_count <- DEAD_Mgigas_CI %>% group_by(SH_Temp, SH_Tide, MHW) %>% 
  summarize("Numb_Dead" = n())
DEAD_count

## CSV of counts dead oysters (NOTE: going into data_output)
write_csv(DEAD_count, file = "data_output/M_gigas/DEAD_Mgigas_CI.csv")


#### Omit NAs=========

#### now remove NAs from data sheet
colSums(is.na(Mgigas_CI1)) ## find NAs in each column, 20 under CI
Mgigas_CI2 <- Mgigas_CI1 %>% 
  filter(!is.na(CI)) # omit the 20 NAs in CI

colSums(is.na(Mgigas_CI2)) ## 0 NAs under CI

glimpse(Mgigas_CI2)
summary(Mgigas_CI2)

## CSV without NAs (empty cells / dead oysters)
write_csv(Mgigas_CI2 , file = "data/M_gigas/Mgigas_CI_noNAs.csv")

#histogram of all CIs
ggplot(Mgigas_CI2, aes(CI)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "CI Histogram",
       x = "CI",
       y = "Counts of CI")

Mgigas_HighCI <- filter(Mgigas_CI2, CI > 10)
nrow(Mgigas_HighCI)

Mgigas_LowCI <- filter(Mgigas_CI2, CI < 0)
nrow(Mgigas_LowCI)

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Mgigas_CI2 %>%
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
Stats_MHW <- Mgigas_CI2 %>%
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
ggplot(Mgigas_CI2, aes(x=MHW, y=CI, fill = MHW)) + #factor MHW
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

#### Grouped by SH_Tide ====
Stats_SH_Tide <- Mgigas_CI2 %>%
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
ggplot(Mgigas_CI2, aes(x=SH_Tide, y = CI, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)

#### Grouped by SH_Temp ====

Stats_SHTemp <- Mgigas_CI2 %>%
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
ggplot(Mgigas_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))

#### Grouped by SH_Tide, SH_Temp ====
Stats_SH_TideTemp <- Mgigas_CI2 %>%
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
ggplot(Mgigas_CI2, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))
#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Mgigas_CI2 %>%
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

ggplot(Mgigas_CI2, aes(x=MHW, y=CI, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  #ylim(0,10) + # "Removed one row containing non-finite values"
  labs(title =expression(paste("Condition Indices of ", italic("M. gigas"))), 
       x = "Marine Heatwave (°C)", 
       y = "Condition Index") #, fill = " Marine Heatwave")

ggsave(filename = "fig_output/Mgigas_CI.png",width = 5.10, height = 5.77, dpi = 300)

## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/M_gigas/Mgigas_CI_MeanSDSE.csv")

#### c. SIKAMEA STATS ===============

## All Factors

## Fixed Factors: SH_Temp, SH_Tide, MHW
m.SHtemp <- glm(CI ~ SH_Temp, family = gaussian(link = "identity"), data = Mgigas_CI2)
summary(m.SHtemp) #AIC: 2635.6
tab_model(m.SHtemp)

m.SHtide <- glm(CI ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas_CI2)
summary(m.SHtide) #AIC: 2621.4
tab_model(m.SHtide)

m.MHW <- glm(CI ~ MHW, family = gaussian(link = "identity"), data = Mgigas_CI2)
summary(m.MHW) #AIC: 2616.4
tab_model(m.MHW)

## Fixed + Random (Tank) Factors
m.SHtide.Tank <- lmer(CI ~ SH_Tide + (1|Tank), data = Mgigas_CI2)
summary(m.SHtide.Tank) #model output
tab_model(m.SHtide.Tank)
AIC(m.SHtide.Tank) #AIC: 2600.26

m.MHW.Tank <- lmer(CI ~ MHW + (1|Tank), data = Mgigas_CI2)
summary(m.MHW.Tank) #model output
tab_model(m.MHW.Tank)
AIC(m.MHW.Tank) # AIC: 2615.642

m.SHtide.MHW.Tank <- lmer(CI ~ SH_Tide * MHW + (1|Tank), data = Mgigas_CI2)
summary(m.SHtide.MHW.Tank) #model output
tab_model(m.SHtide.MHW.Tank)
AIC(m.SHtide.MHW.Tank) # AIC: 2606.973

AIC(m.SHtide.MHW.Tank, m.SHtide.Tank, m.MHW.Tank, m.SHtemp, m.SHtide, m.MHW)
## m.SHtide.Tank has lowest AIC (2600.260)

## Plot of Residuals
plot(fitted(m.SHtide.Tank), resid(m.SHtide.Tank))
abline(0,0)

#plot(m.all)

## Q-Q plot of Residuals
qqnorm(resid(m.SHtide.Tank))
qqline(resid(m.SHtide.Tank)) 

## Density Plot of Residuals
plot(density(resid(m.SHtide.Tank)))
