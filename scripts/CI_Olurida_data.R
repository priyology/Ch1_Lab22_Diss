#### ~ O. LURIDA CONDITION INDEX ~ =====

## load libraries
library(tidyverse)

### load data sheet
Olurida_CI_og <- read_csv("data/O_lurida/CI_Olurida.csv")
glimpse(Olurida_CI_og)
summary(Olurida_CI_og)
View(Olurida_CI_og)

## setting these as.character/as.numeric

Olurida_CI_og$CI <- as.numeric(Olurida_CI_og$CI) ## make CI numeric
is.numeric(Olurida_CI_og$CI) ## True

Olurida_CI_og$Tank <- as.character(Olurida_CI_og$Tank) ## make SH_Temp a character
is.character(Olurida_CI_og$Tank) ## True

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

## same as previous CSV, but make another copy specifically for doing stats in case modifications needs to be made
write_csv(Olurida_CI2 , file = "data/O_lurida/Olurida_CI_StatsData.csv")

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
View(Stats_MHW)

## plot - CI
ggplot(Olurida_CI2, aes(x = MHW, y = CI, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

## plot - Shell
ggplot(Olurida_CI2, aes(x = MHW, y = Shell.mg, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

ggsave(filename = "fig_output/Olurida_Shell_SHTempMHW.png",width = 5.10, height = 5.77, dpi = 300)


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

#### Grouped by SH_Temp,MHW ====
Stats_SHTempMHW <- Olurida_CI2 %>%
  group_by(SH_Temp, MHW) %>% 
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

Stats_SHTempMHW
View(Stats_SHTempMHW)


## plot - CI
ggplot(Olurida_CI2, aes(x=SH_Temp, y=CI, fill = MHW)) +
  geom_boxplot() +
  #facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

ggsave(filename = "fig_output/Olurida_CI_SHTempMHW.png",width = 5.10, height = 5.77, dpi = 300)

## plot - Tissue
ggplot(Olurida_CI2, aes(x=SH_Temp, y=Tissue.mg, fill = MHW)) +
  geom_boxplot() +
  #facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

ggsave(filename = "fig_output/Olurida_Tissu_SHTempMHW.png",width = 5.10, height = 5.77, dpi = 300)

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

