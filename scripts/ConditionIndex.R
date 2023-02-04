#### CONDITION INDEX =========

## load libraries
library(tidyverse)

#### O. lurida Condition Index =====

### load data sheet
Olurida_CI_og <- read_csv("data/O_lurida/CI_Olurida.csv")
str(Olurida_CI_og)
View(Olurida_CI_og)

#### remove Tank 1 b/c foil boats incinerated in muffle furnace =======
Olurida_CI1 <- filter(Olurida_CI_og, Tank > 1)
str(Olurida_CI1) ## check data type
Olurida_CI1$CI <- as.numeric(Olurida_CI1$CI) ## make CI numeric
is.numeric(Olurida_CI1$CI) ## True
Olurida_CI1$MHW <- as.character(Olurida_CI1$MHW) ## make a column where MHW is a character
is.character(Olurida_CI1$MHW)
Olurida_CI1$SH_Temp <- as.character(Olurida_CI1$SH_Temp) ## make a column where SH_Temp is a character
is.character(Olurida_CI1$SH_Temp)

### CSV of data with Tank 1 excluded
write_csv(Olurida_CI1, file = "data/O_lurida/CI_Olurida_noTank1.csv")

#### Check for DEAD oysters =========
DEAD_Olurida_CI <- filter(Olurida_CI_og, Notes == "*DEAD*")
nrow(DEAD_Olurida_CI)
str(DEAD_Olurida_CI)

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
colSums(is.na(Olurida_CI2)) ## 4 NAs under CI
Olurida_CI3 <- na.omit(Olurida_CI2) # omit the 4 NAs in CI
colSums(is.na(Olurida_CI3)) ## all NAs omitted

str(Olurida_CI3)

## CSV without NAs (empty cells / dead oysters)
write_csv(Olurida_CI3 , file = "data/O_lurida/Olurida_CI_noTank1_noNAs.csv")

#### Remove Outliers; go back & check later ===========

#scatter plot of all CI
ggplot(Olurida_CI3, aes(x = MHW, y = CI)) + 
  geom_point() +
  theme_classic()

## filter out CI > 30
Olurida_CI.outliers <- filter(Olurida_CI3, CI < 0 | CI > 30)
Olurida_CI.outliers
View(Olurida_CI.outliers)
write_csv(Olurida_CI.outliers, file = "data_output/O_lurida/Olurida_CI.outliers.csv")

Olurida_CI4 <- filter(Olurida_CI3, between(CI, 0, 30))
View(Olurida_CI4)

ggplot(Olurida_CI4, aes(x = MHW, y = CI)) + 
  geom_point() +
  theme_classic()

#### Summary Statistics =====

#### No Grouping ====
Stats_ALL <- Olurida_CI4 %>%
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
Stats_MHW <- Olurida_CI4 %>%
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

## plot with more reasonable outliers
ggplot(Olurida_CI4, aes(x=MHW, y=CI, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

#### Grouped by SH_Tide ====
Stats_SH_Tide <- Olurida_CI4 %>%
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
ggplot(Olurida_CI4, aes(x=SH_Tide, y=CI, fill = SH_Tide)) +
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
ggplot(Olurida_CI4, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
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
ggplot(Olurida_CI4, aes(x=SH_Temp, y=CI, fill = SH_Temp)) +
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

ggplot(Olurida_CI4, aes(x=MHW, y=CI, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title =expression(paste("Condition Indices of ", italic("O. lurida"))), x = "Marine Heatwave (°C)", y = "Condition Index") #, fill = " Marine Heatwave")

## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/O_lurida/Olurida_CI_MeanSDSE.csv")

#### STATISTICAL ANALYSES ===============


