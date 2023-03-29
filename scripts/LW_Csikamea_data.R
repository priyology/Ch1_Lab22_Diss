#### ~ C. SIKAMEA LENGTH & WIDTH DATA ~ =====

## load libraries
library(tidyverse)
library(cowplot)


#### DAY 1 =====

### load data sheet
Csikamea_LWd1_og <- read_csv("data/C_sikamea/LW_Day1_Csikamea.csv")
glimpse(Csikamea_LWd1_og)
summary(Csikamea_LWd1_og)
View(Csikamea_LWd1_og)

#### Put Length(L) & Width(W) in its own columns ==========
Csikamea_LWd1 <- Csikamea_LWd1_og %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

Csikamea_LWd1


#### Omit NAs=========

colSums(is.na(Csikamea_LWd1)) ## find NAs in each column, 400 under Date, which isn't necessary for analysis

#### LENGTHS histogram ====
ggplot(Csikamea_LWd1, aes(L)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "C. sikamea Lengths Histogram",
       x = "L",
       y = "Counts of L")

#### WIDTHS histogram ====
ggplot(Csikamea_LWd1, aes(W)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "C. sikamea Widths Histogram",
       x = "W",
       y = "Counts of W")

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Csikamea_LWd1 %>%
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_ALL

## create datasheet

write_csv(Csikamea_LWd1, "data/C_sikamea/LWd1_stats.csv")

#### END MHW =====

### load data sheet
Csikamea_LWend_og <- read_csv("data/C_sikamea/LW_EndMHW_Csikamea.csv")
glimpse(Csikamea_LWend_og)
summary(Csikamea_LWend_og)
View(Csikamea_LWend_og)

## setting these SH_Temp, MHW, Tank as.character

Csikamea_LWend_og$SH_Temp <- as.character(Csikamea_LWend_og$SH_Temp) ## make SH_Temp a character
is.character(Csikamea_LWend_og$SH_Temp) ## True

Csikamea_LWend_og$MHW <- as.character(Csikamea_LWend_og$MHW) ## make MHW is a character
is.character(Csikamea_LWend_og$MHW) ## True

Csikamea_LWend_og$Tank <- as.character(Csikamea_LWend_og$Tank) ## make SH_Temp a character
is.character(Csikamea_LWend_og$Tank) ## True


#### Omit NAs

colSums(is.na(Csikamea_LWend_og)) ## find NAs in each column, 400 under Date, 8 under Size_cm

## omit Size_mm column which is all NAs / info
Csikamea_LWend.1 <- Csikamea_LWend_og %>% 
  filter(!is.na(Size_cm)) # omit the 72 NAs in Size_cm
colSums(is.na(Csikamea_LWend.1)) ## All NAs under Size_cm

#### Put Length(L) & Width(W) in its own columns ==========
Csikamea_LWend.2 <- Csikamea_LWend.1 %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

glimpse(Csikamea_LWend.2)

#### LENGTHS histogram ====
ggplot(Csikamea_LWend.2, aes(L)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "C. sikamea Lengths Histogram",
       x = "L",
       y = "Counts of L")

#### WIDTHS histogram ====
ggplot(Csikamea_LWend.2, aes(W)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "C. sikamea Widths Histogram",
       x = "W",
       y = "Counts of W")

## create datasheet

write_csv(Csikamea_LWend.2, "data/C_sikamea/LW_EndMHW_stats.csv")

#### Figures: Mean, SD, SE =====

#### No Grouping ====
EndStats_ALL <- Csikamea_LWend.2 %>%
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

EndStats_ALL

#### Grouped by SH_Temp ====

Stats_SHTemp <- Csikamea_LWend.2 %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SHTemp

## L plot
L.Temp <- ggplot(Csikamea_LWend.2, aes(x=SH_Temp, y=L, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

## W plot
W.Temp <- ggplot(Csikamea_LWend.2, aes(x=SH_Temp, y=W, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Width")

plot_grid(L.Temp, W.Temp, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide ====

Stats_SHTide <- Csikamea_LWend.2 %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SHTide

## L plot
L.Tide <- ggplot(Csikamea_LWend.2, aes(x=SH_Tide, y=L, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Tidal Regime", 
       y = "Length")

## W plot
W.Tide <- ggplot(Csikamea_LWend.2, aes(x=SH_Tide, y=W, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Tidal Regime", 
       y = "Width")

plot_grid(L.Tide, W.Tide, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_Tide.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by MHW ====

Stats_MHW <- Csikamea_LWend.2 %>%
  group_by(MHW) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_MHW

## L plot
L.MHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.MHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.MHW, W.MHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_MHW.png",width = 5.10, height = 5.77, dpi = 300)


#### Grouped by SH_Temp, SH_Tide ====
Stats_SH_TempTide <- Csikamea_LWend.2 %>%
  group_by(SH_Temp, SH_Tide) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SH_TempTide

## L plot
L.TempTide <- ggplot(Csikamea_LWend.2, aes(x = SH_Temp, y = L, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))   +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

## W plot
W.TempTide <- ggplot(Csikamea_LWend.2, aes(x = SH_Temp, y = W, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))  +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Width")

plot_grid(L.TempTide, W.TempTide, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_TempTide.png",width = 5.10, height = 5.77, dpi = 300)


#### Grouped by SH_Temp, MHW ====
Stats_SHTempMHW <- Csikamea_LWend.2 %>%
  group_by(SH_Temp, MHW) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SHTempMHW

## L plot
L.SHTempMHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)   +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SHTempMHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

plot_grid(L.SHTempMHW, W.SHTempMHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_TempMHW.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide, MHW ====
Stats_SHTideMHW <- Csikamea_LWend.2 %>%
  group_by(SH_Tide, MHW) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SHTideMHW

## L plot
L.SHTideMHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SHTideMHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.SHTideMHW, W.SHTideMHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_TideMHW.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Csikamea_LWend.2 %>%
  group_by(SH_Tide, SH_Temp, MHW) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_SH_TideTemp_MHW

## L plot
L.SH_TideTemp_MHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SH_TideTemp_MHW <- ggplot(Csikamea_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("C. sikamea"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.SH_TideTemp_MHW, W.SH_TideTemp_MHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Csikamea_LWendMHW_SH_TideTemp_MHW.png",width = 5.10, height = 5.77, dpi = 300)


## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/C_sikamea/Csikamea_LW_MeanSDSE.csv")
