#### ~ O. LURIDA LENGTH & WIDTH DATA ~ =====

## load libraries
library(tidyverse)
library(cowplot)

#### DAY 1 =====

### load data sheet
Olurida_LWd1_og <- read_csv("data/O_lurida/LW_Day1_Olurida.csv")
glimpse(Olurida_LWd1_og)
summary(Olurida_LWd1_og)
View(Olurida_LWd1_og)

#### Put Length(L) & Width(W) in its own columns ==========
Olurida_LWd1 <- Olurida_LWd1_og %>%
  pivot_wider(names_from = L_W, values_from = Size_mm)

Olurida_LWd1
View(Olurida_LWd1)

#### Omit NAs=========

colSums(is.na(Olurida_LWd1)) ## find NAs in each column, 400 under Date

#### LENGTHS histogram ====
ggplot(Olurida_LWd1, aes(L)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "O. lurida Lengths Histogram",
       x = "L",
       y = "Counts of L")

#### WIDTHS histogram ====
ggplot(Olurida_LWd1, aes(W)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "O. lurida Widths Histogram",
       x = "W",
       y = "Counts of W")

#### Figures, Mean, SD, SE =====

#### No Grouping ====
Stats_ALL <- Olurida_LWd1 %>%
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_ALL

#### Grouped by Batch ====

Stats_MHW <- Olurida_LWd1 %>%
  group_by(Batch) %>% 
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

Stats_MHW

View(Stats_MHW)

## plot - L
L.plot <- ggplot(Olurida_LWd1, aes(x = Batch, y = L)) +
  geom_boxplot() +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)

## plot - W
W.plot <- ggplot(Olurida_LWd1, aes(x = Batch, y = W)) +
  geom_boxplot() +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)

plot_grid(L.plot, W.plot, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWd1.png",width = 5.10, height = 5.77, dpi = 300)


## create datasheet

write_csv(Olurida_LWd1, "data/O_lurida/LWd1_stats.csv")

#### models: difference between batches? ====
library(lme4) ##glm
Lm.null <- lm(L ~ 1, data = Olurida_LWd1)
summary(Lm.null)

# Call:
#  lm(formula = L ~ 1, data = Olurida_LWd1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5246 -0.8221 -0.0921  0.8662  3.5054 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.03060    0.06371   94.66   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.274 on 399 degrees of freedom
#
#Number of Fisher Scoring iterations: 2

Lm.1 <- lm(L ~ Batch, data = Olurida_LWd1)
summary(Lm.1)

# Call:
# lm(formula = L ~ Batch, data = Olurida_LWd1)
#
# Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.4447 -0.8495 -0.0642  0.9287  3.4255 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    5.95072    0.09003  66.096   <2e-16 ***
#  BatchB: 18/24  0.15976    0.12732   1.255     0.21    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.273 on 398 degrees of freedom
#Multiple R-squared:  0.00394,	Adjusted R-squared:  0.001438 
#F-statistic: 1.574 on 1 and 398 DF,  p-value: 0.2103

Wm.null <- glm(W ~ 1, data = Olurida_LWd1)
summary(Wm.null)

#Call:
#glm(formula = W ~ 1, data = Olurida_LWd1)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8224  -0.9564  -0.0394   0.8968   4.7286  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.93844    0.06483   76.18   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.68094)
#
#Null deviance: 670.7  on 399  degrees of freedom
#Residual deviance: 670.7  on 399  degrees of freedom
#AIC: 1345.9
#
#Number of Fisher Scoring iterations: 2

Wm.1 <- glm(W ~ Batch, data = Olurida_LWd1)
summary(Wm.1)

# Call:
#glm(formula = W ~ Batch, data = Olurida_LWd1)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8196  -0.9550  -0.0394   0.8997   4.7257  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.935555   0.091792  53.769   <2e-16 ***
#  BatchB: 18/24 0.005765   0.129814   0.044    0.965    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.685156)
#
#Null deviance: 670.70  on 399  degrees of freedom
#Residual deviance: 670.69  on 398  degrees of freedom
#AIC: 1347.9
#
#Number of Fisher Scoring iterations: 2

#### END MHW =====

### load data sheet
Olurida_LWend_og <- read_csv("data/O_lurida/LW_EndMHW_Olurida.csv")
glimpse(Olurida_LWend_og)
summary(Olurida_LWend_og)
View(Olurida_LWend_og)

## setting these SH_Temp, MHW, Tank as.character

Olurida_LWend_og$SH_Temp <- as.character(Olurida_LWend_og$SH_Temp) ## make SH_Temp a character
is.character(Olurida_LWend_og$SH_Temp) ## True

Olurida_LWend_og$MHW <- as.character(Olurida_LWend_og$MHW) ## make MHW is a character
is.character(Olurida_LWend_og$MHW) ## True

Olurida_LWend_og$Tank <- as.character(Olurida_LWend_og$Tank) ## make SH_Temp a character
is.character(Olurida_LWend_og$Tank) ## True


#### Omit NAs

colSums(is.na(Olurida_LWend_og)) ## find NAs in each column, 400 under Date, 8 under Size_mm

## omit Size_mm column which is all NAs / info
Olurida_LWend.1 <- Olurida_LWend_og %>% 
  filter(!is.na(Size_mm)) # omit the 8 NAs in Size_mm
colSums(is.na(Olurida_LWend.1)) ## All NAs under Size_mm

#### Put Length(L) & Width(W) in its own columns ==========
Olurida_LWend.2 <- Olurida_LWend.1 %>%
  pivot_wider(names_from = L_W, values_from = Size_mm)

glimpse(Olurida_LWend.2)

#### LENGTHS histogram ====
#ggplot(Olurida_LWend.2, aes(L)) +

#### Put Length(L) & Width(W) in its own columns ==========
Olurida_LWend.1 <- Olurida_LWend_og %>%
  pivot_wider(names_from = L_W, values_from = Size_mm)

Olurida_LWend.1

#### Omit NAs=========

colSums(is.na(Olurida_LWd1)) ## find NAs in each column, 400 under Date

#### LENGTHS histogram ====
ggplot(Olurida_LWend.2, aes(L)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "O. lurida Lengths Histogram",
       x = "L",
       y = "Counts of L")

#### WIDTHS histogram ====

#ggplot(Olurida_LWend.2, aes(W)) +

ggplot(Olurida_LWend.2, aes(W)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "O. lurida Widths Histogram",
       x = "W",
       y = "Counts of W")

## create datasheet

write_csv(Olurida_LWend.2, "data/O_lurida/LW_EndMHW_stats.csv")

#### Figures: Mean, SD, SE =====

#### No Grouping ====
EndStats_ALL <- Olurida_LWend.2 %>%
  summarize(
    Mean_L = mean(L),
    SD_L = sd(L),
    SE_L = SD_L/sqrt(n()),
    Mean_W = mean(W),
    SD_W = sd(W),
    SE_W = SD_W/sqrt(n()))

EndStats_ALL

#### Figures, Mean, SD, SE =====

#### Grouped by SH_Temp ====

Stats_SHTemp <- Olurida_LWend.2 %>%
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
L.Temp <- ggplot(Olurida_LWend.2, aes(x=SH_Temp, y=L, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

## W plot
W.Temp <- ggplot(Olurida_LWend.2, aes(x=SH_Temp, y=W, fill = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Width")

plot_grid(L.Temp, W.Temp, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_Temp.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide ====

Stats_SHTide <- Olurida_LWend.2 %>%
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
L.Tide <- ggplot(Olurida_LWend.2, aes(x=SH_Tide, y=L, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Tidal Regime", 
       y = "Length")

## W plot
W.Tide <- ggplot(Olurida_LWend.2, aes(x=SH_Tide, y=W, fill = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Tidal Regime", 
       y = "Width")

plot_grid(L.Tide, W.Tide, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_Tide.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by MHW ====

Stats_MHW <- Olurida_LWend.2 %>%
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
L.MHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.MHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.MHW, W.MHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_MHW.png",width = 5.10, height = 5.77, dpi = 300)


#### Grouped by SH_Temp, SH_Tide ====
Stats_SH_TempTide <- Olurida_LWend.2 %>%
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
L.TempTide <- ggplot(Olurida_LWend.2, aes(x = SH_Temp, y = L, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))   +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

## W plot
W.TempTide <- ggplot(Olurida_LWend.2, aes(x = SH_Temp, y = W, fill = SH_Temp)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_manual(values=c("#4575B4", "#FDAE61"))  +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Stress Hardening Temp (°C)", 
       y = "Width")

plot_grid(L.TempTide, W.TempTide, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_TempTide.png",width = 5.10, height = 5.77, dpi = 300)


#### Grouped by SH_Temp, MHW ====
Stats_SHTempMHW <- Olurida_LWend.2 %>%
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
L.SHTempMHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)   +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SHTempMHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) 

plot_grid(L.SHTempMHW, W.SHTempMHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_TempMHW.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide, MHW ====
Stats_SHTideMHW <- Olurida_LWend.2 %>%
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
L.SHTideMHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SHTideMHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_wrap(~SH_Tide) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.SHTideMHW, W.SHTideMHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_TideMHW.png",width = 5.10, height = 5.77, dpi = 300)

#### Grouped by SH_Tide, SH_Temp, MHW ====
Stats_SH_TideTemp_MHW <- Olurida_LWend.2 %>%
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
L.SH_TideTemp_MHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = L, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Lengths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Length")

## W plot
W.SH_TideTemp_MHW <- ggplot(Olurida_LWend.2, aes(x = MHW, y = W, fill = MHW)) +
  geom_boxplot() +
  facet_grid(SH_Tide ~ SH_Temp) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title =expression(paste("Widths of ", italic("O. lurida"))), 
       x = "Marine Heatwave (°C)", 
       y = "Width")

plot_grid(L.SH_TideTemp_MHW, W.SH_TideTemp_MHW, labels = c('L', 'W'), label_size = 12)
ggsave(filename = "fig_output/Olurida_LWendMHW_SH_TideTemp_MHW.png",width = 5.10, height = 5.77, dpi = 300)


## CSV for Mean, SD, SE: Stats_SH_TideTemp_MHW (folder: data_output)
write_csv(Stats_SH_TideTemp_MHW , file = "data_output/O_lurida/Olurida_LW_MeanSDSE.csv")
