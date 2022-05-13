#### Mortality Data, quick graphs

library(tidyverse)
library(ggdark)

### Stress Hardening

SH_mort <- read_csv("data/SH_mortality.csv")
str(SH_mort)
head(SH_mort)
tail(SH_mort)


## Dealing with NAs
is.na(SH_mort) #are there NAs = yes
sum(is.na(SH_mort)) # how many NAs = 858
sapply(SH_mort, function(x) sum(is.na(x))) #NAs by columns

summarize(SH_mort)
View(SH_mort)

## Summarize Stress Hardening Morts
SH_mort_summ <- SH_mort %>% 
  group_by(Species, Temp, Tide) %>% 
  summarize(
    total = n_distinct(Mortality, na.rm = TRUE),
    mean = mean(Mortality, na.rm = TRUE))
SH_mort_summ

## Graphs
ggplot(SH_mort_summ, aes(x = Tide, y = total, fill = Species)) +
geom_bar(stat = "identity") +
facet_grid(Temp ~ Species) +
labs(
    x = "Tide",
    y = "Mortality during Stress Hardening",
    title = "Oyster Mortality During Stress Hardening"
    ) +
theme_classic()

### MHW

MHW_mort <- read_csv("data/MHW_Mortality.csv")
str(MHW_mort)
head(MHW_mort)
tail(MHW_mort)

is.na(MHW_mort) #are there NAs = yes
sum(is.na(MHW_mort)) # how many NAs = 478
sapply(MHW_mort, function(x) sum(is.na(x))) #NAs by columns

## Summarize Post-Marine Heatwave Morts
MHW_mort_summ <- MHW_mort %>% 
  group_by(Species, SH_Temp, SH_tide, MHW) %>% 
  summarize(
    total = n_distinct(Mortality, na.rm = TRUE),
    mean = mean(Mortality, na.rm = TRUE))
MHW_mort_summ

## Graphs
ggplot(MHW_mort_summ, aes(x = SH_tide, y = total, fill = Species, group = SH_Temp)) +
  geom_bar(stat = "identity") +
  facet_grid(MHW ~ Species) +
  labs(
    x = "Tide",
    y = "Mortality during Stress Hardening",
    title = "Oyster Mortality After Marine Heatwaves"
  ) +
  theme_classic()
