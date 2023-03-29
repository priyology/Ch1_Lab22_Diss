#### ~ C. SIKAMEA MORTALITY STATS ~ =====

library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### ** SH Mortality ** ===============

### load data sheet
Olurida_SH_Morts <- read_csv("data/O_lurida/logit.mort_Olurida_EndSH.csv")
glimpse(Olurida_SH_Morts)
summary(Olurida_SH_Morts)
View(Olurida_SH_Morts)