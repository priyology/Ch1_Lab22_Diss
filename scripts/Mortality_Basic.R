#### Mortality Data, quick graphs

library(tidyverse)
library(ggdark)

#### Stress Hardening ==============

SH_mort <- read_csv("data/Mortality/SH_mortality.csv")
str(SH_mort)
head(SH_mort)
tail(SH_mort)


## Dealing with NAs
colSums(is.na(SH_mort)) # how many NAs per column, photo + notes 460
# SH_mort1 <- SH_mort %>% 
#  filter(!is.na(c("Photo, Notes")))

summarize(SH_mort)
View(SH_mort)

## Summarize Stress Hardening Morts
SH_mort_summ <- SH_mort %>% 
  group_by(Species, Temp, Tide) %>% 
  summarize(
    total = n_distinct(Mortality, na.rm = TRUE),
    mean = mean(Mortality, na.rm = TRUE))
SH_mort_summ

write_csv(SH_mort_summ, file = "data/Mortality/SH_Dead_Trtmt.csv")

## Graphs
ggplot(SH_mort_summ, aes(x = Tide, y = total, fill = Species)) +
geom_bar(stat = "identity") +
facet_grid(Temp ~ Species) +
labs(
    x = "Tide",
    y = "Mortality during Stress Hardening",
    title = "Oyster Mortality During Stress Hardening"
    ) +
dark_theme_classic()

#### MHW =============

MHW_mort <- read_csv("data/Mortality/MHW_Mortality.csv")
str(MHW_mort)
head(MHW_mort)
tail(MHW_mort)

###NAs
colSums(is.na(MHW_mort)) # how many NAs = 239, Notes
# SH_mort1 <- SH_mort %>% 
#  filter(!is.na("Notes"))

## Summarize Post-Marine Heatwave Morts
MHW_mort_summ <- MHW_mort %>% 
  group_by(Species, SH_Temp, SH_tide, MHW) %>% 
  summarize(
    total = n_distinct(Mortality, na.rm = TRUE),
    mean = mean(Mortality, na.rm = TRUE))

MHW_mort_summ

print(MHW_mort_summ, n = 48)

write_csv(MHW_mort_summ, file = "data/Mortality/MHW_Dead_Trtmt.csv")

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


#### O. lurida outplanting =========
Outplant_mort <- read_csv("data/O_lurida/Mortality_Olurida_Outplanting.csv")
str(Outplant_mort)
Outplant_mort$SH_Temp <- as.character(Outplant_mort$SH_Temp) #make a character
head(Outplant_mort)
tail(Outplant_mort)

colSums(is.na(Outplant_mort)) # how many NAs = 0

## Graphs

outplantsurvival.plot <- ggplot(Outplant_mort, aes(x = SH_Temp, y = Survival_Numb, fill = SH_Temp)) +
  geom_bar(stat = "identity") +
  facet_wrap(~SH_Tide) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Temperature (\u00B0C)",
    y = "Number Survived",
    title = "Olympia Oyster Survival 279 Days After Outplanting"
  ) +
  dark_theme_classic()

outplantsurvival.plot


#### ~ SH_Mortality by Species ~ =====

### load data sheet
SH_Morts <- read_csv("data/Mortality/SH_Mortality.csv")
glimpse(SH_Morts)
summary(SH_Morts)
View(SH_Morts)

### change attributes about statistical factors
SH_Morts$SH_Temp <- as.factor(SH_Morts$SH_Temp) ## factor
is.factor(SH_Morts$SH_Temp) ## TRUE
SH_Morts$SH_Tide <- as.factor(SH_Morts$SH_Tide) ## factor
is.factor(SH_Morts$SH_Tide) ## TRUE
#SH_Morts$MHW <- as.factor(SH_Morts$MHW) ## character
#is.factor(SH_Morts$MHW) ## TRUE

#species labels
unique(SH_Morts$Species)


#### Ostrea lurida SH Mortality ===========
Olurida_SHmorts <- SH_Morts %>% 
  filter(Species == "O_lurida")

Olurida_SHmorts

### CSV of O.lurida stress hardening morts
write_csv(Olurida_SHmorts, file = "data/O_lurida/Olurida_SHmorts.csv")


#### Crassostrea sikamea SH Mortality ===========
Csikamea_SHmorts <- SH_Morts %>% 
  filter(Species == "C_sikamea")

Csikamea_SHmorts

### CSV of C. sikamea stress hardening morts
write_csv(Csikamea_SHmorts, file = "data/C_sikamea/Csikamea_SHmorts.csv")

#### Magallana gigas SH Mortality ===========
Mgigas_SHmorts <- SH_Morts %>% 
  filter(Species == "M_gigas")

Mgigas_SHmorts

### CSV of M,. gigas stress hardening morts
write_csv(Mgigas_SHmorts, file = "data/M_gigas/Mgigas_SHmorts.csv")


#### ~ MHW_Mortality by Species ~ =====

### load data sheet
MHW_Morts <- read_csv("data/Mortality/MHW_Mortality.csv")
glimpse(MHW_Morts)
summary(MHW_Morts)
View(MHW_Morts)

### change attributes about statistical factors
MHW_Morts$SH_Temp <- as.factor(MHW_Morts$SH_Temp) ## factor
is.factor(MHW_Morts$SH_Temp) ## TRUE
MHW_Morts$SH_Tide <- as.factor(MHW_Morts$SH_Tide) ## factor
is.factor(MHW_Morts$SH_Tide) ## TRUE
MHW_Morts$MHW <- as.factor(MHW_Morts$MHW) ## character
is.factor(MHW_Morts$MHW) ## TRUE

#species labels
unique(MHW_Morts$Species)


#### Ostrea lurida MHW Mortality ===========
Olurida_MHWmorts <- MHW_Morts %>% 
  filter(Species == "O_lurida")

Olurida_MHWmorts

### CSV of O.lurida post-MHW morts
write_csv(Olurida_MHWmorts, file = "data/O_lurida/Olurida_MHWmorts.csv")


#### Crassostrea sikamea MHW Mortality ===========
Csikamea_MHWmorts <- MHW_Morts %>% 
  filter(Species == "C_sikamea")

Csikamea_MHWmorts

### CSV of C.sikamea post-MHW morts
write_csv(Csikamea_MHWmorts, file = "data/C_sikamea/Csikamea_MHWmorts.csv")

#### Magallana gigas MHW Mortality ===========
Mgigas_MHWmorts <- MHW_Morts %>% 
  filter(Species == "M_gigas")

Mgigas_MHWmorts

### CSV of M. gigas post-MHW morts
write_csv(Mgigas_MHWmorts, file = "data/M_gigas/Mgigas_MHWmorts.csv")


#### Mortality Stack Barplot ==============
All_Morts <- read_csv("data/Mortality/StackedBar_Morts.csv")
glimpse(All_Morts)

Plot_Morts.Plot <- All_Morts %>% 
  select(Species, SH_Morts, MHW_CI_Morts) %>%
  pivot_longer(cols=c('SH_Morts', 'MHW_CI_Morts'),
               names_to='Timepoint',
               values_to='Morts') %>% 
  ggplot(aes(y = Morts, x = Species, fill = Timepoint)) + 
  geom_bar(position='stack', stat='identity') +
  theme_classic() +
  scale_fill_manual(values=c("#FB9A99", "#A6CEE3")) + #, direction = -1)
  labs(title = "Total Mortality During Experiment",
      subtitle = "Mortality out of 1,600 oysters per species",
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

Plot_Morts.Plot

ggsave(filename = "fig_output/AllMorts_BarPlot.png",width = 5.10, height = 5.77, dpi = 300)

library(ggdark)
DARKPlot_Morts.Plot <- All_Morts %>% 
  select(Species, SH_Morts, MHW_CI_Morts) %>%
  pivot_longer(cols=c('SH_Morts', 'MHW_CI_Morts'),
               names_to='Timepoint',
               values_to='Morts') %>% 
  ggplot(aes(y = Morts, x = Species, fill = Timepoint)) + 
  geom_bar(position='stack', stat='identity') +
  dark_theme_classic() +
  scale_fill_manual(values=c("#FB9A99", "#A6CEE3")) + #, direction = -1)
  labs(title = "Total Mortality During Experiment",
       subtitle = "Mortality out of 1,600 oysters per species",
       x = "Stress Hardening Temp (°C)", 
       y = "Length")

DARKPlot_Morts.Plot
ggsave(filename = "fig_output/DARKAllMorts_BarPlot.png",width = 5.10, height = 5.77, dpi = 300)


#### Hex Codes for Color Brewer Palettes =====

library("RColorBrewer")
brewer.pal(11, "RdYlBu")

#[1] "#A50026" "#D73027" "#F46D43" "#FDAE61"
#[5] "#FEE090" "#FFFFBF" "#E0F3F8" "#ABD9E9"
#[9] "#74ADD1" "#4575B4" "#313695"

brewer.pal(12, "Paired")

#[1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C"
#[5] "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
#[9] "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"

#### PowerPoint Editing Prep: Outplanting Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
OutplantingSurvival_fig <- read_pptx()
OutplantingSurvival_fig <- add_slide(OutplantingSurvival_fig, layout = "Title and Content", master = "Office Theme")
OutplantingSurvival_fig <- ph_with(x = OutplantingSurvival_fig, value = outplantsurvival.plot, location = ph_location_fullsize() )
OutplantingSurvival_fig <- ph_with(x = OutplantingSurvival_fig, "O. lurida Survival", location = ph_location_type(type = "title") )
print(OutplantingSurvival_fig, target = "presentations/OluridaOutplantingSurvival.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = outplantsurvival.plot) # Saving the plot to the temporary file

## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

## once in PPT, right-click on image and select "edit picture" to futz with individual components of the graph


