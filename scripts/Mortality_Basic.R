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


