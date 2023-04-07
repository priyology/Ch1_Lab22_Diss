#### ~ LW Figure ~ =====

## load libraries
library(tidyverse)
library(ggdark)
library(ggpattern)
library(cowplot)

## load data
L.summary <- read_csv("data/LW_Summary.csv")

## SH_Temp & MHW as characters
L.summary$SH_Temp <- as.character(L.summary$SH_Temp)
  is.character(L.summary$SH_Temp)

L.summary$MHW <- as.character(L.summary$MHW)
is.character(L.summary$MHW)

L.summary

#### M. gigas =====
L.gigas <- L.summary %>% 
  filter(Species == "Magallana gigas")

## Plot
Plot.gigas <- ggplot(L.gigas, aes(x = SH_Temp, y = Mean_L, fill = MHW)) +
  facet_wrap(~ SH_Tide) + #
  geom_bar(position = position_dodge(), stat="identity") +
  #geom_col_pattern(position = "dodge",
                  # pattern = 
                  #   c(
                      # "none", "none", "none", "none", # 15°C + No Tide
                      # "none", "none", "none", "none", # 21°C + No Tide
                      # "stripe", "stripe", "stripe", "stripe", # 15°C + Tide
                      # "stripe", "stripe", "stripe", "stripe" # 21°C + Tide
                    # ),
                  # pattern_angle = c(rep(0, 3), 
                                    # rep(45, 3), 
                                    # rep(0, 6)),
                   #pattern_density = .2,
                   #pattern_spacing = .04,
                   #pattern_fill = 'white') +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  geom_errorbar(aes(ymin = Mean_L - SE, ymax = Mean_L + SE),
                width = .2,            # Width of the error bars
                position = position_dodge(0.9)) +
  dark_theme_classic() +
  labs(subtitle = expression(paste(italic("Magallana gigas"))),
       x = "Stress Hardening Temperature (°C)", 
       y = expression("Shell Length (mm)")) +
  theme(plot.subtitle = element_text(vjust = 1.5),
         legend.position = 'top') #+ #, 
         #strip.background = element_blank(),
         #strip.text.x = element_blank()
  #guides(fill = guide_legend(override.aes = 
                              # list(
                                # pattern = c("none", "none", "none", "none"))))

Plot.gigas



#### ** PPT: Plot.gigas ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
L.gigas_figs <- read_pptx()
L.gigas_figs <- add_slide(L.gigas_figs, layout = "Title and Content", master = "Office Theme")
L.gigas_figs <- ph_with(x = L.gigas_figs, value = Plot.gigas, location = ph_location_fullsize() )
L.gigas_figs <- ph_with(x = L.gigas_figs, "M.gigas_Length", location = ph_location_type(type = "title") )
print(L.gigas_figs, target = "presentations/L.gigas_figs.pptx")

#### R2PPT / RDCOMClient =====
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type = "win.binary")
library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.gigas) # Saving the plot to the temporary file

## Step 2: Open a blank PPT slide
#devtools::install_github('omegahat/RDCOMClient')
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

## once in PPT, right-click on image and select "edit picture" to futz with individual components of the graph


#### C. sikamea =====
L.sikamea <- L.summary %>% 
  filter(Species == "Crassostrea sikamea")

## Plot
Plot.sikamea <- ggplot(L.sikamea, aes(x = SH_Temp, y = Mean_L, fill = MHW)) +
  facet_wrap(~ SH_Tide) + #
  geom_bar(position = position_dodge(), stat="identity") +
  #geom_col_pattern(position = "dodge",
                  # pattern = 
                  #   c(
                      # "none", "none", "none", "none", # 15°C + No Tide
                      # "none", "none", "none", "none", # 21°C + No Tide
                      # "stripe", "stripe", "stripe", "stripe", # 15°C + Tide
                      # "stripe", "stripe", "stripe", "stripe" # 21°C + Tide
                    # ),
                  # pattern_angle = c(rep(0, 3), 
                                    # rep(45, 3), 
                                    # rep(0, 6)),
                   #pattern_density = .2,
                   #pattern_spacing = .04,
                   #pattern_fill = 'white') +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  geom_errorbar(aes(ymin = Mean_L - SE, ymax = Mean_L + SE),
                width = .2,            # Width of the error bars
                position = position_dodge(0.9)) +
  dark_theme_classic() +
  labs(subtitle = expression(paste(italic("Crassostrea sikamea"))),
       x = "Stress Hardening Temperature (°C)", 
       y = expression("Shell Length (mm)")) +
  theme( plot.subtitle = element_text(hjust = 1.0),
         legend.position = 'none', 
         strip.background = element_blank(),
         strip.text.x = element_blank()
  ) # +
  #guides(fill = guide_legend(override.aes = 
                              # list(
                                # pattern = c("none", "none", "none", "none"))))

Plot.sikamea

#### ** PPT: Plot.sikamea ==============


#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
L.sikamea_figs <- read_pptx()
L.sikamea_figs <- add_slide(L.sikamea_figs, layout = "Title and Content", master = "Office Theme")
L.sikamea_figs <- ph_with(x = L.sikamea_figs, value = Plot.sikamea, location = ph_location_fullsize() )
L.sikamea_figs <- ph_with(x = L.sikamea_figs, "C. sikamea Length", location = ph_location_type(type = "title") )
print(L.sikamea_figs, target = "presentations/L.sikamea_figs.pptx")

#### R2PPT / RDCOMClient =====
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type = "win.binary")
library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.sikamea) # Saving the plot to the temporary file

## Step 2: Open a blank PPT slide
# devtools::install_github('omegahat/RDCOMClient')
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)


#### O. lurida ==============

L.lurida <- L.summary %>% 
  filter(Species == "Ostrea lurida")


## Plot
Plot.lurida <- ggplot(L.lurida, aes(x = SH_Temp, y = Mean_L, fill = MHW)) +
  facet_wrap(~ SH_Tide) + #
  geom_bar(position = position_dodge(), stat="identity") +
  #geom_col_pattern(position = "dodge",
  # pattern = 
  #   c(
  # "none", "none", "none", "none", # 15°C + No Tide
  # "none", "none", "none", "none", # 21°C + No Tide
  # "stripe", "stripe", "stripe", "stripe", # 15°C + Tide
  # "stripe", "stripe", "stripe", "stripe" # 21°C + Tide
  # ),
  # pattern_angle = c(rep(0, 3), 
  # rep(45, 3), 
  # rep(0, 6)),
#pattern_density = .2,
#pattern_spacing = .04,
#pattern_fill = 'white') +
scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
  geom_errorbar(aes(ymin = Mean_L - SE, ymax = Mean_L + SE),
                width = .2,            # Width of the error bars
                position = position_dodge(0.9)) +
  dark_theme_classic() +
  labs(subtitle = expression(paste(italic("Ostrea lurida"))),
       x = "Stress Hardening Temperature (°C)", 
       y = expression("Shell Length (mm)")) +
  theme( plot.subtitle = element_text(hjust = 1.0),
         legend.position = 'none', 
         strip.background = element_blank(),
         strip.text.x = element_blank()
  ) # +
  #guides(fill = guide_legend(override.aes = 
  # list(
  # pattern = c("none", "none", "none", "none"))))
  
  #### ** PPT: Plot.lurida ==============

  #### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
L.lurida_figs <- read_pptx()
L.lurida_figs <- add_slide(L.lurida_figs, layout = "Title and Content", master = "Office Theme")
L.lurida_figs <- ph_with(x = L.lurida_figs, value = Plot.lurida, location = ph_location_fullsize() )
L.lurida_figs <- ph_with(x = L.lurida_figs, "L. lurida Length", location = ph_location_type(type = "title") )
print(L.lurida_figs, target = "presentations/L.lurida_figs.pptx")

#### R2PPT / RDCOMClient =====
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R", type = "win.binary")
library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.lurida) # Saving the plot to the temporary file

## Step 2: Open a blank PPT slide
# devtools::install_github('omegahat/RDCOMClient')
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

  
  
#### Summary plot ====

L.Summ_Plot <- plot_grid(Plot.gigas, Plot.sikamea, Plot.lurida, nrow = 3)

L.Summ_Plot
