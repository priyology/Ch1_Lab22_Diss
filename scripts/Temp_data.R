#### ~ TEMPERATURES ~ =====

## load libraries
library(tidyverse)

#### Sump 15°C =====

### load data sheet
Temps_Sump15 <- read_csv("data/HOBOs/15_sump.csv")
glimpse(Temps_Sump15)
summary(Temps_Sump15)
tail(Temps_Sump15)
View(Temps_Sump15)

#### Clean Data

Temps_Sump15_v2 <- Temps_Sump15 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_Sump15_v2)

#### Stress Hardening 1: 15°C 
## 2022-02-24 to 2022-03-09
Temps_Sump15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
    geom_path(color = "black") +
    #stat_smooth(method = "lm") +
    geom_point(color = "black") +
  theme_classic()

Temps_Sump15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #15.6
            SD_Temp = sd(Temp_C), #0.611
            SE_Temp = SD_Temp/sqrt(n()), #0.0180
            variance = var(Temp_C), #0.373
            max = max(Temp_C), #17.5
            min = min(Temp_C)) #14.5

### MHW: 15°C
## 2022-03-11 to 2022-03-13

Temps_Sump15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #16.3
            SD_Temp = sd(Temp_C), #0.845
            SE_Temp = SD_Temp/sqrt(n()), #0.0862
            variance = var(Temp_C), #0.714
            max = max(Temp_C), #17.3
            min = min(Temp_C)) #14.3
            
### Stress Hardening 1: 15°C
## 2022-02-27 to 2022-03-12

Temps_Sump15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #15.6
            SD_Temp = sd(Temp_C), #0.633
            SE_Temp = SD_Temp/sqrt(n()), #0.0186
            variance = var(Temp_C),  #0.400
            max = max(Temp_C), #17.5
            min = min(Temp_C)) #14.9
            
### MHW: 18°C
## 2022-03-14 to 2022-03-16

Temps_Sump15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #19.3
            SD_Temp = sd(Temp_C), #0.549
            SE_Temp = SD_Temp/sqrt(n()), #0.0561
            variance = var(Temp_C), #0.302
            max = max(Temp_C), #20.1
            min = min(Temp_C)) #18.3

#### Sump 21°C =====

### load data sheet
Temps_Sump21 <- read_csv("data/HOBOs/21_sump.csv")
glimpse(Temps_Sump21)
summary(Temps_Sump21)
tail(Temps_Sump21)
View(Temps_Sump21)

#### Clean Data

Temps_Sump21_v2 <- Temps_Sump21 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_Sump21_v2)

#### Stress Hardening 1: 21°C 
## 2022-02-24 to 2022-03-09
Temps_Sump21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.3
            SD_Temp = sd(Temp_C), #0.350
            SE_Temp = SD_Temp/sqrt(n()), #0.0103
            variance = var(Temp_C), #0.122
            max = max(Temp_C), #22.4
            min = min(Temp_C)) #20.6

### MHW: 21°C
## 2022-03-11 to 2022-03-13

Temps_Sump21_v2 %>% ,
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump21_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.4
            SD_Temp = sd(Temp_C), #0.344
            SE_Temp = SD_Temp/sqrt(n()), #0.0351
            variance = var(Temp_C), #0.119
            max = max(Temp_C), #22.1
            min = min(Temp_C)) #20.8
            
### Stress Hardening 1: 21°C
## 2022-02-27 to 2022-03-12

Temps_Sump21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.3
            SD_Temp = sd(Temp_C), #0.353
            SE_Temp = SD_Temp/sqrt(n()), #0.0104
            variance = var(Temp_C), #0.125
            max = max(Temp_C), #22.4
            min = min(Temp_C)) #20.7

### MHW: 24°C
## 2022-03-14 to 2022-03-16

Temps_Sump21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Sump21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #26.0
            SD_Temp = sd(Temp_C), #0.388
            SE_Temp = SD_Temp/sqrt(n()), #0.0397
            variance = var(Temp_C), #0.151
            max = max(Temp_C), #26.8
            min = min(Temp_C)) #25.3

#### Tide 15°C =====

### load data sheet
Temps_Tide15 <- read_csv("data/HOBOs/15_tide.csv")
glimpse(Temps_Tide15)
summary(Temps_Tide15)
tail(Temps_Tide15)
View(Temps_Tide15)

#### Clean Data

Temps_Tide15_v2 <- Temps_Tide15 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_Tide15_v2)

#### Stress Hardening 1: 15°C 
## 2022-02-24 to 2022-03-09
Temps_Tide15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #16.2
            SD_Temp = sd(Temp_C), #0.732
            SE_Temp = SD_Temp/sqrt(n()), #0.0216
            max = max(Temp_C), #17.9
            min = min(Temp_C)) #15.0

### MHW: 15°C
## 2022-03-11 to 2022-03-13

Temps_Tide15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #16.6
            SD_Temp = sd(Temp_C), #0.817
            SE_Temp = SD_Temp/sqrt(n()), #0.0834
            variance = var(Temp_C), #0.668
            max = max(Temp_C), #17.8
            min = min(Temp_C)) #14.5

### Stress Hardening 1: 15°C
## 2022-02-27 to 2022-03-12

Temps_Tide15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #16.2
            SD_Temp = sd(Temp_C), #0.753
            SE_Temp = SD_Temp/sqrt(n()), #0.0222
            variance = var(Temp_C), #0.567
            max = max(Temp_C), #17.9
            min = min(Temp_C)) #15.0
### MHW: 18°C
## 2022-03-14 to 2022-03-16

Temps_Tide15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #19.4
            SD_Temp = sd(Temp_C), #0.545
            SE_Temp = SD_Temp/sqrt(n()), #0.0556
            variance = var(Temp_C), #0.297
            max = max(Temp_C), #20.1
            min = min(Temp_C)) #18.4

#### No Tide 15°C =====

### load data sheet
Temps_NoTide15 <- read_csv("data/HOBOs/15_NoTide.csv")
glimpse(Temps_NoTide15)
summary(Temps_NoTide15)
tail(Temps_NoTide15)
View(Temps_NoTide15)

#### Clean Data

Temps_NoTide15_v2 <- Temps_NoTide15 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_NoTide15_v2)

#### Stress Hardening 1: 15°C 
## 2022-02-24 to 2022-03-09
Temps_NoTide15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide15_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #15.7
            SD_Temp = sd(Temp_C), #0.602
            SE_Temp = SD_Temp/sqrt(n()), #0.0177
            variance = var(Temp_C), #0.362
            max = max(Temp_C), #17.6
            min = min(Temp_C)) #15.0

### MHW: 15°C
## 2022-03-11 to 2022-03-13

Temps_NoTide15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide15_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #16.4
            SD_Temp = sd(Temp_C), #0.830
            SE_Temp = SD_Temp/sqrt(n()), #0.0847
            variance = var(Temp_C), #0.689
            max = max(Temp_C), #17.4
            min = min(Temp_C)) #15.0

### Stress Hardening 1: 15°C
## 2022-02-27 to 2022-03-12

Temps_NoTide15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide15_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #15.7
            SD_Temp = sd(Temp_C), #0.623
            SE_Temp = SD_Temp/sqrt(n()), #0.0183
            variance = var(Temp_C), #0.388
            max = max(Temp_C), #17.6
            min = min(Temp_C)) #15.0

### MHW: 18°C
## 2022-03-14 to 2022-03-16

Temps_NoTide15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide15_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #19.4
            SD_Temp = sd(Temp_C), #0.462
            SE_Temp = SD_Temp/sqrt(n()), #0.0472
            variance = var(Temp_C), #0.214 
            max = max(Temp_C), #20.0
            min = min(Temp_C)) #18.6

#### Tide 21°C =====

### load data sheet
Temps_Tide21 <- read_csv("data/HOBOs/21_tide.csv")
glimpse(Temps_Tide21)
summary(Temps_Tide21)
tail(Temps_Tide21)
View(Temps_Tide21)

#### Clean Data

Temps_Tide21_v2 <- Temps_Tide21 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_Tide21_v2)

#### Stress Hardening 1: 21°C 
## 2022-02-24 to 2022-03-09
Temps_Tide21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #20.5
            SD_Temp = sd(Temp_C), #1.13
            SE_Temp = SD_Temp/sqrt(n()),#0.03338
            variance = var(Temp_C), #1.28
            max = max(Temp_C), #22.3
            min = min(Temp_C)) #18.0

### MHW: 21°C
## 2022-03-11 to 2022-03-13

Temps_Tide21_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide21_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #20.5
            SD_Temp = sd(Temp_C), #1.24
            SE_Temp = SD_Temp/sqrt(n()), #0.126
            variance = var(Temp_C), #1.53
            max = max(Temp_C), #22.0
            min = min(Temp_C)) #18.5

### Stress Hardening 1: 21°C
## 2022-02-27 to 2022-03-12

Temps_Tide21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #20.5
            SD_Temp = sd(Temp_C), #1.18
            SE_Temp = SD_Temp/sqrt(n()), #0.0348
            variance = var(Temp_C), #1.40
            max = max(Temp_C), #22.3
            min = min(Temp_C)) #18.2

### MHW: 24°C
## 2022-03-14 to 2022-03-16

Temps_Tide21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_Tide21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #26.0
            SD_Temp = sd(Temp_C), #0.350
            SE_Temp = SD_Temp/sqrt(n()), #0.0357
            variance = var(Temp_C), #0.122
            max = max(Temp_C), #26.7
            min = min(Temp_C)) #25.4

#### No Tide 21°C =====

### load data sheet
Temps_NoTide21 <- read_csv("data/HOBOs/21_NoTide.csv")
glimpse(Temps_NoTide21)
summary(Temps_NoTide21)
tail(Temps_NoTide21)
View(Temps_NoTide21)

#### Clean Data

Temps_NoTide21_v2 <- Temps_NoTide21 %>% 
  mutate(Date_Time = mdy_hm(Date_Time), # set date/time format
         Temp_C = (Temp_F - 32) * (5/9)) %>% ## convert Fahrenheit to Celsius and add it as a column
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) %>% ## add date/time as own columns
  filter(date > "2022-02-23" & date < "2022-03-17")

tail(Temps_NoTide21_v2)

#### Stress Hardening 1: 21°C 
## 2022-02-24 to 2022-03-09
Temps_NoTide21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide21_v2 %>% 
  filter(date > "2022-02-24" & date < "2022-03-09") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.3
            SD_Temp = sd(Temp_C), #0.290
            SE_Temp = SD_Temp/sqrt(n()), #0.00855
            variance = var(Temp_C), #0.0842
            max = max(Temp_C), #22.2
            min = min(Temp_C)) #20.7

### MHW: 21°C
## 2022-03-11 to 2022-03-13

Temps_NoTide21_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide21_v2 %>% 
  filter(date > "2022-03-11" & date < "2022-03-13") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.1
            SD_Temp = sd(Temp_C), #0.138
            SE_Temp = SD_Temp/sqrt(n()), #0.0141
            variance = var(Temp_C), #0.0190
            max = max(Temp_C), #21.5
            min = min(Temp_C)) #20.9

### Stress Hardening 1: 21°C
## 2022-02-27 to 2022-03-12

Temps_NoTide21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide21_v2 %>% 
  filter(date > "2022-02-27" & date < "2022-03-12") %>% 
  summarize(mean_Temp = mean(Temp_C), #21.3
            SD_Temp = sd(Temp_C), #0.272
            SE_Temp = SD_Temp/sqrt(n()), #0.00801
            variance = var(Temp_C)) #0.0740

### MHW: 24°C
## 2022-03-14 to 2022-03-16

Temps_NoTide21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>% 
  ggplot(aes(Date_Time, Temp_C)) +
  geom_path(color = "black") +
  #stat_smooth(method = "lm") +
  geom_point(color = "black") +
  theme_classic()

Temps_NoTide21_v2 %>% 
  filter(date > "2022-03-14" & date < "2022-03-16") %>%
  summarize(mean_Temp = mean(Temp_C), #24.0
            SD_Temp = sd(Temp_C), #0.251
            SE_Temp = SD_Temp/sqrt(n()), #0.0256
            variance = var(Temp_C), #0.0629
            max = max(Temp_C), #24.5
            min = min(Temp_C)) #23.5


