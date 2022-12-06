library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
rm(list = ls())

setwd('~/DATA-331-OD/GitHub/Final Project')

moth_df1 <- read_excel('Moth_Data/untidy_moth_data.xlsx', .name_repair = 'universal')
# ladybug_df1 <- read_excel('Ladybug_Data/untidy_ladybug_data.xlsx', .name_repair = 'universal')
# ladybug_df2 <- read_excel('Ladybug_Data/clean_ladybug_data.xlsx', .name_repair = 'universal')

# Selecting moth data
moth_df_selected <- moth_df1 %>%
  select(coreid, dwc.genus, dwc.specificEpithet, SexUpdated, LWingLength, LWingWidth, LBlackPatchApex, LAnteriorSpotM3, 
         RWingLength, RWingWidth, RBlackPatchApex, RAnteriorSpotM3, dwc.day, dwc.month, dwc.year, dwc.country, 
         dwc.stateProvince, DecimalLatitudeUpdated, DecimalLongitudeUpdated)

moth_df_selected$SexUpdated <- toupper(moth_df_selected$SexUpdated)


# Cleaning moth data
moth_df_selected <- moth_df_selected %>%
  dplyr::rename(genus = dwc.genus, 
                specificEpithet = dwc.specificEpithet,
                day = dwc.day, 
                month = dwc.month, 
                year = dwc.year,
                country = dwc.country,
                state = dwc.stateProvince,
                sex = SexUpdated) %>%
  # dplyr::mutate(sex = recode(sex, M = "male", F = "female")) %>%  
  dplyr::mutate(sex = ifelse(sex == "M" |
                                    sex == "MALE", "Male", "Female")) %>%
  dplyr::mutate(country = ifelse(country == "USA" |
                                      sex == "U.S.A", "United States", country)) %>%
  dplyr::mutate(moth_df_selected, Dates=paste(year, month, day, sep="/"))


moth_df_selected$LWingLength <- as.numeric(moth_df_selected$LWingLength)
moth_df_selected$RWingLength <- as.numeric(moth_df_selected$RWingLength)
moth_df_selected$RWingWidth <- as.numeric(moth_df_selected$RWingWidth)
moth_df_selected$LWingWidth <- as.numeric(moth_df_selected$LWingWidth)
moth_df_selected$LBlackPatchApex <- as.numeric(moth_df_selected$LBlackPatchApex)
moth_df_selected$RBlackPatchApex <- as.numeric(moth_df_selected$RBlackPatchApex)
moth_df_selected$LAnteriorSpotM3 <- as.numeric(moth_df_selected$LAnteriorSpotM3)
moth_df_selected$RAnteriorSpotM3 <- as.numeric(moth_df_selected$RAnteriorSpotM3)

# Question 1
moth_df_Q1A_L <- moth_df_selected %>%
  select(sex, LWingLength, RWingLength) %>% 
  group_by(sex) %>%
  summarise(avgWingLength = mean((LWingLength + RWingLength)/2))

moth_df_Q1A_W <- moth_df_selected %>%
  select(sex, LWingWidth, RWingWidth) %>%
  group_by(sex) %>%
  summarise(avgWingWidth = mean((LWingWidth + RWingWidth)/2))

moth_df_Q1A <- moth_df_Q1A_L %>%
  left_join(moth_df_Q1A_W, by = c("sex")) %>%
  na.omit(avgWingLength)

moth_df_Q1B <- moth_df_selected %>%
  select(sex, LBlackPatchApex, RBlackPatchApex) %>%
  group_by(sex) %>%
  summarise(avgApexArea = mean((LBlackPatchApex + RBlackPatchApex)/2)) %>%
  na.omit(avgApexArea)

moth_df_Q1C <- moth_df_selected %>%
  select(sex, LAnteriorSpotM3, RAnteriorSpotM3) %>%
  group_by(sex) %>%
  summarise(avgAnteriorSpot = mean((LAnteriorSpotM3 + RAnteriorSpotM3)/2)) %>%
  na.omit(avgAnteriorSpot)

# Question 2
moth_df_Q2A_L <- moth_df_selected %>%
  select(country, state, DecimalLatitudeUpdated, DecimalLongitudeUpdated, LWingLength, RWingLength) %>%
  group_by(state) %>%
  summarise(avgWingLength = mean((LWingLength + RWingLength)/2))

moth_df_Q2A_W <- moth_df_selected %>%
  select(country, state, DecimalLatitudeUpdated, DecimalLongitudeUpdated, LWingWidth, RWingWidth) %>%
  group_by(state) %>%
  summarise(avgWingWidth = mean((LWingWidth + RWingWidth)/2))

moth_df_Q2A <- moth_df_Q2A_L %>%
  left_join(moth_df_Q2A_W, by = c("state")) %>%
  na.omit(avgWingLength)
  
# moth_df_Q2A %>%
#   ggplot(moth_df_Q2A, aes(avgWingWidth, avgWingLength, label = rownames(moth_df_Q2A))) +
#   geom_text_repel() +
#   geom_point(color = 'red') +
#   theme_classic(base_size = 16)

moth_df_Q2B <- moth_df_selected %>%
  select(country, state, DecimalLatitudeUpdated, DecimalLongitudeUpdated, LBlackPatchApex, RBlackPatchApex) %>%
  group_by(state) %>%
  summarise(avgApexArea = mean((LBlackPatchApex + RBlackPatchApex)/2)) %>%
  na.omit(avgApexArea)

moth_df_Q2C <- moth_df_selected %>%
  select(country, state, DecimalLatitudeUpdated, DecimalLongitudeUpdated, LAnteriorSpotM3, RAnteriorSpotM3) %>%
  group_by(state) %>%
  summarise(avgAnteriorSpot = mean((LAnteriorSpotM3 + RAnteriorSpotM3)/2)) %>%
  na.omit(avgAnteriorSpot)

# Question 3
moth_df_Q3_Apex <- moth_df_selected %>%
  select(coreid, LBlackPatchApex, RBlackPatchApex) %>%
  group_by(coreid) %>%
  summarise(avgApex = mean((LBlackPatchApex + RBlackPatchApex)/2))

moth_df_Q3_Spot <- moth_df_selected %>%
  select(coreid, LAnteriorSpotM3, RAnteriorSpotM3) %>%
  group_by(coreid) %>%
  summarise(avgSpot = mean((LAnteriorSpotM3 + RAnteriorSpotM3)/2))

moth_df_Q3A <- moth_df_Q3_Apex %>%
  left_join(moth_df_Q3_Spot, by = c("coreid")) %>%
  na.omit(avgApex)

# moth_df_Q3A %>%
#   ggplot(data = moth_df_Q3A, aes(x = avgApex, y = avgSpot)) +
#   geom_point()

moth_df_Q3_wLength <- moth_df_selected %>%
  select(coreid, LWingLength, RWingLength) %>%
  group_by(coreid) %>%
  summarise(avgWingLength = mean((LWingLength + RWingLength)/2))

moth_df_Q3B <- moth_df_Q3_wLength %>%
  left_join(moth_df_Q3_Apex, by = c("coreid")) %>%
  na.omit(avgApex)

moth_df_Q3C <- moth_df_Q3_wLength %>%
  left_join(moth_df_Q3_Spot, by = c("coreid")) %>%
  na.omit(avgSpot)

# Question 4
moth_df_Q4A_L <- moth_df_selected %>%
  select(coreid, Dates, LWingLength, RWingLength) %>%
  group_by(coreid) %>%
  summarise(wingLength = mean((LWingLength + RWingLength)/2))

moth_df_Q4A_W <- moth_df_selected %>%
  select(coreid, Dates, LWingWidth, RWingWidth) %>%
  group_by(coreid) %>%
  summarise(wingWidth = mean((LWingWidth + RWingWidth)/2))

moth_df_Q4A <- moth_df_Q4A_L %>%
  left_join(moth_df_Q4A_W, by = c("coreid")) %>%
  na.omit(wingLength)

# # Selecting and cleaning ladybug data
# ladybug_df_selected <- ladybug_df1 %>%
#   select(scientificName, genus, specificEpithet, dateIdentified, eventDate, country, state, county, municipality)







  