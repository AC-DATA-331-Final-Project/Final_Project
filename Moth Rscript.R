library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
rm(list = ls())

setwd('~/DATA-331-OD/GitHub/Final Project')

moth_df1 <- read_excel('Moth_Data/untidy_moth_data.xlsx', .name_repair = 'universal')

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
  dplyr::mutate(moth_df_selected, Dates=paste(year, month, day, sep="-"))


moth_df_selected$LWingLength <- as.numeric(moth_df_selected$LWingLength)
moth_df_selected$RWingLength <- as.numeric(moth_df_selected$RWingLength)
moth_df_selected$RWingWidth <- as.numeric(moth_df_selected$RWingWidth)
moth_df_selected$LWingWidth <- as.numeric(moth_df_selected$LWingWidth)
moth_df_selected$LBlackPatchApex <- as.numeric(moth_df_selected$LBlackPatchApex)
moth_df_selected$RBlackPatchApex <- as.numeric(moth_df_selected$RBlackPatchApex)
moth_df_selected$LAnteriorSpotM3 <- as.numeric(moth_df_selected$LAnteriorSpotM3)
moth_df_selected$RAnteriorSpotM3 <- as.numeric(moth_df_selected$RAnteriorSpotM3)
moth_df_selected$Dates <- as.Date(moth_df_selected$Dates, format="%Y-%m-%d")
moth_df_selected$Dates <- as.POSIXct(moth_df_selected$Dates, format = "%d-%m-%Y")
moth_df_selected$Year <- format(moth_df_selected$Dates, format = "%Y")
moth_df_selected$Month <- format(moth_df_selected$Dates, format = "%m")


# Male vs Female Legth and Width
moth_df_1_MALE_Length <- moth_df_selected %>%
  select(country, state, sex, LWingLength, RWingLength) %>%
  filter(sex == "Male") %>%
  na.omit(LWingLength) %>%
  group_by(state, sex) %>%
  summarise(avgWingLength = mean((LWingLength + RWingLength)/2))

moth_df_1_MALE_Width <- moth_df_selected %>%
  select(country, state, sex, LWingWidth, RWingWidth) %>%
  filter(sex == "Male") %>%
  na.omit(LWingWidth) %>%
  group_by(state) %>%
  summarise(avgWingWidth = mean((LWingWidth + RWingWidth)/2))

moth_df_1_MALE <- moth_df_1_MALE_Length %>%
  left_join(moth_df_1_MALE_Width, by = c("state"))

moth_df_1_FEMALE_Length <- moth_df_selected %>%
  select(country, state, sex, LWingLength, RWingLength) %>%
  filter(sex == "Female") %>%
  na.omit(LWingLength) %>%
  group_by(state, sex) %>%
  summarise(avgWingLength = mean((LWingLength + RWingLength)/2))

moth_df_1_FEMALE_Width <- moth_df_selected %>%
  select(country, state, sex, LWingWidth, RWingWidth) %>%
  filter(sex == "Female") %>%
  na.omit(LWingWidth) %>%
  group_by(state) %>%
  summarise(avgWingWidth = mean((LWingWidth + RWingWidth)/2))

moth_df_1_FEMALE <- moth_df_1_FEMALE_Length %>%
  left_join(moth_df_1_FEMALE_Width, by = c("state"))

moth_df_1_final <- moth_df_1_MALE %>%
  rbind(moth_df_1_FEMALE)
# filter(state != "Alberta")

# Graphing MvsF Length and Width
ggplot(moth_df_1_final, aes(avgWingWidth, avgWingLength, label = state)) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16) +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(sex))

# Male Collection date by state by LxW
moth_df_MALE_2_L <- moth_df_selected %>%
  select(country, state, sex, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Male") %>%
  na.omit(LWingLength) %>%
  group_by(state, sex, Month) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_MALE_2_R <- moth_df_selected %>%
  select(country, state, sex, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Male") %>%
  na.omit(RWingLength) %>%
  group_by(state, sex, Month) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_2_MALE <- moth_df_MALE_2_L %>%
  left_join(moth_df_MALE_2_R, by = c("Month"))

moth_df_2_MALE <- moth_df_2_MALE %>%
  select(state.x, sex.x, Month, lSqUnits, rSqUnits) %>%
  rename(state = state.x, sex = sex.x) %>%
  group_by(state, Month, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

# moth_df_2_MALE <- moth_df_2_MALE[with(moth_df_2_MALE,order(-avgSqUnits)),]
# moth_df_2_MALE <- moth_df_2_MALE[1:5,]

# Female Collection date by state by LxW
moth_df_FEMALE_2_L <- moth_df_selected %>%
  select(country, state, sex, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Female") %>%
  na.omit(LWingLength) %>%
  group_by(state, sex, Month) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_FEMALE_2_R <- moth_df_selected %>%
  select(country, state, sex, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Female") %>%
  na.omit(RWingLength) %>%
  group_by(state, sex, Month) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_2_FEMALE <- moth_df_FEMALE_2_L %>%
  left_join(moth_df_FEMALE_2_R, by = c("Month"))

moth_df_2_FEMALE <- moth_df_2_FEMALE %>%
  select(state.x, sex.x, Month, lSqUnits, rSqUnits) %>%
  rename(state = state.x, sex = sex.x) %>%
  group_by(state, Month, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

# moth_df_2_FEMALE <- moth_df_2_FEMALE[with(moth_df_2_FEMALE,order(-avgSqUnits)),]
# moth_df_2_FEMALE <- moth_df_2_FEMALE[1:5,]

moth_df_2_final <- moth_df_2_MALE %>%
  rbind(moth_df_2_FEMALE)

# Graphing MvsF sqUnits by state
# ggplot(moth_df_2_final, aes(Month, avgSqUnits, color = state, group = 1)) +
#   geom_line() +
#   facet_wrap(vars(sex))

ggplot(moth_df_2_final, aes(Month, avgSqUnits, label = state)) +
  geom_point() +
  geom_text_repel() +
  theme_classic(base_size = 12) +
  # geom_smooth(method="lm", col="black") +
  facet_wrap(vars(sex))

# Graph avgSqUnits by Month
moth_df_MALE_3_L <- moth_df_selected %>%
  select(sex, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Male") %>%
  na.omit(LWingLength) %>%
  group_by(Month, sex) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_MALE_3_R <- moth_df_selected %>%
  select(sex, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Male") %>%
  na.omit(RWingLength) %>%
  group_by(Month, sex) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_3_MALE <- moth_df_MALE_3_L %>%
  select(Month, lSqUnits) %>%
  left_join(moth_df_MALE_3_R, by = c("Month"))

moth_df_3_MALE_Month <- moth_df_3_MALE %>%
  group_by(Month, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

moth_df_FEMALE_3_L <- moth_df_selected %>%
  select(sex, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Female") %>%
  na.omit(LWingLength) %>%
  group_by(Month, sex) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_FEMALE_3_R <- moth_df_selected %>%
  select(sex, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Female") %>%
  na.omit(RWingLength) %>%
  group_by(Month, sex) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_3_FEMALE <- moth_df_FEMALE_3_L %>%
  select(Month, lSqUnits) %>%
  left_join(moth_df_FEMALE_3_R, by = c("Month"))

moth_df_3_FEMALE_Month <- moth_df_3_FEMALE %>%
  group_by(Month, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

moth_df_3_final <- moth_df_3_FEMALE_Month %>%
  rbind(moth_df_3_MALE_Month)

ggplot(moth_df_3_final, aes(x=Month, y=avgSqUnits, group = 1, color = sex)) +
  geom_line() +
  facet_wrap(vars(sex))

# Graph avgSqUnits by state in month 6
moth_df_MALE_4_L <- moth_df_selected %>%
  select(sex, state, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Male", Month == "06") %>%
  na.omit(LWingLength) %>%
  group_by(state, Month, sex) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_MALE_4_R <- moth_df_selected %>%
  select(sex, state, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Male", Month == "06") %>%
  na.omit(RWingLength) %>%
  group_by(state, Month, sex) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_4_MALE <- moth_df_MALE_4_L %>%
  select(state, Month, lSqUnits) %>%
  left_join(moth_df_MALE_4_R, by = c("state"))

moth_df_4_MALE <- moth_df_4_MALE %>%
  group_by(state, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

moth_df_4_MALE <- moth_df_4_MALE %>%
  na.omit(avgSqUnits)

moth_df_4_MALE <- moth_df_4_MALE[with(moth_df_4_MALE,order(-avgSqUnits)),]
moth_df_4_MALE <- moth_df_4_MALE[1:7,]

moth_df_FEMALE_4_L <- moth_df_selected %>%
  select(sex, state, Month, LWingLength, LWingWidth) %>%
  filter(sex == "Female", Month == "06") %>%
  na.omit(LWingLength) %>%
  group_by(state, Month, sex) %>%
  summarise(lSqUnits = mean(LWingLength * LWingWidth))

moth_df_FEMALE_4_R <- moth_df_selected %>%
  select(sex, state, Month, RWingLength, RWingWidth) %>%
  filter(sex == "Female", Month == "06") %>%
  na.omit(RWingLength) %>%
  group_by(state, Month, sex) %>%
  summarise(rSqUnits = mean(RWingLength * RWingWidth))

moth_df_4_FEMALE <- moth_df_FEMALE_4_L %>%
  select(state, Month, lSqUnits) %>%
  left_join(moth_df_FEMALE_4_R, by = c("state"))

moth_df_4_FEMALE <- moth_df_4_FEMALE %>%
  group_by(state, sex) %>%
  summarise(avgSqUnits = mean((lSqUnits + rSqUnits)/2))

moth_df_4_FEMALE <- moth_df_4_FEMALE[with(moth_df_4_FEMALE,order(-avgSqUnits)),]
moth_df_4_FEMALE <- moth_df_4_FEMALE[1:7,]

moth_df_4_final <- moth_df_4_MALE %>%
  rbind(moth_df_4_FEMALE)

ggplot(moth_df_4_final, aes(x=state, y = avgSqUnits, fill = state)) +
  geom_col() +
  facet_wrap(vars(sex))

# Creating a ttest
moth_df_ttest <- moth_df_2_final %>%
  select(sex, avgSqUnits) 

ggplot(moth_df_ttest, aes(sex, avgSqUnits)) +
  geom_boxplot()
  
t.test(moth_df_2_MALE$avgSqUnits, moth_df_2_FEMALE$avgSqUnits,var.equal = T)


  

