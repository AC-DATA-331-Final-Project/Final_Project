library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(stringi)
library(stringr)
library(treemapify)
library(plotly)
rm(list = ls())

setwd('~/DATA-331-OD/GitHub/Final Project')

ladybug_df1 <- read_excel('Ladybug_Data/untidy_ladybug_data.xlsx', .name_repair = 'universal')

ladybug_selected <- ladybug_df1 %>%
  select(scientificName, eventDate, year, month, country, stateProvince) %>%
  rename(date = eventDate,
         state = stateProvince)

ladybug_selected$date <- format(ladybug_selected$date, format = "%Y")
ladybug_selected$date <- as.Date(ladybug_selected$date, format = "%Y")

ladybug_selected$state <- ifelse(stri_length(ladybug_selected$state) < 3, state.name[match(ladybug_selected$state, state.abb)], ladybug_selected$state)
ladybug_selected$scientificName <- gsub(" ", "-", ladybug_selected$scientificName)

ladybug_df_state <- ladybug_selected %>%
  dplyr::mutate(nameXstate = paste(state, scientificName)) %>%
  dplyr::mutate(count = 1)

ladybug_df_state <- ladybug_df_state %>%
  select(nameXstate, count) %>%
  group_by(nameXstate) %>%
  summarise(occurences = sum(count))
  
ladybug_df_state[c('state', 'scientificName')] <- str_split_fixed(ladybug_df_state$nameXstate, ' ', 2)

ladybug_df_state <- ladybug_df_state %>%
  select(scientificName, state, occurences) %>% 
  filter(scientificName != "NA", state != "NA") %>%
  dplyr::mutate(nameXoccurences = paste(scientificName, occurences))

ladybug_df_state$nameXoccurences <- gsub(" ", ", ", ladybug_df_state$nameXoccurences)
ladybug_df_state$scientificName <- gsub("-", " ", ladybug_df_state$scientificName)
ladybug_df_state$nameXoccurences <- gsub("-", " ", ladybug_df_state$nameXoccurences)

ggplot(ladybug_df_state, aes(area = occurences, fill = scientificName, label = nameXoccurences)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) +
  facet_wrap(vars(state))

# Graphing scientific name by year
ladybug_df_year <- ladybug_selected %>%
  dplyr::mutate(nameXyear = paste(year, scientificName)) %>%
  dplyr::mutate(count = 1)

ladybug_df_year <- ladybug_df_year %>%
  select(nameXyear, count) %>%
  group_by(nameXyear) %>%
  summarise(occurences = sum(count))

ladybug_df_year[c('year', 'scientificName')] <- str_split_fixed(ladybug_df_year$nameXyear, ' ', 2)

ladybug_df_year <- ladybug_df_year %>%
  select(scientificName, year, occurences) %>% 
  filter(scientificName != "NA", year != "NA")

ladybug_df_year$scientificName <- gsub("-", " ", ladybug_df_year$scientificName)

ggplot(ladybug_df_year, aes(year, occurences, color = scientificName)) +
  geom_point() 

# Graphing scientific name by year
ladybug_df_month <- ladybug_selected %>%
  dplyr::mutate(nameXmonth = paste(month, scientificName)) %>%
  dplyr::mutate(count = 1)

ladybug_df_month <- ladybug_df_month %>%
  select(nameXmonth, count) %>%
  group_by(nameXmonth) %>%
  summarise(occurences = sum(count))

ladybug_df_month[c('month', 'scientificName')] <- str_split_fixed(ladybug_df_month$nameXmonth, ' ', 2)

ladybug_df_month <- ladybug_df_month %>%
  select(scientificName, month, occurences) %>% 
  filter(scientificName != "NA", month != "NA") 

ladybug_df_month$scientificName <- gsub("-", " ", ladybug_df_month$scientificName)
ladybug_df_month$month <- as.numeric(ladybug_df_month$month)

ggplot(ladybug_df_month, aes(month, occurences, color = scientificName)) +
  geom_point()

# ttest to see if bouth month 7 and 8 could considered peak ladybug season
ladybug_df_ttest <- ladybug_df_month %>%
  filter(month == "7" | month == "8")

ggplot(ladybug_df_ttest, aes(month, occurences, group = month)) +
  geom_boxplot()

ladybug_df_ttest_7 <- ladybug_df_month %>%
  filter(month == "7")

ladybug_df_ttest_8 <- ladybug_df_month %>%
  filter(month == "8")

t.test(ladybug_df_ttest_7$occurences, ladybug_df_ttest_8$occurences, var.equal = T)
