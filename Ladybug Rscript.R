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
  select(scientificName, eventDate, country, stateProvince) %>%
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

