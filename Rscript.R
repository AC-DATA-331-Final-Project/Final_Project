library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
rm(list = ls())

setwd('~/DATA-331-OD/GitHub/Final Project')

butterfly_df <- read_excel('Butterfly_Data/untidy_butterfly_data.xlsx', .name_repair = 'universal')
ladybug_df <- read_excel('Ladybug_Data/untidy_ladybug_data.xlsx', .name_repair = 'universal')

clean_butterfly_df <- butterfly_df %>%
  select()
