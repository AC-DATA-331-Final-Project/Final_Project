library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
rm(list = ls())

setwd('~/DATA-331-OD/GitHub/Final Project')

ladybug_df1 <- read_excel('Ladybug_Data/untidy_ladybug_data.xlsx', .name_repair = 'universal')
ladybug_df2 <- read_excel('Ladybug_Data/clean_ladybug_data.xlsx', .name_repair = 'universal')

