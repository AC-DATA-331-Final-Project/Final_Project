# Final_Project
## Selecting and cleaning the data
* We first selected the columns that we needed from the dataframe.
* Then, we formatted each column as needed - dates to dates, state abbreviations to full names, and put dashes between scientific names to make it easier to concatenate the state column later on.

### Code
```
ladybug_selected <- ladybug_df1 %>%
  select(scientificName, eventDate, year, month, country, stateProvince) %>%
  rename(date = eventDate,
         state = stateProvince)

ladybug_selected$date <- format(ladybug_selected$date, format = "%Y")
ladybug_selected$date <- as.Date(ladybug_selected$date, format = "%Y")

ladybug_selected$state <- ifelse(stri_length(ladybug_selected$state) < 3, state.name[match(ladybug_selected$state, state.abb)], ladybug_selected$state)
ladybug_selected$scientificName <- gsub(" ", "-", ladybug_selected$scientificName)
```




## Graph 1
<img src="images/Rplot03.png" alt="Girl in a jacket" width="1400" height="700">
* We wanted to show which species were most prevelent in each state. However, we were extremely limited in displaying this because mostly all the data was collected in either Illinois or Iowa.
* We concatenated scientific name and state into 1 column to do a group by and summarise to count how many findings there were for each species in each state.

### Failures
* Finding a way to count state name and scientific name at the same time.
* How to space the data.

### Code
```
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
```



## Graph 2
<img src="images/Rplot.png" alt="Girl in a jacket" width="1400" height="700">
* We wanted to show the number of observations by year for each species.

### Failures
* While this graph does display its purpose, it is not very easy to read and lookig back, a stacked bar chart would have been better.

### Code
```
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
```



## Graph 3
<img src="images/Rplot06.png" alt="Girl in a jacket" width="1400" height="700">
* We wanted to show the number of observations by month for each species.
* The goal was to determine if the amount of ladybugs increase during certain times of the year. 
* It was pretty easy to tell that months 7 and 8 were peak ladybug season.

### Failures
* When month was a character it wouldn't format properly. We had to change month to a number and the x axis is scaled to month 5, 7.5, and 10.
* Graph would have been clearer if it was just a stacked bar chart.

### Code
```
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
```


## Graph 4 and ttest
<img src="images/Rplot07.png" alt="Girl in a jacket" width="1200" height="700">

* We wanted to show some of the statistics for months 7 and 8 to determine if one month was better than the other.
* It was hard to determine if the datasets were statistically similar or not so we created a ttest to see.

### Code
```
ladybug_df_ttest <- ladybug_df_month %>%
  filter(month == "7" | month == "8")

ggplot(ladybug_df_ttest, aes(month, occurences, group = month)) +
  geom_boxplot()
```

<img src="images/ttest_ladybug.png" alt="Girl in a jacket" width="1200" height="500">

* We were able to determine that month 7 and 8 were statistically similar and that they both can be considered equally peak ladybug season. 

### Code
```
ladybug_df_ttest_7 <- ladybug_df_month %>%
  filter(month == "7")

ladybug_df_ttest_8 <- ladybug_df_month %>%
  filter(month == "8")

t.test(ladybug_df_ttest_7$occurences, ladybug_df_ttest_8$occurences, var.equal = T)
```
