library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(feather)
library(VIM)

setwd("~/Dropbox (CSU Fullerton)/bigDownloads/LA-Open-Data/LACrimeData/Brian")
source('R/HelperFunctions.R')

crime <- read_feather('ProcessedData/LAcrime2012-2016.feather')

# basic visualizations

sapply(crime, function(x) sum(is.na(x)))

# not surprising
missing_hour_of_day <- crime %>%
  filter(!complete.cases(hour_of_day)) %>%
  group_by(crm_cd_desc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# not surprising
missing_lat_long <- crime %>%
  filter(!complete.cases(lat, long)) %>%
  group_by(crm_cd_desc) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

crime %>%
  ggplot(aes(x = hour_of_day)) +
  geom_bar( color = 'black') +
  facet_wrap(~simple_bucket) +
  ggtitle('LA Crime Type Time of day Distributions')
ggsave('Output/MissingData/LACrimeTimeOfDay2012-2016.png', width = 8, height = 5)

crime %>%
  filter(!complete.cases(lat, long)) %>%
  group_by(simple_bucket) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = simple_bucket, y = count)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Crime Buckets - Missing Lat&Long')
ggsave('Output/MissingData/LACrimeMissingLatLong2012-2016.png', width = 8, height = 5)
