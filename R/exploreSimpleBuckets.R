library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(feather)
library(plotly)

setwd("~/Dropbox (CSU Fullerton)/bigDownloads/LA-Open-Data/LACrimeData/Brian")
source('R/HelperFunctions.R')

crime <- read_feather('ProcessedData/LAcrime2012-2016.feather')

# plot 
simple_bucket_date_occ <- crime %>%
  group_by(date_rptd, simple_bucket, area_name) %>%
  summarize(count = n())

simple_bucket_date_occ %>%
  ggplot(aes(date_rptd, y = count, group = simple_bucket)) +
  geom_line() + 
  facet_wrap(~simple_bucket)

g <- simple_bucket_date_occ %>%
  ggplot(aes(x = date_rptd, y = count, color = simple_bucket, 
        group = simple_bucket)) + 
  geom_line(alpha = .3) +
  facet_wrap(~area_name) +
  ggtitle('Simple Buckets Count over Time')
  g %>% ggplotly()
  
  # # # # 
  library(RDSTK)
  coordinates2statistics(crime$lat[1], crime$long[1], 'population_density')


# Lat long bin
crime$lat_long_bin <- bin_lat_long(crime$lat, crime$long, breaks = 100)

crime %>%
  ggplot(aes(x = long, y = lat, color = lat_long_bin)) + 
  geom_point(alpha = .2) + theme(legend.position = 'none')
  
crime %>%
  group_by(lat_long_bin) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  View()
