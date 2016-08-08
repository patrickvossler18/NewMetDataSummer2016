library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(feather)

setwd("~/Dropbox (CSU Fullerton)/bigDownloads/LA-Open-Data/LACrimeData/Brian")
source('R/HelperFunctions.R')

# read in Crime and Collision data
c2012 <- readLAcsv(year = 2012)
c2013 <- readLAcsv(year = 2013)
c2014 <- readLAcsv(year = 2014)
c2015 <- readLAcsv(year = 2015)
c2016 <- readLAcsv(year = 2016)

# match colnames
standardColNames <- colnames(c2016) # use latest colnames
colnames(c2012) <- standardColNames
colnames(c2013) <- standardColNames
colnames(c2014) <- standardColNames
colnames(c2015) <- standardColNames

# data matches; rowbind these together, add a year column
crimeDfList <- list(
  c2012 = c2012, c2013 = c2013, c2014 = c2014, c2015 = c2015, c2016 = c2016
  )

# sapply(crimeDfList, 'colnames')
LAcrime <- ldply(crimeDfList, 'rbind')
# combine all cleaning and processing into 1 function here
LAcrime <- cleanAndProcess(LAcrime)
LAcrime <- make_vars_date(LAcrime)
# remove the following crime types (they don't appear often enough)
filteredCrimes <- read_csv('ProcessedData/FilterCrimeTypes.csv')

LAcrime <- LAcrime %>%
  filter(!`Crm Cd Desc` %in% filteredCrimes$`Crm Cd Desc`)

# Initial Mapping/ Binning of Crime Types
crime_buckets <- read_csv('ProcessedData/CrimeBuckets.csv')

LAcrime$simple_buckets <- mapvalues(
  LAcrime$`Crm Cd Desc`,
  from = crime_buckets$`Crm Cd Desc`,
  to = crime_buckets$`Binning Simple`
)

# rename variables
LAcrime <- LAcrime %>%
  select(
    year_id = year_id,
    date_rptd = `Date Rptd`,
    dr_no = `DR. NO`,
    date_occ = `DATE OCC`,
    time_occ = `TIME OCC`,
    area = AREA,
    area_name = `AREA NAME`,
    rd = RD,
    crm_cd = `Crm Cd`,
    crm_cd_desc = `Crm Cd Desc`,
    status = Status,
    status_desc = `Status Desc`,
    location = LOCATION,
    cross_st = `Cross Street`,
    lat = Lat,
    long = Long,
    year = Years,
    month = Month,
    day_of_month = DayOfMonth,
    hour_of_day = Hour,
    year_month = YearsMo,
    day_of_week = DayOfWeek,
    weekday = weekday,
    intersection = Intersection,
    simple_bucket = simple_buckets
  )



saveRDS(LAcrime, 'ProcessedData/LAcrime2012-2016.rds')
write_feather(LAcrime, 'ProcessedData/LAcrime2012-2016.feather')

# Load in Stop Data
stop2015 <- read_csv('RawData/Stop_Data_Open_Data-2015.csv') %>%
  cleanStopData()




# save it
saveRDS(stop2015, 'ProcessedData/PoliceStop2015.rds')
