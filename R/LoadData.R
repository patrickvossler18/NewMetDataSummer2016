library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)

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

sapply(crimeDfList, 'colnames')
LAcrime <- ldply(crimeDfList, 'rbind')
# combine all cleaning and processing into 1 function here
LAcrime <- cleanAndProcess(LAcrime)
LAcrime = make_vars_date(LAcrime)
saveRDS(LAcrime, 'ProcessedData/LAcrime2012-2016.rds')

# Load in Stop Data
stop2015 <- read_csv('RawData/Stop_Data_Open_Data-2015.csv') %>%
  cleanStopData()

# save it
saveRDS(stop2015, 'ProcessedData/PoliceStop2015.rds')
