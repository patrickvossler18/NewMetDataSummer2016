# read each CSV file
# uses read_csv from readr
readLAcsv <- function(year, subdirectory = 'RawData/') {
  fileName <- paste0(subdirectory, 'LAPD_Crime_and_Collision_Raw_data_for_',
                      year, '.csv')
  read_csv(fileName)
}

# split Location col into Lat/Long
splitLatLong <- function(df) {
  df <- df %>%
    separate(`Location 1`, c('Lat', 'Long'), sep = ', ')
  df$Lat <- gsub(pattern = '(', replacement = '', df$Lat, fixed = T)
  df$Long <- gsub(pattern = ')', replacement = '', df$Long, fixed = T)
  df$Lat <- df$Lat %>% as.numeric()
  df$Long <- df$Long %>% as.numeric()
  return(df)
}

# UGLY WAY!
# return vector of TIME OCC
# includes today's date :/  for plotting purposes only
reformatTimeOcc <- function(df) {
  splitDF <- separate(df, col = `TIME OCC`, into = c('hour', 'min'), sep = 2)
  splitDF$`TIME OCC` <- paste0(splitDF$hour, ':', splitDF$min)
  splitDF$`TIME OCC NEW` <- as.POSIXct(splitDF$`TIME OCC`, format = '%H:%M')
  return(splitDF$`TIME OCC`)
}

cleanAndProcess <- function(df) {
  # dates
  df$`Date Rptd` <- as.Date(df$`Date Rptd`, '%m/%d/%Y')
  df$`DATE OCC` <- as.Date(df$`DATE OCC`, '%m/%d/%Y')
  # ugly time fix
  df$`DATETIME OCC` <- reformatTimeOcc(df)
  
  colnames(df)[1] <- 'year'  # rename/ clean .id col
  df[[1]] <- df[[1]] %>%
    substr(start = 2, stop = 5) 
  
  df <- splitLatLong(df)
  return(df)
}

cleanStopData <- function(df) {
  df$`STOP_DT` <- as.Date(df$`STOP_DT`, format = '%m/%d/%Y')
  df$`STOP_TM` <- as.POSIXct(strptime(df$`STOP_TM`, format = '%H:%M'))
  return(df)
}

make_vars_date <- function(crime_df) {
  crime_df$Years = strftime((crime_df$`DATE OCC`),"%Y")
  crime_df$Month = strftime((crime_df$`DATE OCC`),"%m")
  crime_df$DayOfMonth = strftime((crime_df$`DATE OCC`),"%d")
  crime_df$Hour = strftime(strptime(LAcrime$`DATETIME OCC`,"%H:%M"),"%H")
  crime_df$YearsMo = paste( crime_df$Years, crime_df$Month,sep = "-" )
  crime_df$DayOfWeek = factor(weekdays(crime_df$`DATE OCC`),
                              levels=c("Monday","Tuesday",
                                       "Wednesday","Thursday",
                                       "Friday","Saturday","Sunday"),
                              ordered=TRUE)
  crime_df$weekday = "Weekday"
  crime_df$weekday[crime_df$DayOfWeek== "Saturday" | 
                     crime_df$DayOfWeek== "Sunday" | 
                     crime_df$DayOfWeek== "Friday" ] = "Weekend"
  addr_spl = strsplit(as.character(crime_df$Address),"/")
  crime_df$Intersection = ifelse(!is.na(crime_df$`Cross Street`),1,0)
  
  #Because we are using Date Occurred, there are some much older crimes that show up in our dataset
  #For EDA purposes I am going to remove crimes that happened before 2012
  crime_df = crime_df[as.numeric(crime_df$Years) >= 2012,]
  
  return(crime_df)
}


# functions to make contour maps

map_contours <- function(data_trunc, alp) {
  p1 = ggmap(map, extent='device') + 
    geom_point(data=data_trunc, aes(x=Long, y=Lat), alpha= alp) + 
    stat_density2d(aes(x = Long, y = Lat,
                       fill = ..level.., alpha = ..level..),
                   size = 0.1, data = data_trunc, n=100,
                   geom = "polygon") +
    theme(legend.position="none")
  return(p1)
}
