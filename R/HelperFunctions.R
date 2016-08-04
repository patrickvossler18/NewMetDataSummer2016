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