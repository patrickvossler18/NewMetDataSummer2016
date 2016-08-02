library(dplyr)
library(ggplot2)
library(ggthemes)
library(xkcd)
library(plotly)

LAcrime <- readRDS('ProcessedData/LAcrime2012-2016.rds')
# explore some Summary Data ----
# Crime Code (Crm Cd) / Crime Description (Crm Cd Desc)
crimeTypes <- LAcrime %>%
  group_by(`Crm Cd Desc`) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

crimeTypes %>% head(8) %>%
  ggplot(aes(`Crm Cd Desc`, count, fill = `Crm Cd Desc`)) + 
  geom_bar(stat = 'identity', colour = 'black') +
  ggtitle('Top 8 Crimes from 2012 - 2016') +
  theme_xkcd() + theme(axis.text.x = element_blank())
ggsave('Output/SumPlots/top8crimes2012-2016.png', width = 6, height = 4)

# Status of Crimes by Year
crimeStatus <- LAcrime %>%
  group_by(`.id`, `Status Desc`) %>%
  summarize(count = n())

crimeStatus %>% 
  ggplot(aes(`Status Desc`, count, fill = `Status Desc`)) + 
  geom_bar(stat = 'identity', colour = 'black') + facet_wrap(~`.id`) +
  ggtitle('Crime Status by Year') +
  theme_xkcd() + theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave('Output/SumPlots/crimeStatusByYear2012-2016.png', width = 6, height = 4)

# Crime by Dates Reported
crimeDates <- LAcrime %>%
  group_by(date = `Date Rptd`) %>%
  summarize(count = n()) %>%
  arrange(date)

crimeDates %>%
  ggplot(aes(date, count)) + geom_line(alpha = .85) + 
  geom_smooth(colour = 'red', alpha = .2) +
  ggtitle('Crime Count') + theme_xkcd()
ggsave('Output/SumPlots/crimeCountvsDate2012-2016.png', width = 6, height = 4)

# Crimes by Time of Day
crimeTimes <- LAcrime %>%
  group_by(time = `DATETIME OCC`) %>%
  summarize(count = n()) %>%
  arrange(time)

crimeTimes %>%
  ggplot(aes(time, count)) + geom_point(alpha = .15) +
  geom_smooth(colour = 'blue', alpha = .1) +
  theme_xkcd() + theme(axis.text.x = element_blank()) +
  ggtitle('Time of Crimes Occurring') +
  xlab('Time of Day')
ggsave('Output/SumPlots/crimeCountsvsTime2012-2016.png', width = 6, height = 4)

# look at most common crimes vs dates / time
commonCrimes <- crimeTypes %>% 
  select(crime = `Crm Cd Desc`) %>% 
  head(10)
  
crimeTypeDates <- LAcrime %>%
  filter(`Crm Cd Desc` %in% commonCrimes$crime) %>%
  group_by(date = `Date Rptd`, type = `Crm Cd Desc`) %>%
  summarize(count = n())

ctd <- crimeTypeDates %>%
  ggplot(aes(date, count, colour = type, group = type)) + 
  geom_line(alpha = .5) +
  theme_xkcd() +
  ggtitle('Top Ten Types of Crime')
ggsave('Output/SumPlots/crimeTypesVsDates2012-2016.png', width = 8, height = 5)

ctd %>% ggplotly() # save this manually

crimeTypeTimes <- LAcrime %>%
  filter(`Crm Cd Desc` %in% commonCrimes$crime) %>%
  group_by(time = `DATETIME OCC`, type = `Crm Cd Desc`) %>%
  summarize(count = n())

ctt <- crimeTypeTimes %>%
  ggplot(aes(time, count, colour = type, group = type)) + 
  geom_line(alpha = .5) + 
  ggtitle('Top Ten Types of Crime vs Time') +
  theme(axis.text.x = element_blank())

ctt %>% ggplotly()

# Instead of DAate Rptd, look at DATE OCC
ctd_occ <- LAcrime %>%
  filter(`Crm Cd Desc` %in% commonCrimes$crime) %>%
  group_by(date = `DATE OCC`, type = `Crm Cd Desc`) %>%
  summarize(count = n())

ctd_plot <- ctd_occ %>%
  ggplot(aes(date, count, colour = type, group = type)) +
  geom_line(alpha = .5) +
  ggtitle('Top Ten Types of Crime - Date Occurred') +
  theme_xkcd()

ctd_plot %>% ggplotly() # save manually
