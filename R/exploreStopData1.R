library(dplyr)
library(ggplot2)
library(ggthemes)
library(xkcd)
library(plotly)

# LAcrime <- readRDS('ProcessedData/LAcrime2012-2016.rds')
stop2015 <- readRDS('ProcessedData/PoliceStop2015.rds')

# Stop Type and Race? Summary ----
typeRaceSum <- stop2015 %>%
  group_by(stopType = `STOP_TYPE`,
           race = `DESCENT_DESC`) %>%
  summarize(count = n())

typeRaceSum %>%
  ggplot(aes(x = race, y = count, fill = race)) +
  geom_bar(stat = 'identity', colour = 'black') +
  theme(axis.text.x = element_blank()) +
  ggtitle('Police 2015 Stops by Race- Veh vs Ped') +
  facet_wrap(~stopType) + theme_xkcd()
ggsave('Output/SumPlots/stopsByRaceAndType2015.png', width = 6, height = 4)

# stops by Dates -----------------
stopDates <- stop2015 %>%
  group_by(date = `STOP_DT`,
           race = `DESCENT_DESC`) %>%
  summarize(count = n())

stopDates %>%
  ggplot(aes(date, count, group = race, colour = race)) + 
  geom_line() + 
  geom_smooth(alpha = .2) +
  ggtitle('Stops by Race') + theme_xkcd()
ggsave('Output/SumPlots/StopsByRaceAndDate2015.png', width = 6, height = 4)

# Stops by Division Desc
stopDiv <- stop2015 %>%
  group_by(div = DIV1_DESC,
           date = `STOP_DT`) %>%
  summarize(count = n())

divDatePlot <- stopDiv %>%
  ggplot(aes(date, y = count, group = div, colour = div)) +
  geom_line() + 
  ggtitle('Stops by Division') + theme_xkcd()

divDatePlot %>% ggplotly()

# take the top most common ticketers and analyze them ----
topTenStoppers <- stop2015 %>%
  group_by(ofcr = OFCR1_SERL_NBR) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
topTenStoppers <- topTenStoppers$ofcr

topTenWithDates <- stop2015 %>%
  filter(OFCR1_SERL_NBR %in% topTenStoppers) %>%
  group_by(ofcr = OFCR1_SERL_NBR, 
           dates = `STOP_DT`) %>%
  summarize(count = n())

topTenWithDates$ofcr <- topTenWithDates$ofcr %>% as.factor()

topTenTimePlot <- topTenWithDates %>%
  ggplot(aes(dates, count, group = ofcr, colour = ofcr)) + 
  geom_smooth(alpha = .65, se = F) +
  ggtitle('Top Ten Ticketing Officers: Stops Count') +
  theme_xkcd()

topTenTimePlot %>% ggplotly()

 # who are they targeting?
topTenTargetRaces <- stop2015 %>%
  filter(OFCR1_SERL_NBR %in% topTenStoppers) %>%
  group_by(ofcr = OFCR1_SERL_NBR,
           race = DESCENT_DESC) %>%
  summarize(count = n())

topTenTargetRaces %>%
  ggplot(aes(race, count, fill = race)) + 
  geom_bar(stat = 'identity', colour = 'black') +
  facet_wrap(~ofcr) + theme(axis.text.x = element_blank()) +
  theme_xkcd() +
  ggtitle('Top Ten Most Frequent Police Stoppers - Race Targets')
ggsave('Output/SumPlots/Top10Stoppers-RaceTargets2015.png', width = 8, height = 5)
# seems really interesting. Which location are these officers working???

topTenLoc <- stop2015 %>%
  filter(OFCR1_SERL_NBR %in% topTenStoppers) %>%
  group_by(ofcr = OFCR1_SERL_NBR, 
           div = DIV1_DESC) %>%
  summarize(count = n())

# try again with lcoation information
topTenTargetRaces <- stop2015 %>%
  filter(OFCR1_SERL_NBR %in% topTenStoppers) %>%
  group_by(div = DIV1_DESC,
           ofcr = OFCR1_SERL_NBR,
           race = DESCENT_DESC) %>%
  summarize(count = n())

topTenTargetRaces %>%
  ggplot(aes(race, count, fill = race)) + 
  geom_bar(stat = 'identity', colour = 'black') + 
  facet_wrap(~ ofcr + div) +
  theme_xkcd() +
  theme(axis.text.x = element_blank()) +
  ggtitle('Top Ten Most Frequent Police Stops - Race Targets')
ggsave('Output/SumPlots/Top10Stoppers-RaceTargets2015.png', width = 8, height = 5)


# Division Segmentation -----
divRace <- stop2015 %>%
  group_by(div = DIV1_DESC,
            race = DESCENT_DESC) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

divRace %>%
  ggplot(aes(x = race, y = count, fill = race)) + 
  geom_bar(stat = 'identity', colour = 'black') +
  facet_wrap(~ div) +
  theme(axis.text.x = element_blank()) +
  theme_xkcd()
