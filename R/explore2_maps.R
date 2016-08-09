library(ggmap)
laMap <- qmap('Los Anageles', zoom = 11, maptype = 'hybrid')
m1 <- laMap + 
  geom_point(aes(x = Long, y = Lat, colour = `Crm Cd Desc`), alpha = .3,
             data = LAcrime %>% filter(`Crm Cd Desc` %in% commonCrimes$crime))


m2 <- laMap + 
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
        data = LAcrime
  )

# Try basic scater plots ------------------------------------
topTwelveCrimes <- LAcrime %>%
  group_by(`Crm Cd Desc`) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(12)


thisYearCrime <- LAcrime %>%
  filter(Year == '2016') %>%
  filter(Lat > 30) %>%
  filter(Long < -115) %>%
  filter(`Crm Cd Desc` %in% topTwelveCrimes$`Crm Cd Desc`)

ggplot(thisYearCrime, aes(Long, Lat, color = `AREA NAME`)) +
  geom_point(alpha = .10) + theme(legend.position = 'none') +
  facet_wrap(~`Crm Cd Desc`) + theme_xkcd() + 
  ggtitle('2016 Top 12 Crime Scatters')

ggsave('Output/BasicCrimeScatters/BasicCrimeScatters-2016.png', width = 8, height = 6)

# analyze homicide ----
LAcrime %>%
  filter(`Crm Cd Desc` %in% c('CRIMINAL HOMICIDE', 'HOMICIDE (NON-UCR)')) %>%
  group_by(`Crm Cd Desc`) %>%
  summarize(count = n())

LAcrime %>%
  filter(Lat > 30) %>%
  filter(Long < -115) %>%
  filter(`Crm Cd Desc` == 'CRIMINAL HOMICIDE') %>%
  ggplot(aes(Long, Lat, color = occ_month)) + geom_point(alpha = .55) + 
  facet_wrap(~occ_year) + 
  ggtitle('2012-2016 Homicide Locations')
ggsave('Output/BasicCrimeScatters/HomicideScattersByYear.png', width = 8, height = 6)

LAcrime %>%
  filter(Lat > 30) %>%
  filter(Long < -115) %>%
  filter(`Crm Cd Desc` == 'CRIMINAL HOMICIDE') %>%
  filter(occ_year == 2015) %>%
  ggplot(aes(Long, Lat, color = occ_day)) + geom_point(alpha = .75) + 
  facet_wrap(~occ_month) + theme_bw() +
  ggtitle('2015 Homicide Locations by Month')
ggsave('Output/BasicCrimeScatters/HomicideScatterbyMonth-2015.png', width = 8, height = 6)

# See if 'average' moves over time
homicideFilt <- LAcrime %>%
  filter(Lat > 30) %>%
  filter(Long < -115) %>%
  filter(`Crm Cd Desc` == 'CRIMINAL HOMICIDE') %>%
  filter(occ_year == 2015)

homDayAvg <- homicideFilt %>%
  group_by(`DATE OCC`) %>%
  summarize(avgLat = mean(Lat), avgLong = mean(Long))

homDayAvg %>%
  ggplot(aes(avgLong, avgLat, colour = `DATE OCC`)) + geom_point() +
  facet_wrap(~`DATE OCC`)


a <- 1:10
# slice up Lat & Long
sliceLatLongGrid <- function(longVec, latVec, breaks = 20) {
  longCut <- cut(longVec, breaks = breaks)
  latCut <- cut(latVec, breaks = breaks)
  grid <- 
}