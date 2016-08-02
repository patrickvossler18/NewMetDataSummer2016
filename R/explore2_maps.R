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
