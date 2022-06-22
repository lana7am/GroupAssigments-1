# Load the packages
library(tidyverse)
library(ggplot2)





spotify <- read.csv("data/spotify_songs.csv")

dim(spotify)
colnames(spotify)  
head(spotify, 4)
summary(spotify)
View(spotify)
spotify_songs <- spotify %>% 
  mutate(
    track_album_release_date = as.Date(track_album_release_date),
    year = as.numeric(format(track_album_release_date,'%Y')),
    era = case_when(year <= 2000 ~ "under 00s era",
                    year > 2000 & year <= 2010 ~ "2010 era",
                    year > 2010 ~ "modern era"),
    duration_min = duration_ms/60000) %>%
  select(-year, -track_album_id, -playlist_id, -duration_ms)
spotify_songs$year <- as.numeric(format(spotify_songs$track_album_release_date, "%Y"))


#duration

#the distribution of duration in each Genre and Era:

#using ggplot geom_density
spotify_songs %>%
  na.omit(era) %>%
  ggplot() +
  geom_density(aes(duration_min, fill=playlist_genre)) +
  facet_grid(era~playlist_genre) +
  theme_minimal() +
  labs(x='duration_min') +
  theme(legend.position = 'none')

#As we can see the duration is relatively similar in all eras however
# in 2010's era we can see edm tracks have a longer duration than other eras.
#Also in the modern era rap songs tend to be shorter compared to the other eras



#duration popularity throughout time

spotify_songs %>%
  na.omit(era) %>% 
ggplot(aes(x=duration_min, y=track_popularity, col=era))+ 
  geom_jitter(width = 0.2, alpha = 0.5, shape = 16)+
  facet_grid(era ~ .)



#loudness and genre 
#boxplot and density


ggplot(spotify_songs, aes(x = factor(playlist_genre), y =loudness, fill = playlist_genre)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  geom_boxplot() +
  guides(fill = guide_legend(title = "genre"))+ xlab("genre")




spotify_songs %>%
  na.omit() %>%
  ggplot() +
  geom_density(aes(loudness, fill=playlist_genre)) +
  facet_grid(~playlist_genre) +
  theme_light() +
  labs(x='loudness') +
  theme(legend.position = 'none')

















