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

#duration

##########
spotify_songs %>%
  na.omit(era) %>%
  ggplot() +
  geom_density(aes(duration_min, fill=playlist_genre)) +
  facet_grid(era~playlist_genre) +
  theme_minimal() +
  labs(x='duration_min') +
  theme(legend.position = 'none')

#duration popularity throughout time
ggplot(spotify_songs, aes(x=duration_min, y=track_popularity, col=era))+ 
  geom_jitter( size = 1.5, alpha(0.25) )+ facet_grid(era ~ .)







#loudness

#boxplot

ggplot(data = spotify, mapping = aes(x = playlist_genre, y=loudness, fill=playlist_genre, alpha=0.05)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Playlist Genres",
       y = "loudness",
       title = "Distribution of loudness by using boxpplot")




ggplot(spotify_songs, aes(x=loudness, y=track_popularity, col=playlist_genre))+ 
  geom_jitter( size = 1.5 )+ facet_wrap(playlist_genre ~ .)











