# Load the packages
library(tidyverse)
library(skimr)

songs <- read_csv("data/spotify_songs.csv")

# view general statistics on the data set
skim(songs)


# what genre is most prevelent? 
ggplot(songs, aes(factor(playlist_genre))) +
  geom_bar()


ggplot(songs, aes(track_popularity)) + 
  geom_histogram()

songs_exclusive <- songs %>% 
  filter(track_popularity > 5)

songs_amatures <- songs %>% 
  filter(track_popularity < 5)

ggplot(songs, aes(x = track_popularity, color = playlist_genre)) + 
  geom_freqpoly()

# which genre is most popular?
ggplot(songs, aes(x = track_popularity, color = playlist_genre)) + 
  geom_freqpoly() +
  coord_cartesian(ylim = c(0, 500)) + 
  coord_cartesian(xlim = c(20, 120)) + 
  NULL

# rock genre has very interesting popularity distribution (right-hand tail)
songs %>% 
  filter(playlist_genre == "rock") %>% 
  filter(track_popularity > 5) %>% 

ggplot(aes(track_popularity)) +
  geom_histogram()


# compared to pop genre

songs %>% 
  filter(playlist_genre == "pop") %>% 
  filter(track_popularity > 5) %>% 
  
  ggplot(aes(track_popularity)) +
  geom_histogram()


# what genres are most popular amongst amature creators?





















