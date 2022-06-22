# Load the packages
library(tidyverse)
library(ggplot2)





spotify <- read.csv("data/spotify_songs.csv")

dim(spotify)
colnames(spotify)  
head(spotify, 4)
summary(spotify)
View(spotify)
# save the track_album_release_date as a Date
spotify$track_album_release_date <- as.Date(spotify$track_album_release_date)

#extract the year from the date
spotify$year <- as.numeric(format(spotify$track_album_release_date, "%Y"))

spotify$duration_min <- spotify$duration_ms/60000
View(spotify)

ggplot(spotify, aes(x = duration_min, y = track_popularity)) + 
  geom_jitter(shape = 1) + 
  facet_grid(playlist_genre ~ .)


############################################
data1960s <- spotify %>%
  filter(year < 1970 & year > 1959)
ggplot(data1960s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1960s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1970s <- spotify %>%
  filter(year < 1980 & year > 1969)
ggplot(data1970s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1970s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1980s <- spotify %>%
  filter(year < 1990 & year > 1979)
ggplot(data1980s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1980s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1990s <- spotify %>%
  filter(year < 2000 & year > 1989)
ggplot(data1990s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1990s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
################################################
data2000s <- spotify %>%
  filter(year < 2010 & year > 1999)
ggplot(data2000s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 2000s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data2010s <- spotify %>%
  filter(year < 2020 & year > 2010)
ggplot(data2010s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 2010s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


decadedata <- rbind(data1960s, data1970s, data1980s, data1990s, data2000s, data2010s)
ggplot(decadedata, aes(x=duration_min, y=track_popularity, col=year))+ 
  geom_jitter( size = 1.5 )+ facet_wrap(year ~ .)






