# Load the packages
library(tidyverse)
library(skimr)
library(corrplot)
library(scales)
library(ggrepel)
library(gtools)


songs <- read_csv("data/songs_songs.csv")

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

songs_middle <- songs %>% 
  filter(track_popularity > 5 & track_popularity < 70)

ggplot(songs, aes(x = track_popularity, y = ..density.., color = playlist_genre)) + 
  geom_freqpoly()

# which genre is most popular?
ggplot(songs, aes(x = track_popularity, ..density.., color = playlist_genre)) + 
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


# what genres are most popular amongst amature/popular creators? --------

  
ggplot(songs_amatures, aes(x = track_popularity, fill = playlist_genre)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Proportion")

ggplot(songs_popular, aes(x = track_popularity, fill = playlist_genre)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_y_continuous("Proportion")

ggplot(songs_middle, aes(x = track_popularity, fill = playlist_genre)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_y_continuous("Proportion")

# best plot so far
ggplot(songs, aes(x = track_popularity, fill = playlist_genre)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_y_continuous("Proportion")

songs %>% 
  group_by(quartile_popularity, playlist_genre) %>% 
  summarise(proportion = percent((n()/nrow(songs))))

# best table so far
table(songs$playlist_genre, songs$quartile_popularity) %>% 
  as.data.frame() -> tentile_popularity

ggplot(tentile_popularity, aes(Var2, Freq, color = Var1, group = Var1)) +
  geom_point() +
  # geom_col(position = "fill") +
  # scale_y_continuous("Proportion") +
  # geom_line() +
  # geom_step() +
  # geom_path() +
  NULL








songs_amatures %>% 
  select(track_popularity, playlist_genre) %>% 
  group_by(track_popularity, playlist_genre) %>% 
  summarise(count = n()) %>% 
  arrange(track_popularity, desc(count)) -> amatur_fav_genre

# interesting: pop genre is least popular amongst amature creators
ggplot(amatur_fav_genre, aes(track_popularity, count, fill = playlist_genre)) +
  geom_col(position = "dodge")


# what genres are most popular amongst top songs? --------

songs %>% 
  filter(track_popularity > 70) -> songs_popular

ggplot(songs_popular, aes(track_popularity, fill = playlist_genre))  +
  geom_histogram(binwidth = 10, position = "dodge")

ggplot(songs_popular, aes(playlist_genre, track_popularity)) +
  geom_boxplot()

ggplot(songs, aes(playlist_genre, track_popularity)) +
  geom_boxplot()

# assigned tasks ---------

# versatility of artists -----------


songs %>% 
  mutate(year = as.numeric(format(as.Date(track_album_release_date, format = "%Y-%m-%d"), "%Y"))) %>%
  # summarise(min(year, na.rm = TRUE), max = max(year, na.rm = TRUE))
  mutate(decade = cut(year, breaks = c(1960, 1969, 1979, 1989, 1999, 2009, 2019, 2029), 
                      labels = c("60s", "70s", "80s", "90s", "00s", "10s", "20s"))) %>% 
  group_by(track_artist) %>% 
  # summarise(avg_popularity = mean(track_popularity), range = diff(range(track_popularity)))
  mutate(range_popularity = diff(range(track_popularity))) %>% 
  mutate(avg_popularity = mean(track_popularity)) %>% 
  mutate(stddev_popularity = sd(track_popularity)) %>% 
  mutate(num_performed_tracks = n_distinct(track_id)) %>% 
  ungroup() %>% 
  mutate(quartile_popularity = quantcut(track_popularity, q = 10)) %>% 
  ungroup() -> songs

  

# what decade is most prevelent in our dataset?
songs %>% 
  # filter(num_performed_tracks > 1) %>% 
  # filter(decade == "10s") %>% 

ggplot(aes(x = num_performed_tracks, color = decade)) +
  geom_freqpoly() +
  # coord_cartesian(ylim = c(0, 1000)) +
  NULL

# what is the distribution of popularity based on release decade
songs %>% 
  group_by(decade) %>% 
  summarise(track_popularity) %>% 

ggplot(aes(decade, track_popularity)) +
  geom_point(position = "jitter", shape = 16, alpha = 0.3)

# popularity by decade --------
# in the 10s there were some artists who consistently
# had extremely high avg_popularity
songs %>% 
  group_by(decade) %>% 
  summarise(avg_popularity) %>% 
  
  ggplot(aes(decade, avg_popularity)) +
  geom_violin()
  
songs %>% 
  group_by(decade) %>% 
  summarise(track_popularity) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_violin()
  
# popularity by genre ----------
songs %>% 
  group_by(playlist_genre) %>% 
  summarise(avg_popularity) %>% 
  
  ggplot(aes(playlist_genre, avg_popularity)) +
  geom_violin()

songs %>% 
  group_by(playlist_genre) %>% 
  summarise(track_popularity) %>% 
  
  ggplot(aes(playlist_genre, track_popularity)) +
  geom_violin()

  


ggplot(songs, aes(x = duration_min, y = track_popularity)) + 
  geom_jitter(shape = 1) + 
  facet_grid(playlist_genre ~ .)


songs %>% 
  group_by(track_artist) %>% 
  mutate(performed_genres = n_distinct(playlist_genre)) %>% 
  ungroup() %>%
  group_by(decade) %>% 
  summarise(avg_number_genres = mean(performed_genres)) %>%
  # print()
  
ggplot(aes(decade, avg_number_genres)) +
  geom_col(position = "dodge")  



# table(songs$playlist_genre, songs$playlist_genre)



songs_exclusive$year <- as.numeric(songs_exclusive$year) 
songs_exclusive %>% 
  select(- c(track_id, track_name, track_artist, track_album_id,
         track_album_name, track_album_name,
         track_album_release_date, playlist_name, playlist_id,
         playlist_genre, playlist_subgenre, decade)) %>% 
  cor() -> corr_matrix

corr_matrix %>% 
  corrplot()

corr_matrix[1,] %>% 
  abs() %>% 
  sort()


# most important attributes to predicting track_popularity
# - instrumentallness  (negatively correlated)
# - duration  (negatively correlated)
# - energy  (negatively correlated)
# - acousticness
# - danceability
# - number of performed tracks 
# - loudness
# - liveness (negatively correlated)











songs$duration_min <- songs$duration_ms/60000

############################################
data1960s <- songs %>%
  filter(year < 1970 & year > 1959)
ggplot(data1960s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1960s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1970s <- songs %>%
  filter(year < 1980 & year > 1969)
ggplot(data1970s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1970s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1980s <- songs %>%
  filter(year < 1990 & year > 1979)
ggplot(data1980s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1980s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data1990s <- songs %>%
  filter(year < 2000 & year > 1989)
ggplot(data1990s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 1990s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
################################################
data2000s <- songs %>%
  filter(year < 2010 & year > 1999)
ggplot(data2000s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 2000s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
############################################
data2010s <- songs %>%
  filter(year < 2020 & year > 2010)
ggplot(data2010s, aes(x = duration_min, y = track_popularity)) + geom_jitter() +
  labs(title = "Relationship between Song Duration and Popularity from 2010s", y = "track_popularity", x = "Duration (min)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


decadedata <- rbind(data1960s, data1970s, data1980s, data1990s, data2000s, data2010s)
ggplot(decadedata, aes(x=duration_min, y=track_popularity, col=year))+ 
  geom_jitter( size = 1.5 )+ facet_wrap(year ~ .)























