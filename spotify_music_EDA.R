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

# genre and quartile relationship
ggplot(tentile_popularity, aes(Var2, Freq, color = Var1, group = Var1)) +
  geom_point() +
  # geom_col(position = "fill") +
  scale_y_continuous("Proportion") +
  geom_line() +
  # geom_step() +
  # geom_path() +
  NULL

ggplot(songs, aes(year , facets = playlist_genre)) +
  geom_density(alpha = 0.3) +
  # coord_cartesian(xlim = c(-20, 5)) +
  facet_wrap(. ~ playlist_genre) +
  NULL

# ggplot(songs, aes(year )) +
#   geom_density(alpha = 0.3) +
#   # coord_cartesian(xlim = c(-20, 5)) +
#   NULL


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
# 1
songs %>% 
  group_by(decade) %>% 
  summarise(avg_popularity) %>% 
  
  ggplot(aes(decade, avg_popularity)) +
  geom_violin()
  
# 2
songs %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_violin()

# 3 all
songs %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  geom_violin()

# 3 most popular %50
# I HAVE SOMETHING GOOD HERE. COME BACK TO IT IF YOU HAVE TIME 
# the most popular %50 of artists- is their tracks' performance more consistent?
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(avg_popularity, 2)) == "(45.4,90.7]") %>% 
  filter(top_50th_percentile) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL

songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = !as.character(quantcut(avg_popularity, 2)) == "(45.4,90.7]") %>% 
  filter(top_50th_percentile) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL


# 4 all
# best plot so far (it and #3)
# it tells us that artist's performance on track_popularity
# is more consistent in recent years 
songs %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, range_popularity)) +
  geom_violin()

#4 most popular %50
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, range_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL


songs %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, performed_tracks)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  coord_cartesian(ylim = c(0, 40)) +
  NULL

# copy of above but with genre instead of decade ---------

# 3 all
songs %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  geom_violin()

# 3 most popular %50

songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL


# 4 all
# best plot so far (it and #3)
# it tells us that artist's performance on track_popularity
# is more consistent in recent years 
songs %>%     # rock tracks have a large range -- wait a second could this be 
              # influenced by the fact rock tracks are more likely to be produced
              # in older decades, which is a confounding variable that is 
              # assosiated with both rock tracks and high range_popularity
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, range_popularity)) +
  geom_violin()

#4 most popular %50
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, range_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL


songs %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, performed_tracks)) +
  # geom_violin() +
  geom_point(alpha = 0.8, position = "jitter") +
  coord_cartesian(ylim = c(0, 40)) +
  NULL






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


songs %>% 
  group_by(playlist_genre) %>% 
  # summarise(track_popularity) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  # geom_violin()
  geom_boxplot()



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


# dead end ----------
# songs_exclusive$year <- as.numeric(songs_exclusive$year) 
# songs_exclusive %>% 
#   select(- c(track_id, track_name, track_artist, track_album_id,
#          track_album_name, track_album_name,
#          track_album_release_date, playlist_name, playlist_id,
#          playlist_genre, playlist_subgenre, decade)) %>% 
#   cor() -> corr_matrix
# 
# corr_matrix %>% 
#   corrplot()
# 
# corr_matrix[1,] %>% 
#   abs() %>% 
#   sort()


# most important attributes to predicting track_popularity
# - instrumentallness  (negatively correlated)
# - duration  (negatively correlated)
# - energy  (negatively correlated)
# - acousticness
# - danceability
# - number of performed tracks 
# - loudness
# - liveness (negatively correlated)







# finalists -----------

# best plot so far
songs %>% 
  filter(year > 1960) %>% 
ggplot(aes(x = track_popularity, fill = playlist_genre)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_y_continuous("Proportion")



# genre x year proportion: rock was very popular in the past
# Viz c -----------------
songs %>% 
  filter(year > 1965) %>% 
ggplot(aes(x = year, fill = playlist_genre)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_y_continuous("Proportion") -> genre_year_prop


# density plot genre x year
ggplot(songs, aes(year , facets = playlist_genre)) +
  geom_density(alpha = 0.3) +
  # coord_cartesian(xlim = c(-20, 5)) +
  facet_wrap(. ~ playlist_genre) +
  NULL

# track_popularity x decade
songs %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_violin() +
  NULL

songs %>% 
  group_by(playlist_genre) %>% 
  # summarise(track_popularity) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  # geom_violin()
  geom_boxplot() -> stddev_popularity_genre


# Viz A --------
# it tells us that artist's performance on track_popularity
# is more consistent in recent years 
songs %>% 
  filter(!performed_tracks == 1) %>%
  filter(track_popularity > 5) %>% 
  filter (!is.na(decade)) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, range_popularity)) +
  # geom_violin() 
  geom_boxplot() -> decade_popularity_range


songs %>% 
  filter(!performed_tracks == 1) %>% 
  filter(track_popularity > 5) %>% 
  filter (!is.na(decade)) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  # geom_violin() 
  geom_boxplot() + 
  labs(
    x = "Decade",
    y = "Standard Deviation of Popularity",
    title = "Popularity Avg. Std. Deviation by Decade"
  ) -> decade_popularity_stddev


# genre x popularity range -----------


# 4 all
# best plot so far (it and #3)
# it tells us that artist's performance on track_popularity
# is more consistent in recent years 
songs %>%     # rock tracks have a large range -- wait a second could this be 
  # influenced by the fact rock tracks are more likely to be produced
  # in older decades, which is a confounding variable that is 
  # assosiated with both rock tracks and high range_popularity
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, range_popularity)) +
  geom_violin()

#4 most popular %50
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, range_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL

# section -------------
#4 most popular %50
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  # filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  geom_violin() +
  facet_wrap(. ~ top_50th_percentile) +
  # geom_point(alpha = 0.2, position = "jitter") +
  coord_cartesian(ylim = c(0, 40))
NULL


songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  # filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(playlist_genre, stddev_popularity)) +
  # geom_violin() +
  geom_boxplot() +
  # facet_wrap(. ~ top_50th_percentile) +
  # geom_point(alpha = 0.2, position = "jitter") +
  coord_cartesian(ylim = c(0, 40))
NULL

# Viz B ----------
songs %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = 
           as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  group_by(top_50th_percentile, playlist_genre) %>% 
  summarise(avg_stddev = mean(stddev_popularity)) %>% 
  arrange(top_50th_percentile, avg_stddev) %>% 
  
  ggplot(aes(top_50th_percentile, avg_stddev, color = playlist_genre, group = playlist_genre)) +
  geom_point() +
  geom_line() + 
  labs (
    x = "Top 50th Percentile?",
    y = "Average Standard Deviation", 
    title = "Artist-Popularity & Genre Relationship with Profit Variation"
  ) -> stddev_artist_popularity_genre




# different section -----------

# track popularity and genre
songs %>% 
  group_by(playlist_genre) %>% 
  summarise(track_popularity) %>% 
  
  ggplot(aes(playlist_genre, track_popularity)) +
  geom_violin()

decade_popularity_stddev

# saving plots to files
ggsave(filename = "decade_popularity_range.jpg", plot = decade_popularity_range)
ggsave(filename = "decade_popularity_stddev.jpg", plot = decade_popularity_stddev)
ggsave(filename = "genre_year_prop.jpg", plot = genre_year_prop)
ggsave(filename = "stddev_popularity_genre.jpg", plot = stddev_popularity_genre)
ggsave(filename = "stddev_artist_popularity_genre.jpg", plot = stddev_artist_popularity_genre)





