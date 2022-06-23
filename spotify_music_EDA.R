# Load the packages
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(GGally)
library(janitor)
library(here)
library(corrplot)
library(plotly)
library(scales)
library(ggrepel)
library(lubridate)
library(gtools)
install.packages('corrgram')



#header=TRUE, stringsAsFactors=FALSE)


library(readr)

SpotifyData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
SpotifyData <- tidyverse::as_tibble(SpotifyData)

#glimpse
dplyr::glimpse(SpotifyData)

#summary
summary(SpotifyData)

#--- do certain release dates (months) affect popularity? ------

#first split the formatting of the the date 
library(data.table)
setDT(SpotifyData)[, c("Year", "Month", "Day") := 
             c(tstrsplit(track_album_release_date, "-", type.convert = TRUE))]
#head(SpotifyData)
#names(SpotifyData)

#--------------------

# overall corr 
featuresCorr <- dplyr::select(SpotifyData , track_popularity, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo ,key)
corrgram::corrgram(cor(featuresCorr),order = TRUE, upper.panel =  NULL)

#genre nad release date
TopMonth=SpotifyData %>%
  select(-c(("track_id"),("track_album_id"),("playlist_name"),("playlist_id"),("duration_ms")))

TopMonth$track_album_release_date=as.Date(TopMonth$track_album_release_date)

TopMonth=TopMonth %>%
  mutate(month=month(TopMonth$track_album_release_date),year=year(TopMonth$track_album_release_date)) %>%
  select(-c("track_album_release_date"))

TopMonth =TopMonth%>%
  mutate(period = case_when(
      month == 1  ~ 'january'
    , month == 2  ~ 'february'
    , month == 3  ~ 'march'
    , month == 4  ~ 'april'
    , month == 5  ~ 'may'
    , month == 6  ~ 'june'
    , month == 7  ~ 'july'
    , month == 8  ~ 'august'
    , month == 9  ~ 'september'
    , month == 10  ~ 'october'
    , month == 11  ~ 'november'
    , month == 12  ~ 'december'))

TopMonth= TopMonth %>%
  group_by(period) %>%
  count(playlist_genre)

TopMonth = na.omit(TopMonth)

TopMonth$period = factor(TopMonth$period, levels = c('january', 'february', 'march', 'april', 'may', 'june' ,'july' ,'august', 'september', 'october', 'november' ,'december'))

library("ggplot2")
ggplot(TopMonth,aes(x=period,y=n, group = playlist_genre))+
  geom_point(aes(colour=playlist_genre))+
  geom_line(aes(colour=playlist_genre))+
  xlab("Month")+ylab("Number of songs")+
  ggtitle("Number of songs released during each month") 
 
#are tracks with speech/words more popular than instrumental ?

SpotifyData %>% filter(speechiness > .80)  %>% pull(track_popularity) %>% median()
SpotifyData %>% filter(instrumentalness	 > .80)  %>% pull(track_popularity) %>% median()

#sultan-----------------

#Is there any correlation between Energy and Danceability?
cor(SpotifyData$energy , SpotifyData$danceability)
#The correlation between Energy and Danceability is = -0.086
corrplot::corrplot(cor(SpotifyData[c("danceability","energy")]), method="color",type="upper" ,tl.srt=90,tl.col="black")

#tibble(SpotifyData$track_artist)
#tibble(SpotifyData$track_artist) 
# group_by(SpotifyData$track_album_release_date)


#Top Artist with the most track released

highest_tracks <- SpotifyData %>% group_by(Artis_Name = track_artist) %>%
  summarise(No_of_tracks = n()) %>%
  arrange(desc(Artis_Name , No_of_tracks)) %>%
  top_n(15, wt = No_of_tracks) %>% 
  ggplot(aes(x = reorder(Artis_Name , - No_of_tracks), y = No_of_tracks)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Top Artists having the most track releases", x = "Artist Name", y = "Number of Tracks")
ggplotly(highest_tracks)

#dina-----------------------------

dim(SpotifyData)
colnames(SpotifyData)  

summary(SpotifyData)
View(SpotifyData)
SpotifyData <- SpotifyData %>% 
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

spotify_SpotifyData <- SpotifyData %>% 
  mutate(
    track_album_release_date = as.Date(track_album_release_date),
    year = as.numeric(format(track_album_release_date,'%Y')),
    era = case_when(year <= 2000 ~ "under 00s era",
                    year > 2000 & year <= 2010 ~ "2010 era",
                    year > 2010 ~ "modern era"),
    duration_min = duration_ms/60000) %>%
  select(-year, -track_album_id, -playlist_id, -duration_ms)

SpotifyData %>%
  na.omit(era) %>%
  ggplot() +
  geom_density(aes(duration_min, fill=playlist_genre)) +
  facet_grid(era~playlist_genre) +
  theme_minimal() +
  labs(x='duration_min') +
  theme(legend.position = 'none')

#duration popularity throughout time
ggplot(SpotifyData, aes(x=duration_min, y=track_popularity, col=era))+ 
  geom_jitter( size = 1.5, alpha(0.25) )+ facet_grid(era ~ .)


#loudness

#boxplot

ggplot(data = SpotifyData, mapping = aes(x = playlist_genre, y=loudness, fill=playlist_genre, alpha=0.05)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Playlist Genres",
       y = "loudness",
       title = "Distribution of loudness by using boxpplot")


ggplot(SpotifyData, aes(x=loudness, y=track_popularity, col=playlist_genre))+ 
  geom_jitter( size = 1.5 )+ facet_wrap(playlist_genre ~ .)

 #--------------------------

##### Ahmed's EDA: Pie Chart of Genre/Popularity ------

## Genre: make the bar plot first then turn it into a pie chart

SpotifyData  %>%
  group_by(playlist_genre)  %>% 
  summarise(pop = (n() / nrow(SpotifyData)) * 100) -> genre_df

print(sum(genre_df$pop))

gen  <- ggplot(genre_df, aes(x= "", y= pop, fill=playlist_genre)) +
  geom_bar(width = 1, stat = "identity")

# turn it into a pie chart
gen_pie  <- gen + coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(pop/100)), size=6, position = position_stack(vjust = 0.5)) +
  NULL

gen_pie

## Popularity: make the bar plot first then turn it into a pie chart

# get the total sum of popularity in each genre
SpotifyData  %>%
  group_by(playlist_genre)  %>% 
  summarise(pop = sum(track_popularity)) -> popularity_df

# get the total value for popularity in the dataset
total_popularity  <- sum(popularity_df$pop)

# modify the popularity df
popularity_df  <- popularity_df  %>% 
  mutate(pop = (pop / total_popularity) * 100)

print(sum(popularity_df$pop))

# now create the bar plot
pop_bar  <- ggplot(popularity_df, aes(x= "", y= pop, fill=playlist_genre)) +
  geom_bar(width = 1, stat = "identity")

# turn it into a pie chart
pop_pie  <- pop_bar + coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(pop/100)), size=6, position = position_stack(vjust = 0.5)) +
  NULL
pop_pie


## A question arises: Does the genre affect the duration of the song?

# plot the graph
SpotifyData  %>%
  ggplot(aes(x=(duration_ms/ 60000), fill= playlist_genre)) +
  labs(title = "Genre and Duration", y= "Density" , x= "Duration (min)") +
  geom_density(alpha= 0.4, color=NA) -> gen_dur 


# saving the plots/charts
ggsave(filename = "genre_pie.jpg", plot = gen_pie)
ggsave(filename = "popularity_pie.jpg", plot = pop_pie)
ggsave(filename = "genre_duration.jpg", plot = gen_dur)

_________________________________

SpotifyData %>% 
  filter(track_popularity > 70) -> songs_popular

ggplot(songs_popular, aes(track_popularity, fill = playlist_genre))  +
  geom_histogram(binwidth = 10, position = "dodge")

ggplot(songs_popular, aes(playlist_genre, track_popularity)) +
  geom_boxplot()

ggplot(SpotifyData, aes(playlist_genre, track_popularity)) +
  geom_boxplot()

#mohammed---------------------------------------

# which genre is most popular?
ggplot(SpotifyData, aes(x = track_popularity, ..density.., color = playlist_genre)) + 
  geom_freqpoly() +
  coord_cartesian(ylim = c(0, 500)) + 
  coord_cartesian(xlim = c(20, 120)) + 
  NULL

# rock genre has very interesting popularity distribution (right-hand tail)
SpotifyData %>% 
  filter(playlist_genre == "rock") %>% 
  filter(track_popularity > 5) %>% 
  
  ggplot(aes(track_popularity)) +
  geom_histogram()


# compared to pop genre

SpotifyData %>% 
  filter(playlist_genre == "pop") %>% 
  filter(track_popularity > 5) %>% 
  
  ggplot(aes(track_popularity)) +
  geom_histogram()

# what genres are most popular amongst top songs? 
SpotifyData %>% 
filter(track_popularity > 70) -> songs_popular

ggplot(songs_popular, aes(track_popularity, fill = playlist_genre))  +
  geom_histogram(binwidth = 10, position = "dodge")

# versatility of artists -----------

SpotifyData %>% 
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
  ungroup() -> SpotifyData


# what decade is most prevelent in our dataset?
SpotifyData %>% 
  # filter(num_performed_tracks > 1) %>% 
  # filter(decade == "10s") %>% 
  
  ggplot(aes(x = num_performed_tracks, color = decade)) +
  geom_freqpoly() +
  # coord_cartesian(ylim = c(0, 1000)) +
  NULL

# what is the distribution of popularity based on release decade
SpotifyData %>% 
  group_by(decade) %>% 
  summarise(track_popularity) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_point(position = "jitter", shape = 16, alpha = 0.3)

# popularity by decade --------
# in the 10s there were some artists who consistently
# had extremely high avg_popularity
SpotifyData %>% 
  group_by(decade) %>% 
  summarise(avg_popularity) %>% 
  
  ggplot(aes(decade, avg_popularity)) +
  geom_violin()

SpotifyData %>% 
  group_by(decade) %>% 
  summarise(track_popularity) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_violin()


# what genre is most prevelent? 
ggplot(SpotifyData, aes(factor(playlist_genre))) +
  geom_bar()


ggplot(SpotifyData, aes(track_popularity)) + 
  geom_histogram()

songs_exclusive <- SpotifyData %>% 
  filter(track_popularity > 5)

songs_amatures <- SpotifyData %>% 
  filter(track_popularity < 5)

songs_middle <- SpotifyData %>% 
  filter(track_popularity > 5 & track_popularity < 70)

ggplot(SpotifyData, aes(x = track_popularity, y = ..density.., color = playlist_genre)) + 
  geom_freqpoly()


# popularity by decade --------
# in the 10s there were some artists who consistently
# had extremely high avg_popularity
# 1
SpotifyData %>% 
  group_by(decade) %>% 
  summarise(avg_popularity) %>% 
  
  ggplot(aes(decade, avg_popularity)) +
  geom_violin()

# 2
SpotifyData %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, track_popularity)) +
  geom_violin()

# 3 all
SpotifyData %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  geom_violin()

# 3 most popular %50
# I HAVE SOMETHING GOOD HERE. COME BACK TO IT IF YOU HAVE TIME 
# the most popular %50 of artists- is their tracks' performance more consistent?
SpotifyData %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(avg_popularity, 2)) == "(45.4,90.7]") %>% 
  filter(top_50th_percentile) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, stddev_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL

SpotifyData %>% 
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
SpotifyData %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, range_popularity)) +
  geom_violin()

#4 most popular %50
SpotifyData %>% 
  filter(!performed_tracks == 1) %>% 
  mutate(top_50th_percentile = as.character(quantcut(track_popularity, 2)) == "(49,100]") %>% 
  filter(top_50th_percentile == TRUE) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, range_popularity)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  NULL


SpotifyData %>% 
  filter(!performed_tracks == 1) %>% 
  group_by(decade) %>% 
  
  ggplot(aes(decade, performed_tracks)) +
  geom_violin() +
  # geom_point(alpha = 0.2, position = "jitter") +
  coord_cartesian(ylim = c(0, 40)) +
  NULL

