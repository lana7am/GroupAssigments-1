# Load the packages
library(tidyverse)
library(scales)
library(ggrepel)

# initialize the dataset as a tibble
spotify_df  <- as_tibble(read.csv("data/spotify_songs.csv"))

##### Ahmed's EDA: Pie Chart of Genre/Popularity ------

## Genre: make the bar plot first then turn it into a pie chart

spotify_df  %>%
    group_by(playlist_genre)  %>% 
    summarise(pop = (n() / nrow(spotify_df)) * 100) -> genre_df

print(sum(genre_df$pop))

gen  <- ggplot(genre_df, aes(x= "", y= pop, fill=playlist_genre)) +
            geom_bar(width = 1, stat = "identity")

# turn it into a pie chart
gen_pie  <- gen + coord_polar("y", start = 0) +
            geom_text(aes(label = percent(pop/100)), size=6, position = position_stack(vjust = 0.5)) +
            NULL

gen_pie

## Popularity: make the bar plot first then turn it into a pie chart

# get the total sum of popularity in each genre
spotify_df  %>%
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
            geom_text(aes(label = percent(pop/100)), size=6, position = position_stack(vjust = 0.5)) +
            NULL
pop_pie


## A question arises: Does the genre affect the duration of the song?

# plot the graph
spotify_df  %>%
    ggplot(aes(x=(duration_ms / 60000), fill= playlist_genre)) +
        labs(title = "Genre and Duration", y= "Density" , x= "Duration (min)") +
        geom_density(alpha= 0.4, color=NA) -> gen_dur 


# saving the plots/charts
ggsave(filename = "genre_pie.jpg", plot = gen_pie)
ggsave(filename = "popularity_pie.jpg", plot = pop_pie)
ggsave(filename = "genre_duration.jpg", plot = gen_dur)


