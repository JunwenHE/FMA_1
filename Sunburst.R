genre <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/genres.csv", header = TRUE)
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
library(jsonlite)
##Output the genre csv as JSON file
top_level_track <- subset(genre, genre$parent == 0)

parent_level_track <- genre[genre$genre_id %in% genre$parent,]
parent_level_track <- subset(parent_level_track, parent_level_track$parent != 0)

sub_level_track <- genre[!(genre$genre_id %in% top_level_track$genre_id),]
sub_level_track <- sub_level_track[!(sub_level_track$genre_id %in% parent_level_track$genre_id),]

new_genre <- arrange(genre, genre$top_level, genre$parent)
sum(top_level_track$X.tracks)

x <- toJSON(new_genre)
x <- toJSON(new_genre, pretty = TRUE)
write_json(x, "/Users/junwenhe/Desktop/top_genre.json")