library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)

merge_csv <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/genres.csv", header = TRUE)
track_csv <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/tracks.csv", header = TRUE, stringsAsFactors=FALSE)
track_csv[1,1] = track_csv[2,1]
track_csv = track_csv[-2,]

Punk <- subset(merge_csv, merge_csv$genre_id == 15 | merge_csv$parent == 15)
track_csv$track.8 <- gsub("\\[|\\]", "", track_csv$track.8)
track_punk <- subset(track_csv, track_csv$track.8 == "15"| track_csv$track.8 == "42"| track_csv$track.8 == "181"| track_csv$track.8 == "182"| track_csv$track.8 == "183"| track_csv$track.8 == "184"
                     | track_csv$track.8 == "185"| track_csv$track.8 == "236"|track_csv$track.8 == "286" | track_csv$track.8 == "296"| track_csv$track.8 == "297"|track_csv$track.8 == "337"| track_csv$track.8 == "468"|track_csv$track.8 == "495"|track_csv$track.8 == "695")

track_punk2 <- subset(track_punk, select = c(4,41,42,53))

track_punk4 <- track_punk2[!(is.na(track_punk2$album.2) | track_punk2$album.2==""),]
track_punk4$album.2 <- substring(track_punk4$album.2,1,4)
track_punk5 <- data.frame(table(track_punk4$album.2, track_punk4$track.8))
names(track_punk5)<-c("Year","title","Count")
track_punk5[["title"]] <- Punk[ match(track_punk5[['title']], Punk[['genre_id']] ) , 'title']
#change column name
names(track_punk5)<-c("Year","title","Count")

track_punk5 <- merge(track_punk5, Punk, by = "genre_id")
track_punk5 <- subset(track_punk5, select = c(1,2,3,6))

#produce chart
ggplot(track_punk5, aes(x = track_punk5$Year, y = track_punk5$Count)) + 
  geom_line(aes(color = track_punk5$title, group = track_punk5$title)) +
  labs(title = "Electronic songs ", x = "Year", y = "Count of songs", color = "Genres") + facet_grid(track_punk5$title ~.)

#Genres classification
top_genre_classify <- aggregate( merge_csv$parent, by=list(top_level = merge_csv$top_level), simplify=TRUE, count)
parent_genre_classify <- aggregate( merge_csv$genre_id, by=list(parent_genre = merge_csv$parent), simplify=TRUE, count)

#genre id conver to genre title
#filtering track dataset by another genre dataset, using match function or %in% etc. 

Electronic_genre <- merge_csv[merge_csv$top_level %in% c('15'),]
Electronic_song <- track_csv[track_csv$track.8 %in% Electronic_genre$genre_id,]
Electronic_song$album.2 <- substring(Electronic_song$album.2,1,4)
Electronic_count <- aggregate(track.19 ~ track.8 + album.2 , data = Electronic_song, length)
#Electronic_count[["track.8"]] <- Electronic_genre[ match(Electronic_count[['track.8']], Electronic_genre[['genre_id']] ) , 'title']
#change column name
names(Electronic_count)<-c("genre_id","year","count")

Electronic_count$genre_id <- as.factor(Electronic_count$genre_id)
Electronic_count$year <- as.factor(Electronic_count$year)
Electronic_count[Electronic_count == ""] <- NA
Electronic_count <- Electronic_count[complete.cases(Electronic_count),]

ggplot(Electronic_count, aes(x = Electronic_count$year, y = Electronic_count$count)) + 
  geom_line(aes(color = Electronic_count$genre_id, group = Electronic_count$genre_id)) +
  labs(title = "Electronic songs ", x = "Year", y = "Count of songs", color = "Genres") + facet_grid(Electronic_count$genre_id ~.)
