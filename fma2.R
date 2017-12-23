#load the packages
library(dplyr)
library(plyr)
library(ggplot2)

#read the datasets change the path!
genre_csv <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/genres.csv", header = TRUE)
track_csv <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/tracks.csv", header = TRUE, stringsAsFactors=FALSE)

#parent_genre <- subset(parent_genre, !(parent_genre$genre_id %in% top_genre$genre_id))

#dataset heading changing
track_csv[1,1] = track_csv[2,1]
track_csv = track_csv[-2,]
colnames(track_csv) = track_csv[1,]
track_csv = track_csv[-1,]
track_csv$date_released <- substring(track_csv$date_released,1,4)
track_csv$date_released
#select attributes
track_select <- subset(track_csv, select = c(1,4,34,41,42,43,48))

#remove the brackets
track_select$genres <- gsub("\\[|\\]", "", track_select$genres)
track_select$genres_all <- gsub("\\[|\\]", "", track_select$genres_all)

#keep the years data
track_select$date_released <- substring(track_select$date_released,1,4)

#remove the blank values
track_select[track_select == ""] <- NA
track_select <- track_select[complete.cases(track_select),]

#change datatype
track_select$bit_rate <- as.numeric(track_select$bit_rate)
track_select$listens <- as.numeric(track_select$listens)
#Genres dataset pre processing
#Find all top level
top_genre <- genre_csv[genre_csv$genre_id %in% genre_csv$top_level,]
#Find all parent level
parent_genre <- genre_csv[genre_csv$genre_id %in% genre_csv$parent,]


#calculate the top genres tracks
genre_count <- aggregate(track_select$track_id, by=list(track_select$date_released, track_select$genre_top),length)
#calculate the top genres bit rate
avg_bit_rate <- aggregate(track_select$bit_rate, by=list(track_select$date_released, track_select$genre_top),mean)
#calculate the top genres listens
sum_listen <- aggregate(track_select$listens, by=list(track_select$date_released, track_select$genre_top),sum)
avg_listen <- aggregate(track_select$listens, by=list(track_select$date_released, track_select$genre_top),mean)

#rename the colnum name
names(genre_count)<-c("year","top_genre","count")
names(avg_bit_rate)<-c("year","top_genre","average_bit_rate")
names(sum_listen)<-c("year","top_genre","total_listen")
names(avg_listen)<-c("year","top_genre","average_listen")


#build facet gird plot
genre_tracks_plot <- ggplot(genre_count, aes(x = genre_count$year, y = genre_count$count)) + 
                     geom_line(aes(color = genre_count$top_genre, group = genre_count$top_genre)) +
                     labs(title = "Top genres distribution ", x = "Year", y = "Count of tracks", color = "Top genres") + 
                     facet_grid(genre_count$top_genre ~.)

genre_bit_rate_plot <- ggplot(avg_bit_rate, aes(x = avg_bit_rate$year, y = avg_bit_rate$average_bit_rate)) + 
                       geom_line(aes(color = avg_bit_rate$top_genre, group = avg_bit_rate$top_genre)) +
                       labs(title = "Top genres distribution ", x = "Year", y = "Count of tracks", color = "Top genres") + 
                       facet_grid(avg_bit_rate$top_genre ~.)

genre_avg_listen_plot <- ggplot(avg_listen, aes(x = avg_listen$year, y = avg_listen$average_listen)) + 
                         geom_line(aes(color = avg_listen$top_genre, group = avg_listen$top_genre)) +
                         labs(title = "Top genres distribution ", x = "Year", y = "Count of tracks", color = "Top genres") + 
                         facet_grid(avg_listen$top_genre ~.)

genre_sum_listen_plot <- ggplot(sum_listen, aes(x = sum_listen$year, y = sum_listen$total_listen)) + 
                         geom_line(aes(color = sum_listen$top_genre, group = sum_listen$top_genre)) +
                         labs(title = "Top genres distribution ", x = "Year", y = "Count of tracks", color = "Top genres") + 
                         facet_grid(sum_listen$top_genre ~.)


#match specific tracks by sub genres
sub_genre <- genre_csv[genre_csv$parent %in% c('15'),]
Electronic_song <- track_select[track_select$genres %in% sub_genre$genre_id,]
Electronic_song_count <- aggregate(Electronic_song$track_id, by=list(Electronic_song$date_released, Electronic_song$genres), length)

names(Electronic_song_count)<-c("year","genre","count")
Electronic_song_count$title = genre_csv$title[match(Electronic_song_count$genre, genre_csv$genre_id)]

sub_genreg_cal <- ggplot(Electronic_song_count, aes(x = Electronic_song_count$year, y = Electronic_song_count$count)) + 
                  geom_point(aes(color = Electronic_song_count$title, group = Electronic_song_count$genre)) +
                  labs(title = "Sub genre distribution ", x = "Year", y = "Values", color = "Genres") + 
                  facet_grid(Electronic_song_count$genre ~.) +
                  theme(axis.text.x = element_text(angle=90,hjust=1))
sub_genreg_cal

#Temporal chart
track_csv$genres <-  gsub("\\[|\\]", "", track_csv$genres)
track_csv$genre_name = genre_csv$title[match(track_csv$genres, genre_csv$genre_id)]
track_csv <- subset(track_csv, genre_name != "NA")
track_csv <- subset(track_csv, date_released != "")
ggplot(subset(track_csv, genre_top == "Rock"), aes(date_released,genre_name, colour = genre_top)) + geom_point() + theme(axis.text.x = element_text(angle=90,hjust=1))  


#Sunburst chart
merge_csv <- read.csv(file = "/Users/junwenhe/Desktop/fma_metadata/genres.csv", header = TRUE)
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
#library(sunburstR)
#library(ggsunburst)
top_level <- count(merge_csv$top_level)
top_level$freq <- top_level$freq / sum(top_level$freq)
names(top_level)[1]<-paste("genre_id")
top_level3 <- merge(top_level, merge_csv, by = "genre_id")
top_level3 <- subset(top_level3, select = c(1,5))
names(top_level3)[1]<-paste("top_level")
names(top_level3)[2]<-paste("top_title")

parent_level <- count(merge_csv$parent)
parent_level$freq <- parent_level$freq / sum(parent_level$freq)
parent_level <- subset(parent_level, parent_level$x != 0)
names(parent_level)[1]<-paste("genre_id")
parent_level2 <- parent_level[!(parent_level$genre_id %in% top_level$genre_id),]
parent_level2 <- merge(parent_level2, merge_csv, by = "genre_id")
parent_level3 <- subset(parent_level2, select = c(1,5,6))
names(parent_level3)[1]<-paste("parent")
names(parent_level3)[2]<-paste("parent_title")

bottom_level <- merge_csv[!(merge_csv$genre_id %in% parent_level3$genre_id),]
bottom_level <- bottom_level[!(bottom_level$genre_id %in% top_level3$genre_id),]
bottom_level <- subset(bottom_level, select = c(1,4,3))

bottom_mid <- left_join(parent_level3, bottom_level)
all_genre <- left_join(top_level3, bottom_mid)

all_genre1 <- all_genre %>% 
  mutate(Index = 1) %>%
  group_by(top_title)

First <- ggplot(all_genre1) + geom_bar(aes(x=1, y=top_title, fill=top_title), position='stack', stat='identity', size=0.20) +
  theme(panel.grid = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text = element_blank(), legend.position="none") +
  labs(fill = "Genre")

Second <- First + geom_bar(data=all_genre1, aes(x=2, y=top_title, fill=parent_title, group=top_title), position='stack', stat='identity', size=0.20, colour = "black") +
  theme(axis.title=element_blank(), axis.ticks=element_blank(), axis.text = element_blank())

Third <- Second + geom_bar(data=all_genre1, aes(x=3, y=top_title, fill=title), position='stack', stat='identity', size=0.20, colour = "black") +
  theme(axis.title=element_blank(), axis.ticks=element_blank(), axis.text.y = element_blank(), panel.background = element_rect(fill = "white"))

Third + coord_polar("y")
#############################
