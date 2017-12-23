#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    
    switch(input$result,
           "Tracks" = 1, 
           "Bit rate" = 3, 
           "Listens" = 8)
  })
  
  calculateInput <- reactive({
    
   switch(input$calculate,
          "count" = length, 
          "average" = mean,
          "sum" = sum)  
  })
  
  parentDataInput <- reactive({
    
    switch(input$larger_result,
           "Tracks" = 1, 
           "Bit rate" = 3, 
           "Listens" = 8)
  })
  
  parentCalculateInput <- reactive({
    
    switch(input$larger_calculate,
           "count" = length, 
           "average" = mean
           )  
  })
  
  getPage<-function() {
    
    return(includeHTML("index.html"))
  }
  
  
  
  output$genre_plot <- renderPlot({
    
    sub_genre <- subset(genre_csv, genre_csv$title == input$genres)
    Electronic_song <- track_select[track_select$genres %in% sub_genre$genre_id,]
    Electronic_song_count <- aggregate(Electronic_song[[dataInput()]], by=list(Electronic_song$date_released, Electronic_song$genres), calculateInput())
    
    names(Electronic_song_count)<-c("year","genre","count")
    Electronic_song_count$title = genre_csv$title[match(Electronic_song_count$genre, genre_csv$genre_id)]
    
    sub_genreg_cal <- ggplot(Electronic_song_count, aes(x = Electronic_song_count$year, y = Electronic_song_count$count)) + 
      geom_point(aes(color = Electronic_song_count$title, group = Electronic_song_count$genre)) +
      labs(title = "Sub genre distribution ", x = "Year", y = "Values", color = "Genres") + 
      theme(axis.text.x = element_text(angle=90,hjust=1))
    
    
    print(sub_genreg_cal)
  })
  
  output$parent_genre_plot <- renderPlot({
    find_parent_genre <- subset(genre_csv, genre_csv$title == input$larger_genres)
    find_sub_genre <- subset(genre_csv, genre_csv$parent == find_parent_genre$genre_id)
    Electronic_song <- track_select[track_select$genres %in% find_sub_genre$genre_id,]
    Electronic_song_count <- aggregate(Electronic_song[[parentDataInput()]], by=list(Electronic_song$date_released, Electronic_song$genres), parentCalculateInput())
    
    names(Electronic_song_count)<-c("year","genre","values")
    
    Electronic_song_count$title = genre_csv$title[match(Electronic_song_count$genre, genre_csv$genre_id)]
    
    sub_genre_cal <- ggplot(Electronic_song_count, aes(x = Electronic_song_count$year, y = Electronic_song_count$values)) + 
      geom_point(aes(color = Electronic_song_count$title, group = Electronic_song_count$genre)) +
      labs(title = "Sub genre distribution ", x = "Year", y = "Values", color = "Genres") + 
      facet_grid(Electronic_song_count$genre ~.) + theme(axis.text.x = element_text(angle=90,hjust=1))
    
    print(sub_genre_cal) 
  })
  
  output$genre_count <- renderPlot({
    genre_count <- aggregate(track_select$track_id, by=list(track_select$date_released, track_select$genre_top),length)
    
    names(genre_count)<-c("year","top_genre","count")
    
    genre_tracks_plot <- ggplot(genre_count, aes(x = genre_count$year, y = genre_count$count)) + 
      geom_point(aes(color = genre_count$top_genre, group = genre_count$top_genre)) +
      labs(title = "Top genres distribution ", x = "Year", y = "Total tracks", color = "Top genres") + 
      facet_grid(genre_count$top_genre ~.) + theme(axis.text.x = element_text(angle=90,hjust=1))
    
    genre_tracks_plot
  })
  output$avg_bitRate <- renderPlot({
    
    
    avg_bit_rate <- aggregate(track_select$bit_rate, by=list(track_select$date_released, track_select$genre_top),mean)
    
    names(avg_bit_rate)<-c("year","top_genre","average_bit_rate")
    
    genre_bit_rate_plot <- ggplot(avg_bit_rate, aes(x = avg_bit_rate$year, y = avg_bit_rate$average_bit_rate)) + 
      geom_point(aes(color = avg_bit_rate$top_genre, group = avg_bit_rate$top_genre)) +
      labs(title = "Top genres distribution ", x = "Year", y = "Average bit rate", color = "Top genres") + 
      facet_grid(avg_bit_rate$top_genre ~.) + theme(axis.text.x = element_text(angle=90,hjust=1))
    
    genre_bit_rate_plot
  })
  output$avg_listens <- renderPlot({
    
    avg_listen <- aggregate(track_select$listens, by=list(track_select$date_released, track_select$genre_top),mean)
    
    
    names(avg_listen)<-c("year","top_genre","average_listen")
    
    genre_avg_listen_plot <- ggplot(avg_listen, aes(x = avg_listen$year, y = avg_listen$average_listen)) + 
      geom_point(aes(color = avg_listen$top_genre, group = avg_listen$top_genre)) +
      labs(title = "Top genres distribution ", x = "Year", y = "Average listens", color = "Top genres") + 
      facet_grid(avg_listen$top_genre ~.) + theme(axis.text.x = element_text(angle=90,hjust=1))
    
    genre_avg_listen_plot
  })
  
  
  output$count_listens <- renderPlot({
    
    sum_listen <- aggregate(track_select$listens, by=list(track_select$date_released, track_select$genre_top),sum)
    
    names(sum_listen)<-c("year","top_genre","total_listen")
    
    genre_sum_listen_plot <- ggplot(sum_listen, aes(x = sum_listen$year, y = sum_listen$total_listen)) + 
      geom_point(aes(color = sum_listen$top_genre, group = sum_listen$top_genre)) +
      labs(title = "Top genres distribution ", x = "Year", y = "Total listens", color = "Top genres") + 
      facet_grid(sum_listen$top_genre ~.) + theme(axis.text.x = element_text(angle=90,hjust=1))
    
    genre_sum_listen_plot
    
  })
  
  output$tem_pro_plot <- renderPlot({
    ggplot(subset(track_csv, genre_top == input$main_genre), aes(date_released,genre_name, colour = genre_top)) + geom_point() + theme(axis.text.x = element_text(angle=90,hjust=1))  

  })
  
})
