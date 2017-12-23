#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
navbarPage("Menu", 
           tabPanel("Single genre",titlePanel("Sub genre distribution"),
                                sidebarLayout(sidebarPanel(
                                selectInput("genres", "Please select single genre",   choices = genre_csv$title),
                                radioButtons("result", "Select the features", list("Tracks", "Bit rate", "Listens")),
                                radioButtons("calculate", "Select the calculations", list("count", "average", "sum"))),
                                mainPanel(plotOutput("genre_plot")))),
    
          tabPanel("Parent genre", titlePanel("Parent genres distribution"),
                                sidebarLayout(sidebarPanel(
                                selectInput("larger_genres", "Please select single genre",   choices = parent_genre$title),
                                radioButtons("larger_result", "Select the features", list("Tracks", "Bit rate", "Listens")),
                                radioButtons("larger_calculate", "Select the calculations", list("count", "average"))),
                                mainPanel(plotOutput("parent_genre_plot")))),
                  
          tabPanel("Top genre", titlePanel("Top genres distribution"),
                                  navbarPage("Kind of distribution",
                                             tabPanel("Temporal process plot", titlePanel("Temporal process plot of top genres"), 
                                                      sidebarLayout(sidebarPanel(
                                                        selectInput("main_genre", "Please select top genre", choices = track_csv$genre_top)), 
                                                        mainPanel(plotOutput("tem_pro_plot")))),
                                                                                                                                                
                                             tabPanel("Total tracks", titlePanel("Total tracks of top genres"),mainPanel(plotOutput("genre_count"))),
                                             tabPanel("Average bit rate", titlePanel("Average bit rate of top genres"),mainPanel(plotOutput("avg_bitRate"))),
                                             tabPanel("Average listens", titlePanel("Average listens of top genres"),mainPanel(plotOutput("avg_listens"))),
                                             tabPanel("Total listens", titlePanel("Total listens of top genres"),mainPanel(plotOutput("count_listens")))
                                             )
                   ),
        
          
          
          tags$head(includeHTML("http://127.0.0.1:5228/Desktop/FMA_1/test2/www/")  )                
                   )
          