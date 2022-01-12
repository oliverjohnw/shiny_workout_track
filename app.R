# app designed to track personal workouts throughout 2022

# load packages
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)

# read data
lifts <- read_excel("workout22.xlsx", sheet = "lifts")
work <- read_excel("workout22.xlsx", sheet = "workouts") %>% 
  mutate(Start_Time = substr(Start_Time, 12, 16)) %>% 
  mutate(temp_time = round(as.numeric(substr(Start_Time, 4,5))/60,2) * 10000) %>% 
  mutate(time = as.numeric(paste(substr(Start_Time, 1,2), ".", temp_time, sep = "")))

# calculate max values for each lift for every day I do them
lift_temp <- lifts %>% 
  group_by(Date, Exercise) %>% 
  filter(Weight == max(Weight))


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  
                  "Workout Tracker 2022",
                  tabPanel("Max Lifts",
                           sidebarPanel(selectizeInput("exerciseInput", "Exercise",
                                                       choices = unique(lift_temp$Exercise),  
                                                       selected="Bench Press", multiple =FALSE)
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Max Weight by Date"),
                             
                             h4("Maximum amount of weight I put up for each day"),
                             plotOutput("maxliftsplot")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Lift Count", # sidebarPanel
                           mainPanel(
                             h1("Workout Days"),
                             fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("liftcountplot"),
                                                  plotOutput("locationplot"))
                             )
                           ) ),
                  tabPanel("Intensity Tracker",
                           mainPanel(
                             h1("Intensity metrics of workouts"),
                             fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("caloriesplot"),
                                                  plotOutput("durationplot"))
                               )
                           ))
                  
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
  d <- reactive({
    filtered <-
      lift_temp %>%
      filter(Exercise == input$exerciseInput)    
    
  }) 
  
  output$maxliftsplot <- renderPlot({
    
    d() %>% 
      ggplot(., aes(x = as.Date(Date), y = Weight)) +
      geom_line() +
      xlab("") +
      ylab("Weight in Pounds") + 
      theme(legend.position = "none")
  })
  
  output$liftcountplot <- renderPlot({
    work %>% 
      ggplot(., aes(x = Workout, fill = Workout)) +
      geom_bar() +
      ylab("Total Days")
    
  })
  
  output$caloriesplot <- renderPlot({
    work %>% 
      ggplot(., aes(x = Date, y = Calories)) + 
      geom_line(color = "red") 
    
  })
  
  output$durationplot <- renderPlot({
    work %>% 
      ggplot(., aes(x = Date, y = Duration)) + 
      geom_line(color = "blue")
    
  })
  
  output$locationplot <- renderPlot({
    work %>% 
      ggplot(., aes(x = Location, fill = Location)) +
      geom_bar()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

# rsconnect::deployApp('~/Downloads/shiny_workout_track')