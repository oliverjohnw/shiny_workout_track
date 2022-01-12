# app designed to track personal workouts throughout 2022

# load packages
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)

# read data
lifts <- read_excel("workout22.xlsx", sheet = "lifts")
work <- read_excel("workout22.xlsx", sheet = "workouts")

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
                             
                             h4("Number of days I made it (and didn't make it) to the gym"),
                             plotOutput("liftcountplot")
                           ) ),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
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
      ggplot(., aes(x = Workout)) +
      geom_bar()
    
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
