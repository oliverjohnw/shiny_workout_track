library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)


lifts <- read_excel("workout22.xlsx", sheet = "lifts")
lift_temp <- lifts %>% 
  group_by(Date, Exercise) %>% 
  filter(Weight == max(Weight))

dates <- as.Date(lifts$Date)
date_range <- seq(min(dates), max(dates), by = 1)
non_lift_days <- date_range[!date_range %in% dates]
non_lift_count <- length(non_lift_days)
lift_count <- length(date_range) - non_lift_count
lift_count_df <- data.frame(lift = c("Non-lift Day", "Lift Day"),
                            count = c(non_lift_count, lift_count))

lift_count_df$lift <- factor(c("Non-lift Day", "Lift Day"),
                             levels = c("Non-lift Day", "Lift Day"))

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
    lift_count_df %>% 
      ggplot(., aes(x = lift, count, fill = as.factor(count))) +
      geom_bar(stat = "identity") + 
      xlab("") + 
      ylab("Number of Days") + 
      theme(legend.position = "none")
    
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
