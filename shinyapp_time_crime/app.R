# Load packages
library(shiny)
library(shiny)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library("shiny")
library("shinyWidgets")

# Load data
#trend_data <- read_csv("data/trend_data.csv")
#trend_description <- read_csv("data/trend_description.csv")

# Define UI
ui <- fluidPage(
   
  titlePanel("Crime Distribution From 1975 to 2016"),
  column(6, plotOutput(outputId = "line")),
  
  column(6, plotOutput(outputId = "pie")),
  
  fluidRow(
  column(12, 
      absolutePanel(
      sliderInput("year", 
                  "Year:", 
                  min = 1975,
                  max = 2016,
                  value = 1975,
                  sep = ""), top = 0, left = 300, right = 100, bottom = 0, width = 500
    ))

)  
)

# Define server function
server <- function(input, output) {
  
  library(ggplot2)
  library(sqldf)
  library(forcats)
  library(stringr)
  library(reshape)
  library(scales)
  df = read.csv("shiny2.csv")
  
  output$line <- renderPlot({
    
    year_i = input$year
    ggplot() + 
      geom_line(data = df, aes(x = year, y = total), size = 1.2) + 
      ggtitle('Total Crime 1980 - 2010') + 
      geom_point(data = subset(df, year == year_i), 
                 aes(x=year, y=total), colour="orange", size=7) +
      xlab('year')  + 
      ylab('Total Crime') +
      geom_vline(xintercept=year_i, linetype="dotted")
  }, height = 350)
  
  output$pie <- renderPlot({
    
    year_i = input$year
    df_pie <- df[df$year == year_i, ]
    df_melt_pie <- melt(df_pie[, c('Murder',
                                   'Rape',
                                   'Robbery',
                                   'Agg..Assult',
                                   'B.E',
                                   'Larceny.Theft',
                                   'MotorVehicle.Theft',
                                   'year')], id = c("year"))
    
    ggplot(df_melt_pie, aes(x="", y=value, fill=variable)) +
      ggtitle(paste('Total Crime in' , as.character(year_i))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") + 
      scale_fill_discrete(name = "Crime Type", labels = c("Murder", "Rape", "Robbery", 
                                                      "aggravated assault", "Break and Entering",
                                                      'Larceny Theft', 'MotorVehicle Theft'))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

# deploy:
# library(rsconnect)
# rsconnect::deployApp('C:/Users/yifan/Documents/ANLY 503/Final Exam/shinyapp')