# Load packages
library(shiny)
library(shiny)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library("shiny")
library("shinyWidgets")

# Define UI
ui <- fluidPage(
  
  titlePanel("Location and Frequently Occuring Crime"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("location",
                  "Location Type: ",
                  choices = c('Parking Lot', 'Street', 'Parking Garage', 'Residence', 'Park',
                              'Retail', 'Other/Unknown', 'Jail/Prison',
                              'Air/Bus/Train/Metro Terminal', 'Hotel/Motel/Etc.',
                              'Lake/Waterway', 'Grocery/Supermarket', 'Bank',
                              'Convenience Store', 'Government Building', 'Construction Site',
                              'Auto Repair', 'Restaurant', 'Hospital/Emergency Care Center',
                              'Liquor Store', 'School/College', 'Commercial',
                              'Bank/S&L/Credit Union', 'Doctor/Dentist/Vet Office',
                              'Gas Station', 'Auto Dealership', 'Pool', 'Theater',
                              'Bar/Night Club', 'Pawn Shop', 'Field/ Open Space',
                              'Check Cashing Est.', 'Library', 'Pedestrian Tunnel',
                              'Recreation Center', 'Church/Synagogue/Temple', 'Wooded Area',
                              'Rental Storage Facility', 'Golf Course', 'Laundromat', 'Nursery')
        
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot"),
      tableOutput("table")
    )
  ))
as.integer()

library(ggplot2)
library(sqldf)
library(forcats)
library(stringr)

df2 = read.csv("df2.csv")

# Define server function
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    place = input$location
    sub = df2[df2$place_clean == place,]
    vc = tail(names(sort(table(sub$Crime.Name2))), 10)
    top_10 = sub[is.element(sub$Crime.Name2, vc),]
    ggplot(top_10, aes(x=fct_infreq(Crime.Name2), fill = factor(Crime.Name2))) +
      geom_bar() + 
      theme(axis.text.x = element_text(angle = 0, vjust=1, hjust = 0.5)) +
      ggtitle(paste('Location Type: ', place )) +
      xlab('Specific Crime Type')  + 
      ylab('Frequency') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
      theme(legend.position="none")
  })
  
  output$table <- renderTable({
    place = input$location
    sub = df2[df2$place_clean == place,]
    vc = tail(names(sort(table(sub$Crime.Name2))), 8)
    df = data.frame(rev(vc))
    colnames(df) <- c("Top Frequent Crimes:")
    df
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

# deploy:
# library(rsconnect)
# rsconnect::deployApp('C:/Users/yifan/Documents/ANLY 503/Final Exam/shinyapp')
