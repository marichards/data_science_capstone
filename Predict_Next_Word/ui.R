library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Your Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("phrase","Enter a phrase:"),
       actionButton("submit.phrase","Submit phrase"),
       h3("Top Predicted Word"),
       textOutput("next.word")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
       tabPanel("Top Word Barplot", br(), plotOutput("word.plot")),
       tabPanel("Top Word Table", br(), tableOutput("word.table"))
      )
    )
  )
))
