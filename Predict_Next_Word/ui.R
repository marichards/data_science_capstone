library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Your Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Instructions for Use"),
      tags$ol(
        tags$li("Enter a phrase below"),
        tags$li("Click the 'Submit Phrase' button")
      ),
       textInput("phrase","Enter a phrase:"),
       actionButton("submit.phrase","Submit Phrase"),
       h4("Top Predicted Word:"),
      h4(textOutput("next.word"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("word.plot"),
       tableOutput("word.table")
      
    )
  )
))
