library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction Algorithm"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("phrase","Enter a phrase:"),
       actionButton("submit.phrase","Submit phrase"),
       textOutput("next.word")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("word.plot")
       
    )
  )
))
