#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction Algorithm"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("phrase","Enter a phrase:"),
       actionButton("submit.phrase","Submit phrase")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("next.word")
    )
  )
))
