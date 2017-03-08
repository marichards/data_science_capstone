#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  table.list <- readRDS("../../20percent_tablelist.rds")
  
  predict.word <- eventReactive(input$submit.phrase,{
    
    # For now, just return the last word after lowering its case
    my.word <- word(input$phrase, -1)
    return(tolower(my.word))
  })
  
  output$next.word <- renderText(predict.word())
  
})
