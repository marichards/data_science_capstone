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
source("../predictNextWord.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  table.list <- readRDS("../15th_tablelist.rds")
  
  predict.word <- eventReactive(input$submit.phrase,{
    
    # For now, just return the last word after lowering its case
    pred.results <- predictNextWord(input$phrase, table.list)
    
    return(pred.results)
  })
  
  output$next.word <- renderText(predict.word()[[1]])
  
  output$word.plot <- renderPlot(
    barplot(height = predict.word()[[2]]$prob.total,
            names.arg = predict.word()[[2]]$pred.words))
})
