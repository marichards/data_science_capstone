library(shiny)
library(stringr)
source("./predictNextWord.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  table.list <- readRDS("./10percent_tablelist.rds")
  
  predict.word <- eventReactive(input$submit.phrase,{
    
    # For now, just return the last word after lowering its case
    pred.results <- predictNextWord(input$phrase, table.list)
    
    return(pred.results)
  })
  
  output$next.word <- renderText(predict.word()[[1]])
  
  output$word.plot <- renderPlot(
    barplot(height = predict.word()[[2]]$Probability[1:5],
            names.arg = predict.word()[[2]]$Word[1:5],
            horiz = FALSE, col = 1+round(4*predict.word()[[2]]$Probability[1:5]),
            xlab = "5 Most Likely Words",
            ylab = "Estimated Probability"))
  
  output$word.table <- renderTable(
    predict.word()[[2]]
  )
})
