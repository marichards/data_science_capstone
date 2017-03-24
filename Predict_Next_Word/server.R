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
            horiz = FALSE, col = 1+round((4*predict.word()[[2]]$Probability[1:5])-0.5),
            xlab = "5 Most Likely Words",
            ylab = "Estimated Probability",
            legend.text = c("p = 0-0.25",
                            "p = 0.26-0.5",
                            "p = 0.51-0.75",
                            "p = 0.76-1"),
            args.legend = (list("fill" = 1:4))
            )
    )
  
  output$word.table <- renderTable(
    predict.word()[[2]][1:5,]
  )
})
