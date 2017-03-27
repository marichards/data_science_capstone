suppressMessages(library(tm))
suppressMessages(library(tokenizers))
suppressMessages(library(stringr))
suppressMessages(library(hash))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))

testPredictiveAccuracy <- function(){
  
  # Part 1: Grab a new random sample 
  
  en.blog <- readLines(con = "../final/en_US/en_US.blogs.txt", warn=FALSE, skipNul = TRUE)
  en.news <- readLines(con = "../final/en_US/en_US.news.txt", warn=FALSE, skipNul = TRUE)
  en.twitter <- readLines(con = "../final/en_US/en_US.twitter.txt", warn=FALSE, skipNul = TRUE)
  invisible(gc())
  
  # Take 10% of each (or 20% if stopwords are removed?)
  set.seed(12) #140185 seems to not work well
  en.blog <- sample(en.blog,round(length(en.blog)/1000))
  en.news <- sample(en.blog,round(length(en.blog)/1000))
  en.twitter <- sample(en.blog,round(length(en.blog)/1000))
  
  # Combine the data
  en.data <- c(en.blog,en.news,en.twitter)
  rm(en.blog,en.news,en.twitter)
  invisible(gc())
  
  length(en.data)
  
  # Part 2: Clean data and make it into sentences
  en.data <- unlist(str_split(en.data, pattern = "\\.+"))
  en.data <- en.data[en.data != ""]
  
  # Try removing stop words
  en.data <- tolower(en.data)
  
  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
  #  en.data <- removeWords(en.data, stopwords())
  en.data <- trimws(en.data)
  en.data <- iconv(en.data,"UTF-8","ASCII",sub="")
  
  bad.words <- read.table("./swearWords.txt")
  en.data <- removeWords(en.data,bad.words$V1)
  rm(bad.words); invisible(gc())
  
  ## Now have 2347 sentences
  
  # Part 3: Split each sentence into the last word and the rest
  split.grams <- lapply(en.data, function(x) strsplit(x, split = " (?=[^ ]+$)", perl = TRUE)[[1]])
  phrases <- lapply()
  
  # Source the prediction script and table list
  source("./Predict_Next_Word/predictNextWord.R")
  table.list <- readRDS("./Predict_Next_Word/10percent_tablelist.rds")
  
  # Create a function that predicts each and compares the prediction to the last gram
  test.predictions <- function(list.element, table.list){
    
    my.prediction <- try(predictNextWord(list.element[1], table.list)[[1]], silent = TRUE)
    my.prediction == list.element[2]
  }
  
  # Remove things with no length
  testything <- sapply(split.grams, function(x) length(strsplit(x[1], split = " ")[[1]]))
  blah <- testything==0
  split.grams <- split.grams[!blah]
  

  pred.list <- sapply(split.grams[1:100], test.predictions, table.list)
  
  test.inclusion <- function(list.element, table.list){
    possible.preds <- try(predictNextWord(list.element[1], table.list)[[2]], silent = TRUE)
    if(is.data.frame(possible.preds)){my.result <- list.element[2] %in% possible.preds$Word}
       else{my.result <- NA}
       my.result
  }
}