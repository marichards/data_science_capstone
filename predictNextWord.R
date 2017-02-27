## predictNextWord.R
library(RWeka)
suppressMessages(library(data.table))
library(stringr)

constructNgramTables <- function(){

  # Load and clean the dataset
  suppressMessages(library(tm))
  en.blog <- readLines(con = "../final/en_US/en_US.blogs.txt", warn=FALSE, skipNul = TRUE)
  unlink("../final/en_US/en_US.blogs.txt")
  en.news <- readLines(con = "../final/en_US/en_US.news.txt", warn=FALSE, skipNul = TRUE)
  unlink("../final/en_US/en_US.news.txt")
  en.twitter <- readLines(con = "../final/en_US/en_US.twitter.txt", warn=FALSE, skipNul = TRUE)
  unlink("../final/en_US/en_US.twitter.txt")
  invisible(gc())
  
  # Take 10% of each
  set.seed(140185)
  en.blog <- sample(en.blog,round(length(en.blog)/10))
  en.news <- sample(en.blog,round(length(en.blog)/10))
  en.twitter <- sample(en.blog,round(length(en.blog)/10))
  
  # Combine the data
  en.data <- c(en.blog,en.news,en.twitter)
  rm(en.blog,en.news,en.twitter)
  invisible(gc())
  
  # Clean the data
  en.data <- tolower(en.data)
  # Try removing stop words
#  en.data <- removeWords(en.data, stopwords())
  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
  
  # Remove bad words and non-ASCII
  bad.words <- read.table("./swearWords.txt")
  en.data <- removeWords(en.data,bad.words$V1)
  en.data <- iconv(en.data,"UTF-8","ASCII",sub="")
  rm(bad.words)
  
  ############## Dataset is now clean ##################
  
  ## Step 2: Create N-Gram Tables
  
  # Create table of words, including an index 
  singlets <- WordTokenizer(en.data) # Takes about 5 seconds
  singlet.df <- data.table(table(singlets)) # Takes about 2 seconds
  singlet.df$idx <- 1:length(singlet.df$singlets)
  rm(singlets); invisible(gc())
  
  # Create N-gram tables
  ## Given the singlets, construct the couplets; don't worry about idx for now
  couplets <- NGramTokenizer(en.data, Weka_control(min=2,max=2)) # Takes about 8 seconds
  couplet.df <- data.table(table(couplets)); rm(couplets)
  ## Split words; 5 seconds apiece
  couplet.df$ngram <- word(couplet.df$couplets, 1)
  
  couplet.df$last.word <- word(couplet.df$couplets, 2)
  couplet.df[,couplets:=NULL] # Remove couplets column
  
  invisible(gc())  
  
  triplets <- NGramTokenizer(en.data, Weka_control(min=3,max=3)) # Takes about 13 seconds
  triplet.df <- data.table(table(triplets)); rm(triplets)
  ## These next 2 take maybe 25 seconds total
  triplet.df$ngram <- paste(word(triplet.df$triplets,1),word(triplet.df$triplets,2))
  triplet.df$last.word <- word(triplet.df$triplets,3)
  triplet.df[,triplets:=NULL] # Remove triplets column
  invisible(gc())
  
  # Do it for 4 grams too
  quads <- NGramTokenizer(en.data, Weka_control(min=4,max=4))
  quad.df <- data.table(table(quads)); rm(quads)
  ## These next 2 take maybe 30 seconds total
  quad.df$ngram <- paste(word(quad.df$quads,1),word(quad.df$quads,2), word(quad.df$quads,3))
  quad.df$last.word <- word(quad.df$quads,4)
  quad.df[,quads:=NULL] # Remove quads column
  invisible(gc())  

  # Do it for 5 grams too
#  quins <- NGramTokenizer(en.data, Weka_control(min=5,max=5))
#  quin.df <- data.table(table(quins)); rm(quins)
  ## These next 2 take a while...way too long
##  quin.df$ngram <- paste(word(quin.df$quins,1),word(quin.df$quins,2), word(quin.df$quins,3), word(quin.df$quins,4))
#  quin.df$last.word <- word(quin.df$quins,5)
#  quin.df[,quins:=NULL] # Remove quads column
#  invisible(gc()) 
  
  return(list(quad.df,
              triplet.df,
              couplet.df))
}

predictNextWord <- function(phrase,quad.df,triplet.df,couplet.df){
  # Take in the input and grab the last n grams for 4, 3, 2, 1
  # Remove stop words
#  phrase <- removeWords(phrase,stopwords())
  phrase <- removePunctuation(phrase)
  
#  last.4grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=4,max=4)),1))
  last.3grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=3,max=3)),1))
  last.2grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=2,max=2)),1))
  last.gram <- tolower(tail(WordTokenizer(phrase),1))
  ### Through here, this takes 218 seconds (usisng 10%)
  ### Using only 1% and data tables, now this takes 72 seconds; it's still 30,000 words
  
  ## Use a simple MLE method on couplet table
  all.candidates <- subset(couplet.df, ngram == last.gram)
  all.candidates <- all.candidates[order(all.candidates$N, decreasing = TRUE),]
  couplet.prediction <- all.candidates$last.word[[1]]

  # Use MLE on triplet and quad
  all.candidates <- subset(triplet.df, ngram == last.2grams)
  all.candidates <- all.candidates[order(all.candidates$N, decreasing = TRUE),]
  triplet.prediction <- all.candidates$last.word[[1]]
  
  all.candidates <- subset(quad.df, ngram == last.3grams)
  all.candidates <- all.candidates[order(all.candidates$N, decreasing = TRUE),]
  quad.prediction <- all.candidates$last.word[[1]]  
  
#  all.candidates <- subset(quin.df, ngram == last.4grams)
#  all.candidates <- all.candidates[order(all.candidates$N, decreasing = TRUE),]
#  quin.prediction <- all.candidates$last.word[[1]]  
  # Construct table of frequencies for 4 grams
  
  # Construct table of frequencies for 3 grams
  
  # Construct table of frequencies for 2 grams
  return(list(quad.prediction,
              triplet.prediction,
              couplet.prediction))
}
