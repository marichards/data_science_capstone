## predictNextWord.R
library(RWeka)
suppressMessages(library(data.table))
library(stringr)


predictNextWord <- function(phrase){

  # Load and clean the dataset
  suppressMessages(library(tm))
  en.docs <- VCorpus(DirSource("../final/en_US"),
                     readerControl = list(language="en"))
  en.blog <- as.character(en.docs[["en_US.blogs.txt"]])
  en.news <- as.character(en.docs[["en_US.news.txt"]]) #This is a list
  en.twitter <- as.character(en.docs[["en_US.twitter.txt"]]) #This is a list
  rm(en.docs); invisible(gc())
  
  # Take 1% of each
  set.seed(140185)
  en.blog <- sample(en.blog,round(length(en.blog)/100))
  en.news <- sample(en.blog,round(length(en.blog)/100))
  en.twitter <- sample(en.blog,round(length(en.blog)/100))
  
  # Combine the data
  en.data <- c(en.blog,en.news,en.twitter)
  rm(en.blog,en.news,en.twitter)
  invisible(gc())
  
  # Clean the data
  en.data <- tolower(en.data)
  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
  
  # Remove bad words and non-ASCII
  bad.words <- read.table("./swearWords.txt")
  en.data <- removeWords(en.data,bad.words$V1)
  en.data <- iconv(en.data,"UTF-8","ASCII",sub="")
  rm(bad.words)
  
  # Create table of words, including an index 
  singlets <- WordTokenizer(en.data)
  singlet.df <- data.table(table(singlets))
  singlet.df$idx <- 1:length(singlet.df$singlets)
  rm(singlets); invisible(gc())
  
  # Create N-gram tables
  ## Given the singlets, construct the couplets; don't worry about idx for now
  couplets <- NGramTokenizer(en.data, Weka_control(min=2,max=2))
  couplet.df <- data.table(table(couplets)); rm(couplets)
  ## Split words; 5 seconds apiece
  couplet.df$ngram <- word(couplet.df$couplets, 1)
  
  couplet.df$last.word <- word(couplet.df$couplets, 2)
  couplet.df[,couplets:=NULL] # Remove couplets column
  
  invisible(gc())  
  
  
  triplets <- NGramTokenizer(en.data, Weka_control(min=3,max=3))
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
  quins <- NGramTokenizer(en.data, Weka_control(min=5,max=5))
  quin.df <- data.table(table(quins)); rm(quins)
  ## These next 2 take a while...way too long
  quin.df$ngram <- paste(word(quin.df$quins,1),word(quin.df$quins,2), word(quin.df$quins,3), word(quin.df$quins,4))
  quin.df$last.word <- word(quin.df$quins,5)
  quin.df[,quins:=NULL] # Remove quads column
  invisible(gc())  
  
  # Take in the input and grab the last n grams for 4, 3, 2, 1
  last.4grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=4,max=4)),1))
  last.3grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=3,max=3)),1))
  last.2grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=2,max=2)),1))
  last.gram <- tolower(tail(WordTokenizer(phrase),1))
  ### Through here, this takes 218 seconds (using 10%)
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
  
  # Construct table of frequencies for 4 grams
  
  # Construct table of frequencies for 3 grams
  
  # Construct table of frequencies for 2 grams
  return(list(quad.prediction,
              triplet.prediction,
              couplet.prediction))

