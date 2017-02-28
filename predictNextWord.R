## predictNextWord.R
library(tokenizers)
suppressMessages(library(data.table))
library(stringr)
suppressMessages(library(tm))
library(hash)

constructNgramTables <- function(){

  # Load and clean the dataset
  
  en.blog <- readLines(con = "../final/en_US/en_US.blogs.txt", warn=FALSE, skipNul = TRUE)
  en.news <- readLines(con = "../final/en_US/en_US.news.txt", warn=FALSE, skipNul = TRUE)
  en.twitter <- readLines(con = "../final/en_US/en_US.twitter.txt", warn=FALSE, skipNul = TRUE)
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

  ############## Dataset is now clean ##################  en.data <- tolower(en.data)
  # Try removing stop words
  en.data <- tolower(en.data)
#  en.data <- removeWords(en.data, stopwords())
  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
  en.data <- iconv(en.data,"UTF-8","ASCII",sub="")
  
  # Remove bad words
  bad.words <- read.table("./swearWords.txt")
  en.data <- removeWords(en.data,bad.words$V1)
  rm(bad.words); invisible(gc())
  
  ## Step 2: Create N-Gram Tables
  
  # Create table of words, including an index 
  singlets <- unlist(tokenize_ngrams(en.data, n=1)) # Takes about 3 seconds
  singlet.hash <- hash(keys = unique(singlets),
                       values = 1:length(unique(singlets))) # Takes about 0.3
  # Invert the hash
  singlet.inverted <- hash(keys = 1:length(unique(singlets)),
                           values = unique(singlets))
  rm(singlets); invisible(gc())
  
  # Create N-gram table up to 5 grams
  quins <- unlist(tokenize_ngrams(en.data, n = 5))
  rm(en.data); invisible(gc())
  quins <-str_split(quins,pattern=" ")
  
  convert.to.ints <- function(ngram,singlet.hash){
    
    # Do an operation I could do once
    nint <- integer(length = length(ngram))
    for(i in 1:length(ngram)){
      nint[[i]] <- singlet.hash[[ngram[[i]]]]
    }
    
    nint
  }
  
  # Convert to integers and make into a matrix
  quins <- lapply(quins,convert.to.ints,singlet.hash=singlet.hash)
  quin.matrix <- matrix(unlist(quins), ncol = 5, byrow = TRUE)
  
  # Option to convert to data table
  quin.df <- data.table(word.1 = quin.matrix[,1],
                        word.2 = quin.matrix[,2],
                        word.3 = quin.matrix[,3],
                        word.4 = quin.matrix[,4],
                        word.5 = quin.matrix[,5])
  

  
  return(list(quin.df,
              singlet.hash,
              singlet.inverted)) ## THis function currently takes 4 minutes...not bad! 
  #List is 153 Mb total, but 76.5 if you drop a quin; they're basically the same size
}

predictNextWord <- function(phrase,singlet.hash,quin.df){
  # Take in the input and grab the last n grams for 4, 3, 2, 1
  # Remove stop words
#  phrase <- removeWords(phrase,stopwords())
  phrase <- removePunctuation(phrase)
  
  # Grab grams as their indices in the hash
  gram4 <- singlet.hash[[tolower(word(phrase, -1))]]
  gram3 <- singlet.hash[[tolower(word(phrase, -2))]]
  gram2 <- singlet.hash[[tolower(word(phrase, -3))]]
  gram1 <- singlet.hash[[tolower(word(phrase, -4))]]
  
  # Filter answers that share the last gram and so forth
  library(dplyr)
  last1 <- filter(quin.df, word.4 == gram4)
  last2 <- filter(last1, word.3 == gram3)
  last3 <- filter(last2, word.2 == gram2)
  last4 <- filter(last3, word.1 == gram1)
  
  
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
