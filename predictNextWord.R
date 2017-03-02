## predictNextWord.R
library(tokenizers)
library(stringr)
suppressMessages(library(tm))
library(hash)
suppressMessages(library(dplyr))
suppressMessages(library(data.table))

constructNgramTables <- function(){

  # Load and clean the dataset
  
  en.blog <- readLines(con = "../final/en_US/en_US.blogs.txt", warn=FALSE, skipNul = TRUE)
  en.news <- readLines(con = "../final/en_US/en_US.news.txt", warn=FALSE, skipNul = TRUE)
  en.twitter <- readLines(con = "../final/en_US/en_US.twitter.txt", warn=FALSE, skipNul = TRUE)
  invisible(gc())
  
  # Take 10% of each (or 20% if stopwords are removed?)
  set.seed(10185) #140185 seems to not work well
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

  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
#  en.data <- removeWords(en.data, stopwords())
  en.data <- trimws(en.data)
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
  invisible(gc())
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
  
  # Convert to data table
  quin.df <- data.table(word.1 = quin.matrix[,1],
                        word.2 = quin.matrix[,2],
                        word.3 = quin.matrix[,3],
                        word.4 = quin.matrix[,4],
                        word.5 = quin.matrix[,5])
  rm(quin.matrix)
  
  # Create 4-gram, 3-gram, and 2-gram tables too
  quads <- unlist(tokenize_ngrams(en.data, n = 4))
  invisible(gc())
  quads <-str_split(quads,pattern=" ")
  quads <- lapply(quads,convert.to.ints,singlet.hash=singlet.hash)
  quad.matrix <- matrix(unlist(quads), ncol = 4, byrow = TRUE)
  
  quad.df <- data.table(word.1 = quad.matrix[,1],
                        word.2 = quad.matrix[,2],
                        word.3 = quad.matrix[,3],
                        word.4 = quad.matrix[,4])
  rm(quad.matrix)
  
  trips <- unlist(tokenize_ngrams(en.data, n = 3))
  invisible(gc())
  trips <-str_split(trips,pattern=" ")
  trips <- lapply(trips,convert.to.ints,singlet.hash=singlet.hash)
  trip.matrix <- matrix(unlist(trips), ncol = 3, byrow = TRUE)
  
  trip.df <- data.table(word.1 = trip.matrix[,1],
                        word.2 = trip.matrix[,2],
                        word.3 = trip.matrix[,3])
  rm(trip.matrix)
  
  coups <- unlist(tokenize_ngrams(en.data, n = 2))
  rm(en.data); invisible(gc())
  coups <-str_split(coups,pattern=" ")
  coups <- lapply(coups,convert.to.ints,singlet.hash=singlet.hash)
  coup.matrix <- matrix(unlist(coups), ncol = 2, byrow = TRUE)

  coup.df <- data.table(word.1 = coup.matrix[,1],
                        word.2 = coup.matrix[,2])
  rm(coup.matrix)
  
  return(list(quin.df,
              quad.df,
              trip.df,
              coup.df,
              singlet.hash,
              singlet.inverted)) ## THis function currently takes 4 minutes...not bad! 
  #List is 153 Mb total, but 76.5 if you drop a quin; they're basically the same size
  ## With all tables, now this takes 15-16 min and produces 220 Mb...not bad!
  # Using 20% and no stop words, takes 24 min and 254.3 Mb
  # Using 20% and all the words, takes 46 min and 515 Mb
}

predictNextWord <- function(phrase,table.list){
  # Take in the input and grab the last n grams for 4, 3, 2, 1
  # Remove stop words
  phrase <- tolower(phrase)
  phrase <- removePunctuation(phrase)
  phrase <- removeNumbers(phrase)
#  phrase <- removeWords(phrase,stopwords())
  phrase <- trimws(phrase)

  
  
  # Grab grams as their indices in the hash
  gram4 <- table.list[[5]][[word(phrase, -1)]]
  gram3 <- table.list[[5]][[word(phrase, -2)]]
  gram2 <- table.list[[5]][[word(phrase, -3)]]
  gram1 <- table.list[[5]][[word(phrase, -4)]]
  
  # Filter answers that share the last gram and so forth using dplyr
  last4 <- filter(table.list[[1]],
                  word.1 == gram1 & word.2 == gram2 & word.3 == gram3 & word.4 == gram4)
  # Going forward, don't let the last gram be the same as previous
  last3 <- filter(table.list[[2]], 
                  word.1 == gram2 & word.2 == gram3 & word.3 == gram4)# & !(word.4 %in% last4$word.5))
  last2 <- filter(table.list[[3]], 
                  word.1 == gram3 & word.2 == gram4)# & !(word.3 %in% last4$word.5) & !(word.3 %in% last3$word.4))
  
  last1 <- filter(table.list[[4]], 
                  word.1 == gram4 )#& !(word.2 %in% last4$word.5) & !(word.2 %in% last3$word.4) & !(word.2 %in% last2$word.3))

  # Now we have unique things
  

 
  ## Grab the table of each one, summing up frequencies
  tbl.5gram <- sort(table(last4$word.5),decreasing=TRUE)
  tbl.4gram <- sort(table(last3$word.4),decreasing=TRUE)
  tbl.3gram <- sort(table(last2$word.3),decreasing=TRUE)
  tbl.2gram <- sort(table(last1$word.2),decreasing=TRUE)
  
  # Filter out stopwords? 
  
  # Return things if they're not null
  if(length(tbl.5gram) !=0)
  {pred5 <- table.list[[6]][[names(tbl.5gram)[[1]]]]}
  else{pred5 <- NA}
  if(length(tbl.4gram) !=0)
  {pred4 <- table.list[[6]][[names(tbl.4gram)[[1]]]]}
  else{pred4 <- NA}
  if(length(tbl.3gram) !=0)
  {pred3 <- table.list[[6]][[names(tbl.3gram)[[1]]]]}
  else{pred3 <- NA}
  if(length(tbl.2gram) !=0)
  {pred2 <- table.list[[6]][[names(tbl.2gram)[[1]]]]}
  else{pred2 <- NA}
  
  my.prediction <- data.frame(pred5 = pred5,
                              pred4 = pred4,
                              pred3 = pred3,
                              pred2 = pred2)
  return(my.prediction)
}
