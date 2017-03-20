suppressMessages(library(tm))
suppressMessages(library(tokenizers))
suppressMessages(library(stringr))
suppressMessages(library(hash))
suppressMessages(library(plyr))
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
  
  #### Clean the data ####
  
  # Split by sentences
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
  #  quins <- unlist(tokenize_ngrams(en.data, n = 5))
  #  invisible(gc())
  #  quins <-str_split(quins,pattern=" ")
  
  convert.to.ints <- function(ngram,singlet.hash){
    
    # Do an operation I could do once
    nint <- integer(length = length(ngram))
    for(i in 1:length(ngram)){
      nint[[i]] <- singlet.hash[[ngram[[i]]]]
    }
    
    nint
  }
  
  # Convert to integers and make into a matrix
  #  quins <- lapply(quins,convert.to.ints,singlet.hash=singlet.hash)
  #  quin.matrix <- matrix(unlist(quins), ncol = 5, byrow = TRUE)
  
  # Convert to data table
  #  quin.df <- data.table(word.1 = quin.matrix[,1],
  #                        word.2 = quin.matrix[,2],
  #                        word.3 = quin.matrix[,3],
  #                        word.4 = quin.matrix[,4],
  #                        word.5 = quin.matrix[,5])
  #  rm(quin.matrix)
  
  # Create 4-gram, 3-gram, and 2-gram tables
  quads <- unlist(tokenize_ngrams(en.data, n = 4))
  invisible(gc())
  quads <-str_split(quads,pattern=" ")
  quads <- lapply(quads,convert.to.ints,singlet.hash=singlet.hash)
  quad.matrix <- matrix(unlist(quads), ncol = 4, byrow = TRUE)
  
  quad.df <- data.table(word.1 = quad.matrix[,1],
                        word.2 = quad.matrix[,2],
                        word.3 = quad.matrix[,3],
                        word.4 = quad.matrix[,4])
  rm(quads); rm(quad.matrix)
  
  trips <- unlist(tokenize_ngrams(en.data, n = 3))
  invisible(gc())
  trips <-str_split(trips,pattern=" ")
  trips <- lapply(trips,convert.to.ints,singlet.hash=singlet.hash)
  trip.matrix <- matrix(unlist(trips), ncol = 3, byrow = TRUE)
  
  trip.df <- data.table(word.1 = trip.matrix[,1],
                        word.2 = trip.matrix[,2],
                        word.3 = trip.matrix[,3])
  rm(trips); rm(trip.matrix)
  
  coups <- unlist(tokenize_ngrams(en.data, n = 2))
  rm(en.data); invisible(gc())
  coups <-str_split(coups,pattern=" ")
  coups <- lapply(coups,convert.to.ints,singlet.hash=singlet.hash)
  coup.matrix <- matrix(unlist(coups), ncol = 2, byrow = TRUE)
  
  coup.df <- data.table(word.1 = coup.matrix[,1],
                        word.2 = coup.matrix[,2])
  rm(coups); rm(coup.matrix)
  
  return(list(quads = quad.df,
              trips = trip.df,
              coups = coup.df,
              word.hash = singlet.hash,
              word.hash.inv = singlet.inverted))
}