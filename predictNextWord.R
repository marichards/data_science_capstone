## predictNextWord.R
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
  en.blog <- sample(en.blog,round(length(en.blog)/5))
  en.news <- sample(en.blog,round(length(en.blog)/5))
  en.twitter <- sample(en.blog,round(length(en.blog)/5))
  
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
              word.hash.inv = singlet.inverted)) ## THis function currently takes 4 minutes...not bad! 
  #List is 153 Mb total, but 76.5 if you drop a quin; they're basically the same size
  ## With all tables, now this takes 15-16 min and produces 220 Mb...not bad!
  # Using 20% and no stop words, takes 24 min and 254.3 Mb
  # Using 20% and all the words, takes 46 min and 515 Mb
  # Dropping the quin.df and using 10%, it takes about 11 minutes and 132 Mb
  # 55 Mb in quads; 45 in trips; 32 in coups
  # Dropping quin.df and using 20%, takes 30 min and 307 Mb (on Sandwich)
  # Takes only 15 min on Buoy
}

predictNextWord <- function(phrase,table.list){
  # Take in the input and grab the last n grams for 3, 2, 1
  phrase <- tolower(phrase)
  phrase <- removePunctuation(phrase)
  phrase <- removeNumbers(phrase)
#  phrase <- removeWords(phrase,stopwords())
  phrase <- trimws(phrase)

  # Remove words that aren't in the singlet hash
  all.words <- strsplit(phrase, split = " ")[[1]]
  not.there <- all.words[!(all.words %in% keys(table.list$word.hash))]
  phrase <- removeWords(phrase, not.there)
  phrase <- stripWhitespace(phrase)
  
  # Grab grams as their indices in the hash
  gram4 <- table.list$word.hash[[word(phrase, -1)]]
  gram3 <- table.list$word.hash[[word(phrase, -2)]]
  gram2 <- table.list$word.hash[[word(phrase, -3)]]
  
  # Filter answers that share the last gram and so forth using dplyr
  # Going forward, don't let the last gram be the same as previous
  last3 <- filter(table.list$quads, 
                  word.1 == gram2 & word.2 == gram3 & word.3 == gram4)# & !(word.4 %in% last4$word.5))
  last2 <- filter(table.list$trips, 
                  word.1 == gram3 & word.2 == gram4)# & !(word.3 %in% last4$word.5) & !(word.3 %in% last3$word.4))
  
  last1 <- filter(table.list$coups, 
                  word.1 == gram4 )#& !(word.2 %in% last4$word.5) & !(word.2 %in% last3$word.4) & !(word.2 %in% last2$word.3))
 
  ## Grab the table of each one, summing up frequencies
  tbl.4gram <- data.frame(table(last3$word.4))
  tbl.3gram <- data.frame(table(last2$word.3))
  tbl.2gram <- data.frame(table(last1$word.2))
  
  # Remove rows of 1's
  tbl.4gram <- subset(tbl.4gram, Freq > 1)
  tbl.3gram <- subset(tbl.3gram, Freq > 1)
  tbl.2gram <- subset(tbl.2gram, Freq > 1)
  
  # Filter out stopwords
  stop.ints <- numeric(length = length(stopwords()))
  for(i in 1:length(stopwords()))
    {stop.ints[[i]] <- table.list$word.hash[[stopwords()[[i]]]]}
  
  tbl.4gram <- filter(tbl.4gram, !(Var1 %in% stop.ints))
  tbl.3gram <- filter(tbl.3gram, !(Var1 %in% stop.ints))
  tbl.2gram <- filter(tbl.2gram, !(Var1 %in% stop.ints))
  
  # Sort it by freq
  tbl.4gram <- tbl.4gram[order(tbl.4gram$Freq, decreasing = TRUE),]
  tbl.3gram <- tbl.3gram[order(tbl.3gram$Freq, decreasing = TRUE),]
  tbl.2gram <- tbl.2gram[order(tbl.2gram$Freq, decreasing = TRUE),]

  # Return things if they're not null
  if(is.data.frame(tbl.4gram)){
    if(nrow(tbl.4gram) !=0){
      lambda.1 <- 0.5
      tbl.4gram <- mutate(tbl.4gram, gt.probs.4 = lambda.1*Freq/(sum(Freq)))
    pred4 <- tbl.4gram[c("Var1", "gt.probs.4")]}
    else{pred4 <- NA; lambda.1 <- 0}}
  else{pred4 <- NA; lambda.1 <- 0}
  
  if(is.data.frame(tbl.3gram)){
    if(nrow(tbl.3gram) !=0){
      lambda.2 <- -0.7*(lambda.1 - 1)
      tbl.3gram <- mutate(tbl.3gram, gt.probs.3 = lambda.2*Freq/(sum(Freq)))
  pred3 <- tbl.3gram[c("Var1", "gt.probs.3")]}
    else{pred3 <- NA; lambda.2 <- 0}}
  else{pred3 <- NA; lambda.2 <- 0}
  
  if(is.data.frame(tbl.2gram)){ 
    if(nrow(tbl.2gram) !=0){
      lambda.3 <- 1 - lambda.1 - lambda.2
      tbl.2gram <- mutate(tbl.2gram, gt.probs.2 = lambda.3*Freq/(sum(Freq)))
  pred2 <- tbl.2gram[c("Var1", "gt.probs.2")]}
    else{pred2 <- NA}}
  else{pred2 <- NA}
  
  # Have each data frame; merge them based on cases and sum probabilities
  if(is.data.frame(pred4)){
    pred.tbl <- join_all(list(pred4, pred3, pred2), by = "Var1", type = "full")
    pred.tbl[is.na(pred.tbl)] <- 0
    pred.tbl <- mutate(pred.tbl, prob.total = gt.probs.4 + gt.probs.3 + gt.probs.2)
  }
  else if(is.data.frame(pred3)){
    pred.tbl <- join_all(list(pred3, pred2), by = "Var1", type = "full")
    pred.tbl[is.na(pred.tbl)] <- 0
    pred.tbl<- mutate(pred.tbl, prob.total = gt.probs.3 + gt.probs.2)
  }
  else{pred.tbl <- pred2
  pred.tbl$prob.total <- gt.probs.2
  }
  
  # Find the index of the highest number
  pred.idx <- which(pred.tbl$prob.total == max(pred.tbl$prob.total))
  my.prediction <- table.list$word.hash.inv[[as.character(pred.tbl$Var1[[pred.idx]])]]
  
  return(my.prediction)
}
