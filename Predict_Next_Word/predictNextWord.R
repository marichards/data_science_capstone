## predictNextWord.R
suppressMessages(library(tm))
suppressMessages(library(stringr))
suppressMessages(library(hash))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))

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
  phrase <- trimws(phrase)
  phrase <- stripWhitespace(phrase)
  
  # If there's no words left, return an error
  if(nchar(phrase) == 0)
    stop("Sorry, none of the words you entered were recognized. Please try again.")
  
  
  # Create list of integers for stopwords
  stop.ints <- numeric(length = length(stopwords()))
  altered.stops <- removePunctuation(stopwords())
  altered.stops <- intersect(altered.stops, keys(table.list$word.hash))
  for(i in 1:length(altered.stops)){
    {stop.ints[[i]] <- table.list$word.hash[[altered.stops[[i]]]]}
  }
  
  # Create a short function that converts an ID to a word
  convert.to.word <- function(nint,word.hash.inv){
    
    # Do an operation I could do once
    ngram <- word.hash.inv[[nint]]
    ngram
  }
  
  # Full process if it's 3 words or more
  if (length(strsplit(phrase, split = " ")[[1]]) > 2){
  # Do the operations for 3-word phrases
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
    pred.tbl <- mutate(pred.tbl, Probability = gt.probs.4 + gt.probs.3 + gt.probs.2)
  }
  else if(is.data.frame(pred3)){
    pred.tbl <- join_all(list(pred3, pred2), by = "Var1", type = "full")
    pred.tbl[is.na(pred.tbl)] <- 0
    pred.tbl<- mutate(pred.tbl, Probability = gt.probs.3 + gt.probs.2)
  }
  else{pred.tbl <- pred2
  pred.tbl <- pred2
  pred.tbl$Probability <- pred.tbl$gt.probs.2
  }
  
  # Sort the table, grabbing total probs and the words for the top 5
  pred.tbl <- head(pred.tbl[order(pred.tbl$Probability, decreasing = TRUE),
                   c("Var1", "Probability")],10)
  
  word.ints <- as.character(pred.tbl$Var1)
  pred.words <- character(length = length(word.ints))
  for(i in 1:length(word.ints)){
    pred.words[[i]] <- convert.to.word(word.ints[[i]], table.list$word.hash.inv)
  }
  pred.tbl$Word <- pred.words
  top.prediction <- pred.words[[1]]
  pred.tbl <- pred.tbl[c("Word","Probability")]
  
  return(list(top.prediction = top.prediction,
              pred.tbl = pred.tbl))}
  
  ### 2-word case
  else if(length(strsplit(phrase, split = " ")[[1]]) == 2){
    # Do the operations for 2-word phrases
    gram2 <- table.list$word.hash[[word(phrase, -1)]]
    gram1 <- table.list$word.hash[[word(phrase, -2)]]
    # Filter answers that share the last gram and so forth using dplyr
    last2 <- filter(table.list$trips, 
                    word.1 == gram1 & word.2 == gram2)# & !(word.3 %in% last4$word.5) & !(word.3 %in% last3$word.4)
    last1 <- filter(table.list$coups, 
                    word.1 == gram2)#& !(word.2 %in% last4$word.5) & !(word.2 %in% last3$word.4) & !(word.2 %in% last2$word.3))
    ## Grab the table of each one, summing up frequencies
    tbl.3gram <- data.frame(table(last2$word.3))
    tbl.2gram <- data.frame(table(last1$word.2))
    # Remove rows of 1's
    tbl.3gram <- subset(tbl.3gram, Freq > 1)
    tbl.2gram <- subset(tbl.2gram, Freq > 1)
    # Filter out stopwords  
    tbl.3gram <- filter(tbl.3gram, !(Var1 %in% stop.ints))
    tbl.2gram <- filter(tbl.2gram, !(Var1 %in% stop.ints))
    # Sort it by freq
    tbl.3gram <- tbl.3gram[order(tbl.3gram$Freq, decreasing = TRUE),]
    tbl.2gram <- tbl.2gram[order(tbl.2gram$Freq, decreasing = TRUE),]
    
    # Return things if they're not null
    if(is.data.frame(tbl.3gram)){
      if(nrow(tbl.3gram) !=0){
        lambda.2 <- 0.7
        tbl.3gram <- mutate(tbl.3gram, gt.probs.3 = lambda.2*Freq/(sum(Freq)))
        pred3 <- tbl.3gram[c("Var1", "gt.probs.3")]}
      else{pred3 <- NA; lambda.2 <- 0}}
    else{pred3 <- NA; lambda.2 <- 0}
    if(is.data.frame(tbl.2gram)){ 
      if(nrow(tbl.2gram) !=0){
        lambda.3 <- 1 - lambda.2
        tbl.2gram <- mutate(tbl.2gram, gt.probs.2 = lambda.3*Freq/(sum(Freq)))
        pred2 <- tbl.2gram[c("Var1", "gt.probs.2")]}
      else{pred2 <- NA}}
    else{pred2 <- NA}
    
    # Have each data frame; merge them based on cases and sum probabilities
    if(is.data.frame(pred3)){
      pred.tbl <- join_all(list(pred3, pred2), by = "Var1", type = "full")
      pred.tbl[is.na(pred.tbl)] <- 0
      pred.tbl<- mutate(pred.tbl, Probability = gt.probs.3 + gt.probs.2)
    }
    else{pred.tbl <- pred2
    pred.tbl$Probability <- pred.tbl$gt.probs.2
    }
    
    # Sort the table, grabbing total probs and the words for the top 5
    pred.tbl <- head(pred.tbl[order(pred.tbl$Probability, decreasing = TRUE),
                              c("Var1", "Probability")],10)
    
    word.ints <- as.character(pred.tbl$Var1)
    pred.words <- character(length = length(word.ints))
    for(i in 1:length(word.ints)){
      pred.words[[i]] <- convert.to.word(word.ints[[i]], table.list$word.hash.inv)
    }
    pred.tbl$Word <- pred.words
    top.prediction <- pred.words[[1]]
    pred.tbl <- pred.tbl[c("Word","Probability")]
    
    return(list(top.prediction = top.prediction,
                pred.tbl = pred.tbl))}
  
  ## 1-word case
  else{
    # Do the operations for 1-word phrases
    gram1 <- table.list$word.hash[[word(phrase, -1)]]
    # Filter answers that share the last gram and so forth using dplyr
    last1 <- filter(table.list$coups, 
                    word.1 == gram1)#& !(word.2 %in% last4$word.5) & !(word.2 %in% last3$word.4) & !(word.2 %in% last2$word.3))
    ## Grab the table of each one, summing up frequencies
    tbl.2gram <- data.frame(table(last1$word.2))
    # Remove rows of 1's
    tbl.2gram <- subset(tbl.2gram, Freq > 1)
    # Filter out stopwords  
    tbl.2gram <- filter(tbl.2gram, !(Var1 %in% stop.ints))
    # Sort it by freq
    tbl.2gram <- tbl.2gram[order(tbl.2gram$Freq, decreasing = TRUE),]

    # Return things if they're not null
    if(is.data.frame(tbl.2gram)){ 
      if(nrow(tbl.2gram) !=0){
        lambda.3 <- 1
        tbl.2gram <- mutate(tbl.2gram, Probability = lambda.3*Freq/(sum(Freq)))
        pred.tbl <- tbl.2gram[c("Var1", "Probability")]}
      else{pred.tbl <- NA}}
    else{pred.tbl <- NA}
    
    # Sort the table, grabbing total probs and the words for the top 5
    pred.tbl <- head(pred.tbl[order(pred.tbl$Probability, decreasing = TRUE),
                              c("Var1", "Probability")],10)
    
    word.ints <- as.character(pred.tbl$Var1)
    pred.words <- character(length = length(word.ints))
    for(i in 1:length(word.ints)){
      pred.words[[i]] <- convert.to.word(word.ints[[i]], table.list$word.hash.inv)
    }
    pred.tbl$Word <- pred.words
    top.prediction <- pred.words[[1]]
    pred.tbl <- pred.tbl[c("Word","Probability")]
    
    return(list(top.prediction = top.prediction,
                pred.tbl = pred.tbl))}
}
