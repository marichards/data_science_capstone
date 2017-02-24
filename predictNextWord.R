## predictNextWord.R

predictNextWord <- function(phrase){

  # Load and clean the dataset
  suppressMessages(library(tm))
  en.docs <- VCorpus(DirSource("../final/en_US"),
                     readerControl = list(language="en"))
  en.blog <- as.character(en.docs[["en_US.blogs.txt"]])
  en.news <- as.character(en.docs[["en_US.news.txt"]]) #This is a list
  en.twitter <- as.character(en.docs[["en_US.twitter.txt"]]) #This is a list
  rm(en.docs); invisible(gc())
  
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
  en.data <- removePunctuation(en.data)
  en.data <- removeNumbers(en.data)
  en.data <- stripWhitespace(en.data)
  
  # Remove bad words and non-ASCII
  bad.words <- read.table("./swearWords.txt")
  en.data <- removeWords(en.data,bad.words$V1)
  en.data <- iconv(en.data,"UTF-8","ASCII",sub="")
  
  # Create tables of quads, triplets, and couplets
  triplets <- NGramTokenizer(en.data, Weka_control(min=3,max=3))
  triplet.df <- data.frame(table(triplets)); rm(triplets)
  invisible(gc())
  
  couplets <- NGramTokenizer(en.data, Weka_control(min=2,max=2))
  couplet.df <- data.frame(table(couplets)); rm(couplets)
  invisible(gc())
  
  # Take in the input and grab the last n grams for 4, 3, 2 (just 3 and 2 for now)
  #last.4grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=4,max=4)),1))
  last.3grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=3,max=3)),1))
  last.2grams <- tolower(tail(NGramTokenizer(phrase, Weka_control(min=2,max=2)),1))
  ### Through here, this takes 218 seconds
  
  
  
  
  # Construct table of frequencies for 4 grams
  
  # Construct table of frequencies for 3 grams
  
  # Construct table of frequencies for 2 grams
  return("All done")

}