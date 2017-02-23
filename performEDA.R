# Explore the dataset and make some figures/tables

# Step 1: Load the (english) dataset
en.docs <- VCorpus(DirSource("./Coursera-SwiftKey/final/en_US"))
en.blog <- as.character(en.docs[["en_US.blogs.txt"]])
en.news <- as.character(en.docs[["en_US.news.txt"]]) #This is a list!
en.twitter <- as.character(en.docs[["en_US.twitter.txt"]]) #This is a list!

# Step 2: Somehow divide datasets into words...need to recognize whitespace
library(stringi)
# Make all lower case
blah <- tolower(blah)
# Remove punctuation(?)
singlets <- gsub(",|'|\\.","",blah)
# Separate into all words and grab that element as a vector
singlets <- stri_split(singlets,regex="\\W")[[1]]

# First, define a function that pastes together a given character vector
paste.vector <- function(x){
  paste.vector <- x[1]
  for(i in 2:length(x)) paste.vector <- paste(paste.vector,x[i])
  paste.vector
}

# Define a function that takes in a phrase length (in words) and creates a list of 
# phrases that long (default of 2 word phrases)
phrases <- function(all.words, phrase.size = 2){
  # Create the character vector and then fill it with a loop
  all.phrases <- character()
  for(i in 1:(length(all.words) - (phrase.size-1))){
    # Use the previously defined function to paste the vector
    all.phrases <- append(all.phrases,paste.vector(all.words[i:(i+phrase.size-1)]))
  }
  all.phrases
}

# Capture all couplets and triplets, and maybe more (recall that blah2 is the singlets)
couplets <- phrases(singlets,2)
triplets <- phrases(singlets,3)

# Step 3: Count word frequencies to construct data frames

# Simplest way I can think of
singlet.df <- data.frame(table(singlets))
couplet.df <- data.frame(table(couplets))
triplet.df <- data.frame(table(triplets))

# Order by frequencies
singlet.df <- singlet.df[order(singlet.df$Freq,decreasing=TRUE),]
couplet.df <- couplet.df[order(couplet.df$Freq,decreasing=TRUE),]
triplet.df <- triplet.df[order(triplet.df$Freq,decreasing=TRUE),]

# Step 4: Make barplots (phrases on x, frequencies on y)
with(singlet.df[1:10,],barplot(height=Freq,names.arg = singlets,
                        horiz=FALSE,cex.names=0.5))
with(couplet.df[1:10,],barplot(height=Freq,names.arg = couplets,
                        horiz=FALSE,cex.names=0.5))
with(triplet.df[1:10,],barplot(height=Freq,names.arg = triplets,
                        horiz=FALSE,cex.names=0.5))
## Still need to figure out how to rotate labels! Then change horiz=TRUE

# Step 5: Investigate # of words needed to cover a given % of the dataset

# Take in the singlets df and sum the frequencies...then figure out the cutoff
find.cutoff <- function(singlet.df,percentage=50){
  # Find the cutoff of words needed
  cutoff <- percentage*sum(singlet.df$Freq)/100
  # Use a while loop to get to the cutoff
  word.sum <- 0
  word.idx <- 0
  while(word.sum < cutoff){
    word.idx <- word.idx + 1
    word.sum <- word.sum + singlet.df$Freq[word.idx]
  }
  word.idx
}

# Find cutoff for 50% and 90%
cutoff.90 <- find.cutoff(singlet.df,90)
cutoff.50 <- find.cutoff(singlet.df,50)

#Step 6. How do we evaluate words from foreign languages?

# We could take an intersect between english and the 3 other languages....
# If you intersect the singlets dictionary of english with each one in turn,
# then you get the overlapping words in each case. Taking the union of these would
# at least hack an estimate of the foreign words in the english word set
