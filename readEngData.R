# Read the english data

# Create corpus and separate out files into character vectors
en.docs <- VCorpus(DirSource("./Coursera-SwiftKey/final/en_US"))
en.blog <- as.character(en.docs[["en_US.blogs.txt"]])
en.news <- as.character(en.docs[["en_US.news.txt"]]) #This is a list!
en.twitter <- as.character(en.docs[["en_US.twitter.txt"]]) #This is a list!

# Find longest line
blog.lengths <- sapply(en.blog,nchar)
news.lengths <- sapply(en.news$content,nchar)
twitter.lengths <- sapply(en.twitter$content,nchar)

max(blog.lengths) #40,835
max(news.lengths) #5760
max(twitter.lengths) #213

# In twitter, find all lines with "love" and "hate", then divide
love.tweets <- length(grep("love", en.twitter$content))
hate.tweets <- length(grep("hate", en.twitter$content))
love.tweets/hate.tweets #4.108592

# Find the line matching "biostat"
my.line <- grep("biostat", en.twitter$content)
en.twitter$content[[my.line]] 
#"i know how you feel.. i have biostats on tuesday and i have yet to study =/"

# Find the number of lines matching the kickboxing line
pattern <- "A computer once beat me at chess, but it was no match for me at kickboxing"
length(grep(pattern, en.twitter$content)) #3
