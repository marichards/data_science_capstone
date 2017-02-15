#------------------------

# Load and explore the blogs data sets

# First load the tm package and create a corpus
library(tm)
en.docs <- Corpus(DirSource("./Coursera-SwiftKey/final/en_US"))
en.blog <- en.docs[["en_US.blogs.txt"]]

#de.docs <- Corpus(DirSource("./Coursera-SwiftKey/final/de_DE"))
#de.blog <- de.docs[[1]]

#fi.docs <- Corpus(DirSource("./Coursera-SwiftKey/final/fi_FI"))
#fi.blog <- fi.docs[[1]]

#ru.docs <- Corpus(DirSource("./Coursera-SwiftKey/final/ru_RU"))
#ru.blog <- ru.docs[[1]]


