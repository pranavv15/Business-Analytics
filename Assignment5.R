# Reading csv file

google <- read.csv(file.choose(), header = TRUE)
str(google)

# Building corpus

library(tm)
corpus <- iconv(google$text, to = 'utf-8-mac')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Data Cleaning

# Make everything lowercase
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

# Remove Punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# Remove Numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# Remove common english words
clean_text <- tm_map(corpus, removeWords, stopwords('english'))
inspect(clean_text[1:5])

# Remove all the urls


