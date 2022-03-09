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
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
clean_text <- tm_map(clean_text, content_transformer(removeURL))
inspect(clean_text[1:5])

# Remove company name
clean_text <- tm_map(clean_text, removeWords, c('googl', 'google', 'goog', '…'))

# Remove whitespaces
clean_text <- tm_map(clean_text, stripWhitespace)
inspect(clean_text[1:5])

# Creating Term Document Matrix
tdm <- TermDocumentMatrix(clean_text)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Frequency bar plot of words
x <- rowSums(tdm)
x <- subset(x, x>25)
barplot(x,
        las = 2,
        col = rainbow(50))


# Wordcloud
library(wordcloud)
x <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(x),
          freq = x,
          max.words = 200,
          random.order = F,
          min.freq = 20,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.3))

# Enhanced wordcloud
library(wordcloud2)
x <- data.frame(names(x),x)
colnames(x) <- c('word','frequency')
head(x)

wordcloud2(x,
           size = 0.8,
           shape = 'circle')
