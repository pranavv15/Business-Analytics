# Google Tweets

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
x <- subset(x, x>50)
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
          scale = c(5,0.3))

# Enhanced wordcloud
library(wordcloud2)
x <- data.frame(names(x),x)
x <- x[-c(3), ]
colnames(x) <- c('word','frequency')
head(x)

wordcloud2(x,
           size = 0.8,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(x,"GA")

# Library not working properly
# Used help from : https://github.com/Lchiffon/wordcloud2/issues/12
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
letterCloud(x,
            word = "Google",
            size = 2)

# Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Save tweets
tweets <- iconv(google$text, to = 'utf-8-mac')

# Get Sentiment Scores
google_scores <- get_nrc_sentiment(tweets)
head(google_scores)
# tweets[2]
# get_nrc_sentiment('intelligence')

# Bar Plot
barplot(colSums(google_scores),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Google Tweets')

#-------------------------------------------------------------------------------------------------

# Apple Tweets

# Reading csv file
apple <- read.csv(file.choose(), header = TRUE)
str(apple)

# Building corpus
library(tm)
corpus <- iconv(apple$text, to = 'utf-8-mac')
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
clean_text <- tm_map(clean_text, removeWords, c('apple', 'aapl', '…'))

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
          scale = c(5,0.3))

# Enhanced wordcloud
library(wordcloud2)
x <- data.frame(names(x),x)
x <- x[-c(3), ]
colnames(x) <- c('word','frequency')
head(x)

wordcloud2(x,
           size = 0.8,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Library not working properly
# Used help from : https://github.com/Lchiffon/wordcloud2/issues/12
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
letterCloud(x,
            word = "Apple",
            size = 2)

# Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Save tweets
tweets <- iconv(apple$text, to = 'utf-8-mac')

# Get Sentiment Scores
apple_scores <- get_nrc_sentiment(tweets)
head(apple_scores)
# tweets[2]
# get_nrc_sentiment('intelligence')

# Bar Plot
barplot(colSums(apple_scores),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')

#-------------------------------------------------------------------------------------------------

# Amazon Tweets

# Reading csv file
amazon <- read.csv(file.choose(), header = TRUE)
str(amazon)

# Building corpus
library(tm)
corpus <- iconv(amazon$text, to = 'utf-8-mac')
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
clean_text <- tm_map(clean_text, removeWords, c('amzn', 'amazon', '…'))

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
          scale = c(5,0.3))

# Enhanced wordcloud
library(wordcloud2)
x <- data.frame(names(x),x)
x <- x[-c(3), ]
colnames(x) <- c('word','frequency')
head(x)

wordcloud2(x,
           size = 0.8,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Library not working properly
# Used help from : https://github.com/Lchiffon/wordcloud2/issues/12
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
letterCloud(x,
            word = "Amazon",
            size = 2)

# Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Save tweets
tweets <- iconv(amazon$text, to = 'utf-8-mac')

# Get Sentiment Scores
amazon_scores <- get_nrc_sentiment(tweets)
head(amazon_scores)
# tweets[2]
# get_nrc_sentiment('intelligence')

# Bar Plot
barplot(colSums(amazon_scores),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Amazon Tweets')

