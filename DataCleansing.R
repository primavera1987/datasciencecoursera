# Load required packages

library(stringi)
library(stringr)
library(ggplot2)
library(NLP)
library(dplyr)
library(tm)
library(wordcloud)
library(RWeka)
library(stylo)
library(data.table)

# Set working directory 

setwd("/Users/MadhaviReddy/Capstone/Word_Prediction/")

# Load the data from Blogs, News and Twitter datasets

rm(list = ls())

file.blogs <- file("Data/en_US.blogs.txt")
file.news <- file("Data/en_US.news.txt", open="rb") # rb to open a binary file
file.twitter <- file("Data/en_US.twitter.txt")

blogs_raw <- readLines(file.blogs)
news_raw <- readLines(file.news)
twitter_raw <- readLines(file.twitter)

# Data Cleansing

# Get a sample of the data

set.seed (12345) 
blogs <- sample(blogs_raw,100000)
news <- sample(news_raw,100000)
twitter <- sample(twitter_raw,100000)

# Create RData files here on, saves workspace and includes the function and value objects created during an open session in R; 

save(blogs, news, twitter, file = "RawData.RData")

rm(list=ls())

load("RawData.RData")

# Convert to lower case
blogs <- tolower(blogs)
news <- tolower(news)
twitter <- tolower(twitter)

# Remove URLs
blogs <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", blogs)
news <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", news)
twitter <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", twitter)

# Remove twitter hashtags
blogs <- gsub("#\\S+", " ", blogs)
news <- gsub("#\\S+", " ", news)
twitter <- gsub("#\\S+", " ", twitter)

# Remove retweets
blogs <- gsub("\\brt\\b", " ", blogs)
news <- gsub("\\brt\\b", " ", news)
twitter <- gsub("\\brt\\b", " ", twitter)

# Expand word contractions
blogs <- gsub("i'm", "i am", blogs)
news <- gsub("i'm", "i am", news)
twitter <- gsub("i'm", "i am", twitter)

blogs <- gsub("it's", "it is", blogs)
news <- gsub("it's", "it is", news)
twitter <- gsub("it's", "it is", twitter)

blogs <- gsub("can't", "cannot", blogs)
news <- gsub("can't", "cannot", news)
twitter <- gsub("can't", "cannot", twitter)

blogs <- gsub("won't", "will not", blogs)
news <- gsub("won't", "will not", news)
twitter <- gsub("won't", "will not", twitter)

blogs <- gsub("ain't", "am not", blogs)
news <- gsub("ain't", "am not", news)
twitter <- gsub("ain't", "am not", twitter)

blogs <- gsub("what's", "what is", blogs)
news <- gsub("what's", "what is", news)
twitter <- gsub("what's", "what is", twitter)

blogs <- gsub("'d", " would", blogs)
news <- gsub("'d", " would", news)
twitter <- gsub("'d", " would", twitter)

blogs <- gsub("'re", " are", blogs)
news <- gsub("'re", " are", news)
twitter <- gsub("'re", " are", twitter)

blogs <- gsub("n't", " not", blogs)
news <- gsub("n't", " not", news)
twitter <- gsub("n't", " not", twitter)

blogs <- gsub("'ll", " will", blogs)
news <- gsub("'ll", " will", news)
twitter <- gsub("'ll", " will", twitter)

blogs <- gsub("'ve", " have", blogs)
news <- gsub("'ve", " have", news)
twitter <- gsub("'ve", " have", twitter)

blogs <- gsub("'s", " is", blogs)
news <- gsub("'s", " is", news)
twitter <- gsub("'s", " is", twitter)

# Remove stopwords
blogs <- removeWords(blogs,stopwords("english"))
news <- removeWords(news,stopwords("english"))
twitter <- removeWords(twitter,stopwords("english"))

# Remove profanities
load("profanity.RData")
blogs <- removeWords(blogs,profanity)
news <- removeWords(news,profanity)
twitter <- removeWords(twitter,profanity)

# Remove numbers and special characters.
blogs <- gsub("[^a-z ]", " ", blogs)
news <- gsub("[^a-z ]", " ", news)
twitter <- gsub("[^a-z ]", " ", twitter)

# Remove extra spaces
blogs <- stripWhitespace(blogs)
news <- stripWhitespace(news)
twitter <- stripWhitespace(twitter)

# Save cleansed data to a file 
blogs_cleansed <- blogs
news_cleansed <- news
twitter_cleansed <- twitter

save(blogs_cleansed, news_cleansed, twitter_cleansed, file = "CleansedData.RData")

rm(list = ls())

load("CleansedData.RData")

# Split text to words and save the file. This will be used to create a unigram list.

blogs_words <- txt.to.words(blogs_cleansed)
news_words <- txt.to.words(news_cleansed)
twitter_words <- txt.to.words(twitter_cleansed)

save(blogs_words, news_words, twitter_words, file = "WordList_Unigram.RData")

rm(blogs_words, news_words, twitter_words)

# Create corpora for the cleansed data sets

blogs_corpus <- VCorpus(VectorSource(blogs_cleansed))
news_corpus <- VCorpus(VectorSource(news_cleansed))
twitter_corpus <- VCorpus(VectorSource(twitter_cleansed))

save(blogs_corpus, news_corpus, twitter_corpus, file = "CorpusData.RData")

rm(blogs_cleansed, news_cleansed, twitter_cleansed)

# Split text to words from the corpus data, these lists will be used to create bigram, trigram and fourgram tables.

blogs_corpus_words <- txt.to.words(blogs_corpus)
news_corpus_words <- txt.to.words(news_corpus)
twitter_corpus_words <- txt.to.words(twitter_corpus)

save(blogs_corpus_words, news_corpus_words, twitter_corpus_words, file = "WordList_Corpus.RData")

rm(blogs_corpus, news_corpus, twitter_corpus)
rm(blogs_corpus_words, news_corpus_words, twitter_corpus_words)

# Create N-Grams

ngrams_func <- function(data, n) {
    ngram <- NGramTokenizer(data, Weka_control(min = n, max = n))
    removeEntry <- grep("\\bc \\b",ngram)
    if (length(removeEntry) > 0) 
    {
        ngram <- ngram[-c(removeEntry)]
    }
    return(ngram)
}

# Bigrams

load("WordList_Corpus.RData")
rm(news_corpus_words, twitter_corpus_words)
bigrams_blogs <- ngrams_func(blogs_corpus_words, 2)
rm(blogs_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, twitter_corpus_words)
bigrams_news <- ngrams_func(news_corpus_words, 2)
rm(news_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, news_corpus_words)
bigrams_twitter <- ngrams_func(twitter_corpus_words, 2)
rm(twitter_corpus_words)

save(bigrams_blogs, bigrams_news, bigrams_twitter, file = "Bigrams.RData")

rm(bigrams_blogs, bigrams_news, bigrams_twitter)

# Trigrams

load("WordList_Corpus.RData")
rm(news_corpus_words, twitter_corpus_words)
trigrams_blogs <- ngrams_func(blogs_corpus_words, 3)
rm(blogs_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, twitter_corpus_words)
trigrams_news <- ngrams_func(news_corpus_words, 3)
rm(news_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, news_corpus_words)
trigrams_twitter <- ngrams_func(twitter_corpus_words, 3)
rm(twitter_corpus_words)

save(trigrams_blogs, trigrams_news, trigrams_twitter, file = "Trigrams.RData")

rm(trigrams_blogs, trigrams_news, trigrams_twitter)

# Fourgrams

load("WordList_Corpus.RData")
rm(news_corpus_words, twitter_corpus_words)
fourgrams_blogs <- ngrams_func(blogs_corpus_words, 4)
rm(blogs_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, twitter_corpus_words)
fourgrams_news <- ngrams_func(news_corpus_words, 4)
rm(news_corpus_words)

load("WordList_Corpus.RData")
rm(blogs_corpus_words, news_corpus_words)
fourgrams_twitter <- ngrams_func(twitter_corpus_words, 4)
rm(twitter_corpus_words)

save(fourgrams_blogs, fourgrams_news, fourgrams_twitter, file = "Fourgrams.RData")

rm(fourgrams_blogs, fourgrams_news, fourgrams_twitter)

rm(list=ls())

# Create tables for bigrams, trigrams and fourgrams

load("WordList_Unigram.RData")
unigrams <- table(c(blogs_words, news_words, twitter_words))
save(unigrams, file = "Unigrams_Table.RData")
rm(unigrams)

load("Bigrams.RData")
bigrams_table <- table(c(bigrams_blogs, bigrams_news, bigrams_twitter))
save(bigrams_table, file = "Bigrams_Table.RData")
rm(list=ls())

load("Trigrams.RData")
trigrams_table <- table(c(trigrams_blogs, trigrams_news, trigrams_twitter))
save(trigrams_table, file = "Trigrams_Table.RData")
rm(list=ls())

load("Fourgrams.RData")
fourgrams_table <- table(c(fourgrams_blogs, fourgrams_news, fourgrams_twitter))
save(fourgrams_table, file = "Fourgrams_Table.RData")
rm(list=ls())

# Create filtered tables

load ("Unigrams_Table.RData")
unigrams_freq <- data.frame(word=names(unigrams), freq=as.numeric(unigrams), stringsAsFactors = FALSE)
unigrams_freq <- filter(unigrams_freq, freq != 1)
unigrams_freq <- arrange(unigrams_freq, desc(freq))
save(unigrams_freq, file = "Unigrams_Table_Freq.RData")
rm(list = ls())

load("Bigrams_Table.RData")
bigrams_freq <- data.frame(word=names(bigrams_table), freq=as.numeric(bigrams_table), stringsAsFactors = FALSE)
bigrams_freq <- filter(bigrams_freq, freq != 1)
bigrams_freq <- arrange(bigrams_freq, desc(freq))
save(bigrams_freq, file = "Bigrams_Table_Freq.RData")
rm(list=ls())

load("Trigrams_Table.RData")
trigrams_freq <- data.frame(word=names(trigrams_table), freq=as.numeric(trigrams_table), stringsAsFactors = FALSE)
trigrams_freq <- filter(trigrams_freq, freq != 1)
trigrams_freq <- arrange(trigrams_freq, desc(freq))
save(trigrams_freq, file = "Trigrams_Table_Freq.RData")
rm(list=ls())

load("Fourgrams_Table.RData")
fourgrams_freq <- data.frame(word=names(fourgrams_table), freq=as.numeric(fourgrams_table), stringsAsFactors = FALSE)
fourgrams_freq <- filter(fourgrams_freq, freq != 1)
fourgrams_freq <- arrange(fourgrams_freq, desc(freq))
save(fourgrams_freq, file = "Fourgrams_Table_Freq.RData")
rm(list=ls())

# Create indexs for unigram, bigram, trigram and fourgram freq tables

load("Unigrams_Table_Freq.RData")
load("Bigrams_Table_Freq.RData")
load("Trigrams_Table_Freq.RData")
load("Fourgrams_Table_Freq.RData")

unigrams_idx <- c(1,nrow(unigrams_freq))
bigrams_idx <- c(unigrams_idx[2]+1, unigrams_idx[2]+nrow(bigrams_freq))
trigrams_idx <- c(bigrams_idx[2]+1, bigrams_idx[2]+nrow(trigrams_freq))
fourgrams_idx <- c(trigrams_idx[2]+1, trigrams_idx[2]+nrow(fourgrams_freq))
save(unigrams_idx, bigrams_idx, trigrams_idx, fourgrams_idx, file = "NGrams_idx.RData")
rm(unigrams_idx, bigrams_idx, trigrams_idx, fourgrams_idx)

# Concatenate the tables and write sorted data to a text file.

ngrams_table <- rbind(unigrams_freq, bigrams_freq, trigrams_freq, fourgrams_freq)
rm(unigrams_freq, bigrams_freq, trigrams_freq, fourgrams_freq)

save(ngrams_table, file = "Ngrams_Final.RData")

write.table(ngrams_table, file = "Ngrams_Final_Sorted.txt", sep = "\t", row.names = FALSE)
rm(list=ls())


