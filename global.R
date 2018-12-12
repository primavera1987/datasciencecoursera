
library(data.table)

load("NGrams_idx.RData")
InputData <- fread("Ngrams_Final_Sorted.txt", sep = "\t", header=TRUE, data.table = FALSE)
unigrams_data <- InputData[unigrams_idx[1]:unigrams_idx[2], ]
bigrams_data <- InputData[bigrams_idx[1]:bigrams_idx[2], ]
trigrams_data <- InputData[trigrams_idx[1]:trigrams_idx[2], ]
fourgrams_data <- InputData[fourgrams_idx[1]:fourgrams_idx[2], ]
rm(InputData, unigrams_idx, bigrams_idx, trigrams_idx, fourgrams_idx)

source('NextWordPrediction.R')

datacleansing_func <- function(x, flag = TRUE) {
    x <- tolower(x)
    x <- gsub("#\\S+", "", x)
    x <- gsub("\\brt\\b", "", x)
    x <- wordcontractions(x)
    x <- gsub("[^a-z ]", "", x)
    load("profanity.RData")
    x <- removeWords(x, profanity)
    if (flag) { x <- removeWords(x, stopwords("english"))}
    x <- replaceonewords(x)
    tmpData <- stripWhitespace(x)
}

wordcontractions <- function(x) {
    x <- gsub("i'm", "i am", x)
    x <- gsub("can't", "cannot", x)
    x <- gsub("won't", "will not", x)
    x <- gsub("ain't", "am not", x)
    x <- gsub("what's", "what is", x)
    x <- gsub("'d", " would", x)
    x <- gsub("'re", " are", x)
    x <- gsub("n't", " not", x)
    x <- gsub("'ll", " will", x)
    x <- gsub("'ve", " have", x)
    return(x)
}

replaceonewords <- function (x) {
    x <- gsub("\\bb\\b", "be", x)
    x <- gsub("\\bc\\b", "see", x)
    x <- gsub("\\br\\b", "are", x)
    x <- gsub("\\bu\\b", "you", x)
    x <- gsub("\\by\\b", "why", x)
    x <- gsub("\\bo\\b", "oh", x)
}

ngrams_func <- function(data, n) {
    ngram <- NGramTokenizer(data, Weka_control(min = n, max = n))
    removeEntry <- grep("\\bc \\b",ngram)
    if (length(removeEntry) > 0) 
    {
        ngram <- ngram[-c(removeEntry)]
    }
    return(ngram)
}

NextWord <- function(userWords) {
    
    lastWord <- NA
    comparePhrase <- paste("^", paste(userWords, collapse = " "), "\\b", sep = "")

    if (length(userWords)==3) {
        rawMatch <- grep(comparePhrase, fourgrams_data$word)
        lastWord <- word(fourgrams_data$word[rawMatch], -1)
    }
    
    if (length(userWords)==2) {
        rawMatch <- grep(comparePhrase, trigrams_data$word)
        lastWord <- word(trigrams_data$word[rawMatch], -1)
    }
    
    if (length(userWords)==1) {
        rawMatch <- grep(comparePhrase, bigrams_data$word)
        lastWord <- word(bigrams_data$word[rawMatch], -1)
    }
    
    return(lastWord)
    
}

getMatchProb <- function(predictedWord, userIn) {
    
    phraseIn <- paste(userIn, collapse=" ")
    
    # Unigram probability
    temp <- unigrams_data$word %in% userIn
    unigramCount <- unigrams_data$freq[temp] / sum(unigrams_data$freq)
    rm(temp)
    
    # Bigrams
    bigrams <- ngrams_func(phraseIn,2)
    
    # Calculate count term
    if (length(bigrams) != 0) {
        term1 <- NULL
        for (i in 1:length(bigrams)){
            comparePhrase <- paste("^", paste(bigrams[i], collapse = " "), "\\b", sep = "")
            term1 <- c( term1, log( (bigrams_data$freq[grep(comparePhrase, bigrams_data$word)] / sum(bigrams_data$freq) ) / unigramCount[i]) )
        }
    } else {
        term1 <- NULL
    }
    
    # Bigram counts based on predicted words
    relativeProb <- NULL
    for (i in 1:length(predictedWord)) {
        comparePhrase <- NULL
        comparePhrase <- paste("^", paste(tail(userIn, 1), predictedWord[i], collapse = " "), "\\b", sep = "")
        relativeProb <- c(relativeProb, as.numeric(sum(term1) + log( (bigrams_data$freq[grep(comparePhrase, bigrams_data$word)] / sum(bigrams_data$freq) ) / tail(unigramCount, 1) )))
    }
    return(relativeProb)
    
}