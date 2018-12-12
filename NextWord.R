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