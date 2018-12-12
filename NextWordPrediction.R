
NextWordPrediction <- function(phraseIn, wordsOut = 5, flag = TRUE) {

    userWords <- txt.to.words(datacleansing_func(phraseIn, flag))
    numWords <- length(userWords)
    
    # If user input is blank, return unigrams
    if (numWords==0){
        lastWord <- as.data.frame(as.character(unigrams_data$word[1:wordsOut]))
        return(lastWord)
    }
    
    # Limit user input to the last 3 words
    if (numWords > 3) {
        numWords <- 3
        userWords <- tail(userWords, numWords)
    }
    
    # Dedup results
    matchLen <- NULL
    matchList <- NULL
    for (i in numWords:1) {
        tempResults <- NULL
        tempResults <- NextWord(tail(userWords, i))
        
        if (is.na(tempResults[1])) {
            matchLen <- c(matchLen, 0)
        } else {
            
            logicRemove <- tempResults %in% matchList
            matchList <- c(matchList, tempResults[!logicRemove])
            
            matchLen <- c(matchLen, length(tempResults[!logicRemove]))
            rm(logicRemove, tempResults)
        }
        if (sum(matchLen) > wordsOut) {break}
        
    }
    
    # If user input returns in a non-match, return unigrams
    if (sum(matchLen)==0) {
        lastWord <- as.data.frame(as.character(unigrams_data$word[1:wordsOut]))
        return(lastWord)
    }

    revInd <- numWords:1
    revInd <- revInd[!(matchLen %in% 0)]
    matchLen <- matchLen[!(matchLen %in% 0)]
    
    # Stupid backoff adjustment
    stepDown <- NULL
    for (i in 1:length(matchLen) ) {
        
        if(matchLen[i]!=0){
            
            if (i==1){
                stepDown <- c(stepDown, rep(log(1), matchLen[i]) )
            } else {
                stepDown <- c(stepDown, rep(log(0.4^(i-1)), matchLen[i]))
            }
            
        } else {
            stepDown <- c(stepDown, NULL)
        }
        
    }
    
    if (length(matchList)>20) {
        matchList <- matchList[1:20]
        stepDown <- stepDown[1:20]
    }
    
    # Log probability
    prob <- NULL
    for (i in 1:length(unique(stepDown))) {
        temp <- NULL
        temp <- stepDown %in% unique(stepDown)[i]
        
        prob <- c(prob, getMatchProb( matchList[temp], tail(userWords, revInd[i]) ))
    }
    
    # Output sorted results
    #Predictions <- data.frame( word=matchList, logProb=as.numeric(prob+stepDown), stringsAsFactors = FALSE )
    #Predictions <- arrange(Predictions, desc(logProb))
    #return(Predictions[1:wordsOut,])
    
    Predictions <- data.frame( word=matchList, stringsAsFactors = FALSE )
    return(Predictions[1:wordsOut,])
    
}