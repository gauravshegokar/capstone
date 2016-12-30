library(stringr)
library(tau)
library(dplyr)
library(tm)

dfUni <- readRDS("./dfUni.RDS")
dfBi <- readRDS("./dfBi.RDS")
dfTri <- readRDS("./dfTri.RDS")
dfQuad <- readRDS("./dfQuad.RDS")

## helper functions
getLastTokens <- function(phrase, tokens = 3){
    ## remove punctuations without apostrophe 
    phrase <- gsub("[^[:alnum:][:space:]']", "", phrase)
    phrase <- removeNumbers(phrase)
    phrase <- stripWhitespace(phrase)
    phrase <- str_trim(phrase)
    phrase <- tolower(phrase)
    
    
    tail(as.list(str_split(phrase," "))[[1]],n = tokens)
}

getPredictionFromNgram <- function(tokens, df){
    wordsCon <- str_c(tokens,collapse =" ")
    pred <- filter(df, phrase == wordsCon)
    pred <- mutate(pred, prob = freq/sum(freq))
    pred
}

predictNextWord <- function(phrase){
    tokens <- getLastTokens(phrase,3)
    if(length(tokens) == 3){
        qPred <- getPredictionFromNgram(tokens,dfQuad)
    }
    tokens <- getLastTokens(phrase,2)
    if(length(tokens) == 2){
        tPred <- getPredictionFromNgram(tokens,dfTri)
    }
    tokens <- getLastTokens(phrase,1)
    if(length(tokens) == 1){
        bPred <- getPredictionFromNgram(tokens, dfBi)
    }
    pred <- data.frame(nextword = character(), fscore = numeric())
    if(exists("qPred") && nrow(qPred) > 0){
        qPred$fscore <- qPred$prob
        if(nrow(qPred) >=3){
            pred <- qPred[1:3,c("nextword", "fscore")]
            droplevels(pred)
            return(as.character(pred$nextword))
        }else{
            pred <- qPred
        }
    }
    if(exists("tPred") && nrow(tPred) > 0){
        tPred$fscore <- 0.4 * tPred$prob
        tPred <- filter(tPred, !nextword %in% pred$nextword)
        pred <- rbind(pred, tPred)
        pred <- arrange(pred, desc(fscore))
        if(nrow(pred) >=3){
            pred <- pred[1:3,c("nextword", "fscore")]
            droplevels(pred)
            return(as.character(pred$nextword))
        }
    }
    if(exists("bPred")&& nrow(bPred) > 0){
        bPred$fscore <- 0.4 * 0.4 * bPred$prob
        bPred <- filter(bPred, !nextword %in% pred$nextword)
        pred <- rbind(pred, bPred)
        pred <- arrange(pred, desc(fscore))
        if(nrow(pred) >=3){
            pred <- pred[1:3,c("nextword", "fscore")]
            droplevels(pred)
            return(as.character(pred$nextword))
        }
    }
    if(exists("pred") && nrow(pred) > 0){
        predV <- as.vector(pred$nextword)
        predU <- as.vector(head(dfUni$word,3))
        return(as.character(c(predV,predU)[1:3]))
    }
    droplevels(dfUni)
    as.character(dfUni$word[1:3])
}
