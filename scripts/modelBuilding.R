## merge training data files
library(stringr)
library(tau)
library(dplyr)
library(tm)

sourceDir <- "./dataset/clean/en_US/";
files <- list.files(sourceDir);
combinedData <- vector(mode = "character")
for(file in files){
    sourceFile <- paste(sourceDir, file, sep = "/");
    tempFile <- readLines(sourceFile)
    combinedData <- c(combinedData,tempFile)
}
dir.create("./dataset/clean/en_US/train")
fileConn<-file("./dataset/clean/en_US/train/train.txt")
writeLines(combinedData, fileConn);
close(fileConn);
rm(list = ls())

## model building
corp <- readLines("./dataset/clean/en_US/train/train.txt")
unigram <- textcnt(corp, method = "string", split = "[[:space:]]", n = 1L, decreasing = T)
bigram <- textcnt(corp, method = "string", split = "[[:space:]]", n = 2L, decreasing = T)
trigram <- textcnt(corp, method = "string", split = "[[:space:]]", n = 3L, decreasing = T)
quadgram1 <- textcnt(corp[1:round(length(corp)/2)], method = "string", split = "[[:space:]]", n = 4L, decreasing = T)
saveRDS("./quadgram1.RDS",object = quadgram1)
rm(quadgram1)

quadgram2 <- textcnt(corp[round(length(corp)/2) : length(corp)], method = "string", split = "[[:space:]]", n = 4L, decreasing = T)
saveRDS("./quadgram2.RDS",object = quadgram2)
rm(quadgram2)

quadgram1 <- readRDS("./quadgram1.RDS")
quadgram2 <- readRDS("./quadgram2.RDS")


dfUni <- data.frame(word = names(unigram), freq = as.matrix(unigram))
rownames(dfUni)<- NULL
rm(unigram)

dfBi <- data.frame(word = names(bigram), freq = as.matrix(bigram))
rownames(dfBi)<- NULL
rm(bigram)

dfTri <- data.frame(word = names(trigram), freq = as.matrix(trigram))
rownames(dfTri)<- NULL
rm(trigram)

dfQuad1 <- data.frame(word = names(quadgram1), freq = as.matrix(quadgram1))
rownames(dfQuad1)<- NULL
rm(quadgram1)

dfQuad2 <- data.frame(word = names(quadgram2), freq = as.matrix(quadgram2))
rownames(dfQuad2)<- NULL
rm(quadgram2)

dfQuad <- rbind(dfQuad1,dfQuad2)
rm(dfQuad1, dfQuad2)
gc()

dfQuad <- dfQuad %>%
    group_by(word) %>%
    summarise(freq = sum(freq)) %>%
    arrange(desc(freq))

rm(corp)
## Generate data frame
words <- str_split_fixed(dfBi$word," ",2)
colnames(words) <- c("phrase","nextword")
dfBi <- cbind(dfBi,words)
dfBi <- select(dfBi, -word)
rm(words)

words <- str_split_fixed(dfTri$word," ",3)
dfTri$phrase <- paste(words[,1], words[,2])
dfTri$nextword <- words[,3]
dfTri <- select(dfTri, -word)
rm(words)

words <- str_split_fixed(dfQuad$word," ",4)
dfQuad$phrase <- paste(words[,1], words[,2], words[,3])
dfQuad$nextword <- words[,4]
dfQuad <- select(dfQuad, -word)
rm(words)

## helper functions
getLastTokens <- function(phrase, tokens = 3){
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
        print("qPred")
        qPred <- getPredictionFromNgram(tokens,dfQuad)
        print(head(qPred))
    }
    tokens <- getLastTokens(phrase,2)
    if(length(tokens) == 2){
        print("tPred")
        tPred <- getPredictionFromNgram(tokens,dfTri)
        print(head(tPred))
    }
    tokens <- getLastTokens(phrase,1)
    if(length(tokens) == 1){
        print("bPred")
        bPred <- getPredictionFromNgram(tokens, dfBi)
        print(head(bPred))
    }
    pred <- data.frame(nextword = character(), fscore = numeric())
    if(exists("qPred") && nrow(qPred) > 0){
        print("case: insert Quadgrams")
        print("Quadgrams are")
        print(head(qPred,10))
        qPred$fscore <- qPred$prob
        if(nrow(qPred) >=3){
            pred <- qPred[1:3,c("nextword", "fscore")]
            print("got predictions with trigrams returning")
            droplevels(pred)
            return(pred$nextword)
        }else{
            pred <- qPred
        }
    }
    if(exists("tPred") && nrow(tPred) > 0){
        print("case: insert trigrams")
        print("trigrams are")
        print(head(tPred))
        tPred$fscore <- 0.4 * tPred$prob
        tPred <- filter(tPred, !nextword %in% pred$nextword)
        pred <- rbind(pred, tPred)
        pred <- arrange(pred, desc(fscore))
        if(nrow(pred) >=3){
            pred <- pred[1:3,c("nextword", "fscore")]
            print("got predictions with trigrams returning")
            droplevels(pred)
            return(pred$nextword)
        }
    }
    if(exists("bPred")&& nrow(bPred) > 0){
        print("case: insert bigrams")
        print("bigrams are")
        print(head(bPred))
        bPred$fscore <- 0.4 * 0.4 * bPred$prob
        bPred <- filter(bPred, !nextword %in% pred$nextword)
        pred <- rbind(pred, bPred)
        pred <- arrange(pred, desc(fscore))
        if(nrow(pred) >=3){
            pred <- pred[1:3,c("nextword", "fscore")]
            print("got predictions with bigrams returning")
            droplevels(pred)
            return(pred$nextword)
        }
    }
    if(exists("pred") && nrow(pred) > 0){
        print("case: insert unigrams")
        print("unigrams are")
        head(dfUni)
        predV <- as.vector(pred$nextword)
        predU <- as.vector(head(dfUni$word,3))
        print("got unigrams returning")
        droplevels(predV)
        droplevels(predU)
        return(c(predV,predU)[1:3])
    }
    print("no match found suggesting unigrams only")
    droplevels(dfUni)
    dfUni$word[1:3]
}
