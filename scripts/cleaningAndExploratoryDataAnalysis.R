set.seed(12345);
setwd("/home/gaurav/rstudio/Course10 Capstone/");
## Downloading and unzipping dataset
if(!file.exists("./dataset")){
    dir.create("./dataset");
    dir.create("./dataset/raw");
    fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip";
    download.file(fileUrl,destfile="./dataset/raw/Dataset.zip");
    
    # Unzip dataSet to /dataset/raw directory
    unzip(zipfile="./dataset/raw/Dataset.zip",exdir="./dataset/raw");
}

## Sampling Dataset
if(!file.exists("./dataset/sampled")){
    sourceDir <- "./dataset/raw/final";
    destDir <- "./dataset/sampled"
    dir.create(destDir);
    newdirectories <- list.files(sourceDir);
    for (dir in newdirectories){
        newDir <- paste(destDir, dir, sep = "/");
        dir.create(newDir);
        files <- list.files(paste(sourceDir, dir, sep = "/"));
        for(file in files){
            sourceFile <- paste(sourceDir, dir, file, sep = "/");
            destFile <- paste(destDir, dir, file, sep = "/");
            
            fl <- readLines(sourceFile);
            sampleVector <- rbinom(length(fl), 1, 0.05);
            
            sampleFl <- fl[sampleVector == 1];
            
            fileConn<-file(destFile);
            writeLines(sampleFl, fileConn);
            close(fileConn);
            rm(fl,sampleFl)
            gc();
        }
    }
    rm(list = ls())
    gc();
}

## Cleaning Data
#1. Clean punctuations
#2. to lower aplphabets 
#3. remove extra white spaciing
#4. remove numbers
#5. Profanity filtering
if(!file.exists("./dataset/clean/")){
    dir.create("./dataset/clean/en_US", recursive = T)
    library(tm);
    sourceDir <- "./dataset/sampled/en_US"
    sourceFile <- "./dataset/sampled/en_US/en_US.blogs.txt"
    
    docs <- Corpus(DirSource(sourceDir));
    
    docs <- tm_map(docs , content_transformer(tolower));
    
    ## remove puctuations like - and : with space
    ## for 24-year-old sentences 
    toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
    docs <- tm_map(docs, toSpace, "-")
    docs <- tm_map(docs, toSpace, ":")
    
    ## for 20's 90's
    docs <- tm_map(docs, toSpace, "[0-9]+'s")
    
    ## remove numbers
    docs <- tm_map(docs, removeNumbers)
    
    ## profanity filtering
    badwords <- readLines("profanity filtering.txt")
    badwords <- paste(badwords,collapse ="|")
    removeBadWords <- content_transformer(function(x, pattern) {return (gsub(pattern, "*", x))})
    docs <- tm_map(docs, removeBadWords, badwords);
    
    ## remove words like sh*t
    removeStaredBadWords <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
    docs <- tm_map(docs, removeStaredBadWords, "[a-z]+\\*+[a-z]+");
    
    ## remove punctuations without apostrophe 
    removePunctWithoutApostrophe <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
    docs <- tm_map(docs, removePunctWithoutApostrophe, "[^[:alnum:][:space:]']");
    
    ## remove non-english words
    removeNonEnglishWords <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
    docs <- tm_map(docs, removeNonEnglishWords, "[^0-9A-Za-z///' ]");
    
    ## remove extra white spaces
    docs <- tm_map(docs , stripWhitespace)
    ## finished preprocessing
    
    writeCorpus(x = docs,path = "dataset/clean/en_US/",filenames = paste(names(docs), sep = ""))
    rm(list = ls())
    gc();
}


## Exploratory Data Analysis
library(tm)
library(ggplot2)
library(dplyr)
library(wordcloud)
sourceDir <- "./dataset/clean/en_US/"
corp <- Corpus(DirSource(sourceDir))

dtm <- DocumentTermMatrix(corp)
mat <- as.matrix(dtm)
freq <- colSums(mat)
df <- data.frame(word <- names(freq), freq <- freq)
colnames(df) <- c("word", "freq")
rownames(df)<- NULL

rm(dtm)
rm(mat)
rm(freq)
gc()

df <- arrange(df, desc(freq))

## histogram of 10 frequent words
barplot( head(df$freq , 10) , names.arg = head(df$word, 10) )

# density plot of last 50 frequent words
hist(head(df$freq , 50), breaks = 1000, freq = F)

## indexing by frequency
df <- mutate(df, index = 1:length(word))

## frequency percent
df <- mutate(df, percent = (freq/(sum(freq)))*100)

## frequency percent cumulative
df <- mutate(df, cumPercent = ave(percent, FUN = cumsum))

## wordcloud
wordcloud(words = df$word[-(1:5)], freq = df$freq[-(1:5)], min.freq = 10,
          max.words=200, random.order=FALSE, rot.per = 0.20, 
          colors=brewer.pal(8, "Dark2"))

library(ggplot2)
p <- ggplot(df[1:15,], aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

rm(p)
## tokeninsation

BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

dtmBi <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
matBi <- as.matrix(dtmBi)
freqBi <- colSums(matBi)
dfBi <- data.frame(word <- names(freqBi), freq <- freqBi)
colnames(dfBi) <- c("word", "freq")
rownames(dfBi)<- NULL
dfBi <- arrange(dfBi, desc(freq))

rm(dtmBi)
rm(freqBi)
rm(matBi)
gc()

TrigramTokenizer <-function(x){
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

dtmTri <- DocumentTermMatrix(corp, control = list(tokenize = TrigramTokenizer))
matTri <- as.matrix(dtmTri)
freqTri <- colSums(matTri)
dfTri <- data.frame(word <- names(freqTri), freq <- freqTri)
colnames(dfTri) <- c("word", "freq")
rownames(dfTri)<- NULL
dfTri <- arrange(dfTri, desc(freq))

rm(dtmTri)
rm(matTri)
rm(freqTri)
gc()

## 4gram
QuadgramTokenizer <-function(x){
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}

dtmQuad <- DocumentTermMatrix(corp, control = list(tokenize = QuadgramTokenizer))
matQuad <- as.matrix(dtmQuad)
freqQuad <- colSums(matQuad)
dfQuad <- data.frame(word <- names(freqQuad), freq <- freqQuad)
colnames(dfQuad) <- c("word", "freq")
rownames(dfQuad)<- NULL
dfQuad <- arrange(dfQuad, desc(freq))

rm(dtmQuad)
rm(matQuad)
rm(freqQuad)
gc()

## 5gram
PentagramTokenizer <-function(x){
    unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
}

dtmPenta <- DocumentTermMatrix(corp, control = list(tokenize = PentagramTokenizer))
matPenta <- as.matrix(dtmPenta)
rm(dtmPenta)
freqPenta <- colSums(matPenta)
rm(matPenta)
dfPenta <- data.frame(word <- names(freqPenta), freq <- freqPenta)
rm(freqPenta)
colnames(dfPenta) <- c("word", "freq")
rownames(dfPenta)<- NULL
dfPenta <- arrange(dfPenta, desc(freq))
