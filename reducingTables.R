#Creamos los ngrams -------------------------------
#//////////////////////////////////////////////////
library(RWeka)
library(tidyr)
library(tm)
library(stringi)
library(stringr)
library(tidytext)

setwd("~/Ale/Coursera-SwiftKey")

#Reading the texts per part. Avoiding problems of memory.
corpus <- readLines("finalCorpusSample.txt")

setwd("~/Ale/NextWordShiny")

#Subset of the corpus
corpus <- corpus[sample(1:length(corpus),10000,replace = FALSE)]

#Trick
corpus <- unname(sapply(corpus,function(text){
    text <- gsub("'","kmkmk",text)
    return(text)
}))

#Model with 6-gram--------------------------------
n <- 6														
ngram <- function(x){
    NGramTokenizer(x, Weka_control(min = n, max = n))
}

#Formal corpus
corpus <- VCorpus(VectorSource(corpus)) 

#Term document matrix
df <- TermDocumentMatrix(corpus,control = list(tokenize = ngram))
df <- tidy(df)
df <- data.frame(term = df$term,count = df$count)
df$term <- unname(sapply(df$term,function(text){
    text <- gsub("kmkmk","'",text)
    return(text)
}))

write.csv(df,"6-gram.csv",row.names = F)

#Model with 5-gram--------------------------------
n <- 5														
ngram <- function(x){
    NGramTokenizer(x, Weka_control(min = n, max = n))
}

#Term document matrix
df <- TermDocumentMatrix(corpus,control = list(tokenize = ngram))
df <- tidy(df)
df <- data.frame(term = df$term,count = df$count)
df$term <- unname(sapply(df$term,function(text){
    text <- gsub("kmkmk","'",text)
    return(text)
}))

write.csv(df,"5-gram.csv",row.names = F)

#Model with 4-gram--------------------------------
n <- 4														
ngram <- function(x){
    NGramTokenizer(x, Weka_control(min = n, max = n))
}

#Term document matrix
df <- TermDocumentMatrix(corpus,control = list(tokenize = ngram))
df <- tidy(df)
df <- data.frame(term = df$term,count = df$count)
df$term <- unname(sapply(df$term,function(text){
    text <- gsub("kmkmk","'",text)
    return(text)
}))

write.csv(df,"4-gram.csv",row.names = F)

#Model with 3-gram--------------------------------
n <- 3														
ngram <- function(x){
    NGramTokenizer(x, Weka_control(min = n, max = n))
}

#Term document matrix
df <- TermDocumentMatrix(corpus,control = list(tokenize = ngram))
df <- tidy(df)
df <- data.frame(term = df$term,count = df$count)
df$term <- unname(sapply(df$term,function(text){
    text <- gsub("kmkmk","'",text)
    return(text)
}))

write.csv(df,"3-gram.csv",row.names = F)

#Model with 2-gram--------------------------------
n <- 2														
ngram <- function(x){
    NGramTokenizer(x, Weka_control(min = n, max = n))
}

#Term document matrix
df <- TermDocumentMatrix(corpus,control = list(tokenize = ngram))
df <- tidy(df)
df <- data.frame(term = df$term,count = df$count)
df$term <- unname(sapply(df$term,function(text){
    text <- gsub("kmkmk","'",text)
    return(text)
}))

write.csv(df,"2-gram.csv",row.names = F)

#Limpiamos memoria
rm(list = ls())
gc()
