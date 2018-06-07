library(tm)
library(stringi)
library(stringr)
library(tidytext)

setwd("~/Ale/Coursera-SwiftKey")

#Read corpus
corpus <- readLines("finalCorpus.txt")

#Function to get the next word
nextWord <- function(pat,words=c()){
    pos <- grepl(pat,corpus)
    corpus <- corpus[pos]
    
    lastWord <- c()
    invisible(sapply(corpus,function(text){
        lastWord <<- c(lastWord,stri_extract_last_words(str_extract(text,pat)))
        return(NULL)
    }))
    
    lastWord  <- Corpus(VectorSource(as.list(lastWord)))
    tdm <- TermDocumentMatrix(lastWord)
    #tdm <- as.matrix(tdm)
    tdm <- tidy(tdm)
    #freq <- rowSums(tdm)
    freq <- tapply(tdm$count,tdm$term,sum)
    freq <- sort(freq,decreasing=TRUE) 
    df <- data.frame(word = names(freq),freq = freq)
    rownames(df) <- NULL
    
    #Subsetting
    if(length(word) != 0){
       df <- df[df$word %in% words,] 
    }
    return(df)
}

#Question 1
df <- nextWord("(and i would+ +[^ ]+)",words = c("die","give","sleep","eat"))
df

#Question 2
df <- nextWord("(about his+ +[^ ]+)",      #wrong
               words = c("marital","spiritual","financial","horticultural"))
df

#Question 3
df <- nextWord("(to see this+ +[^ ]+)",
               words = c("weekend","morning","decade","month"))
df

#Question 4
df <- nextWord("(reduce your+ +[^ ]+)",
               words = c("sleepiness","happiness","hunger","stress")) 
df

#Question 5
df <- nextWord("(time to take a+ +[^ ]+)",  #look does not seem to be correct
               words = c("minute","walk","look","picture"))    
df

#Question 6
df <- nextWord("(to settle the+ +[^ ]+)",   #Wrong... really?...is it matter?
               words = c("matter","case","account","incident"))
df

#Question 7
df <- nextWord("(bags of groceries in each+ +[^ ]+)",
               words = c("arm","finger","hand","toe"))
df

#Question 8
df <- nextWord("(from the bottom to the+ +[^ ]+)",
               words = c("side","top","middle","center"))
df

#Question 9
df <- nextWord("(from playing+ +[^ ]+)",
               words = c("daily","inside","weekly","outside")) 
df

#Question 10
df <- nextWord("(sandler's+ +[^ ]+)", #Nothing
               words = c("novels","pictures","stories","movies")) 
df

pos <- which(grepl("adam sandler's",corpus))
pos1 <- which(grepl("novels",corpus))
pos2 <- which(grepl("pictures",corpus))
pos3 <- which(grepl("stories",corpus))
pos4 <- which(grepl("movies",corpus))

length(intersect(pos,pos1)) #novels    0
length(intersect(pos,pos2)) #pictures  0
length(intersect(pos,pos3)) #stories   0
length(intersect(pos,pos4)) #movies    2 win!!
