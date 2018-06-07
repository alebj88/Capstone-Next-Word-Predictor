#///////////////////////////////////////////////////////////////////////////////
#//////////////////////////////RUN THIS JUST ONCE///////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

library(tm)
library(stringi)
library(stringr)


setwd("~/Ale/Coursera-SwiftKey/final/en_US")
set.seed(20934)

#Text A
textA <- readLines("en_US.blogs.txt", encoding="UTF-8") 			 			 	
#textA <- textA[sample(1:length(textA),length(textA) %/% 2,replace=F)]	
#textA <- textA[sample(1:length(textA),500,replace=F)]	

#Text B
textB <- readLines("en_US.news.txt", encoding="UTF-8") 		
#textB <- textB[sample(1:length(textB),length(textB) %/% 2,replace=F)]		 	
#textB <- textB[sample(1:length(textB),500,replace=F)]		 	

#Text C
textC <- readLines("en_US.twitter.txt", encoding="UTF-8") 	
#textC <- textC[sample(1:length(textC),length(textC) %/% 2,replace=F)]	 			 	
#textC <- textC[sample(1:length(textC),500,replace=F)]	 			 	

texta1 <- textA[1:500000]
texta2 <- textA[500001:length(textA)]

textc1 <- textC[1:500000]
textc2 <- textC[500001:1000000]
textc3 <- textC[1000001:1500000]
textc4 <- textC[1500001:2000000]
textc5 <- textC[2000001:length(textC)]

listTexts <- list(texta1,texta2,textB,textc1,textc2,textc3,textc4,textc5)

#Cleaning memory.
rm(textA);rm(textB);rm(textC)
rm(texta1);rm(texta2)
rm(textc1);rm(textc2);rm(textc3);rm(textc4);rm(textc5)
gc()

##Metodo clasico (corpus estandar).
setwd("~/Ale/Coursera-SwiftKey")

url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download.file(url=url, destfile="badwords.txt")
badWords <- readLines("badwords.txt")

#Cleaning and processing the texts
for (k in 1:8){
    #Getting corpus
    corpus <- listTexts[[k]] 
    
    #Cleaning corpus
    corpus <- unname(sapply(corpus, function(text){
        text <- strsplit(text,split = " ")[[1]]
        text <- tolower(text)
        text <- gsub(pattern="'|’", replace = "M",text)
        text <- gsub(pattern="\\W", replace = " ",text)
        text <- gsub(pattern="\\d", replace = " ",text)
        text <- gsub(pattern="_", replace = " ",text)
        text <- gsub(pattern="M", replace = "'",text)
        text <- as.character(na.omit(iconv(text,to="ASCII")))
        text <- paste0(text, collapse = " ")
        return(text) 
    })) 	
    corpus <- unname(sapply(corpus, function(text){
        text <- removeWords(text,badWords)
        return(text)
    }))
    corpus <- unname(sapply(corpus, function(text){
        text <- stripWhitespace(text)
        return(text)
    })) 
    
    #Exporting corpus.
    writeLines(corpus,paste0("corpus",k,".txt"))
    print(k)
    
    #Liberating memory
    listTexts[[k]] <- 0
}

#Cleaning workspace
rm(list=ls())
gc()
.rs.restartR()

#Creamos la bolsa de palabras---------------------
#/////////////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)

setwd("~/Ale/Coursera-SwiftKey")

#Reading the texts per part. Avoiding problems of memory.
texta1 <- readLines("corpus1.txt")
texta2 <- readLines("corpus2.txt")
textB  <- readLines("corpus3.txt")
textc1 <- readLines("corpus4.txt")
textc2 <- readLines("corpus5.txt")
textc3 <- readLines("corpus6.txt")
textc4 <- readLines("corpus7.txt")
textc5 <- readLines("corpus8.txt")

corpus <- c(texta1,texta2,textB,textc1,textc2,textc3,textc4,textc5)

#Cleaning memory.
rm(textB);rm(textc1);rm(textc2);rm(textc3);rm(textc4);rm(textc5)
rm(texta1);rm(texta2)
gc()

#Bolsa de palabras unicas
wordBag <- c()

for(i in 1:333){
    print(i)
    wBg <- unique(unlist(sapply(corpus[1:10000],function(text){
        bag <- strsplit(text,split = " ")   
        return(bag)
    })))
    wordBag <- c(wordBag,wBg)
    wordBag <- unique(wordBag)
    
    #Resize corpus
    corpus <- corpus[-(1:10000)]
}

wBg <- unique(unlist(sapply(corpus[1:length(corpus)],function(text){
    bag <- strsplit(text,split = " ")   
    return(bag)
})))

wordBag <- c(wordBag,wBg)
wordBag <- unique(wordBag)

#Cleaning wordBag

#Words with extra quotes
pos <- grep(pattern="^'|'$",wordBag)

correct <- unname(sapply(wordBag[pos],function(text){
    text <- gsub("'"," ",text)
    text <- str_trim(text)
    text <- gsub(" ","'",text)
    return(text)
}))
wordBag[pos] <- correct

pos <- grep(pattern="'(.*)'",wordBag)
wordBag <- wordBag[-pos]

#Empty words
pos <- which(wordBag == "")
wordBag <- wordBag[-pos]

#Words with repeated letters
pos <- grep(pattern="qqq|www|eee|rrr|ttt|yyy|uuu|iii|ooo|ppp",wordBag)
wordBag <- wordBag[-pos]

pos <- grep(pattern="aaa|sss|ddd|fff|ggg|hhh|jjj|kkk|lll",wordBag)
wordBag <- wordBag[-pos]

pos <- grep(pattern="zzz|xxx|ccc|vvv|bbb|nnn|mmm",wordBag)
wordBag <- wordBag[-pos]

#Words with laughing
pos <- grep(pattern="haha|jaja|fsfs|kgkg|hghg|brbr|blah|wow|hehe|xoxo|lolo|lala"
            ,wordBag)
wordBag <- wordBag[-pos]

#abcdario
pos <- grep(pattern="^abcd",wordBag)
wordBag <- wordBag[-pos]

#Sorting
wordBag <- sort(wordBag)

#Unique
wordBag <- unique(wordBag)

write.csv(wordBag,file = "wordBag.csv",row.names= FALSE)

#Limpiamos memoria
rm(list = ls())
gc()
.rs.restartR()

#Agregamos frecuencias para filtrado--------------
#/////////////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)

setwd("~/Ale/Coursera-SwiftKey")

#Reading the texts per part. Avoiding problems of memory.
texta1 <- readLines("corpus1.txt")
texta2 <- readLines("corpus2.txt")
textB  <- readLines("corpus3.txt")
textc1 <- readLines("corpus4.txt")
textc2 <- readLines("corpus5.txt")
textc3 <- readLines("corpus6.txt")
textc4 <- readLines("corpus7.txt")
textc5 <- readLines("corpus8.txt")

corpus <- c(texta1,texta2,textB,textc1,textc2,textc3,textc4,textc5)

#Cleaning memory.
rm(textB);rm(textc1);rm(textc2);rm(textc3);rm(textc4);rm(textc5)
rm(texta1);rm(texta2)
gc()


#Agregamos frecuencias
wordBag <- read.csv("wordBag.csv", sep="")
wordBag <- as.data.frame(table(wordBag))
wordBag$Freq <- rep(0,nrow(wordBag))

for(i in 1:333){
    print(i)
    wBg <- unlist(sapply(corpus[1:10000],function(text){
        bag <- strsplit(text,split = " ")   
        return(bag)
    }))
    wBg <- as.data.frame(table(wBg))
    
    wordBag$Freq[wordBag$wordBag %in% wBg$wBg] <- 
        wordBag$Freq[wordBag$wordBag %in% wBg$wBg] + 
        wBg$Freq[wBg$wBg %in% wordBag$wordBag]
    
    #Resize corpus
    corpus <- corpus[-(1:10000)]
}

wBg <- unlist(sapply(corpus[1:length(corpus)],function(text){
    bag <- strsplit(text,split = " ")   
    return(bag)
}))
wBg <- as.data.frame(table(wBg))

wordBag$Freq[wordBag$wordBag %in% wBg$wBg] <- 
    wordBag$Freq[wordBag$wordBag %in% wBg$wBg] + 
    wBg$Freq[wBg$wBg %in% wordBag$wordBag]

#Resize corpus
corpus <- corpus[-(1:length(corpus))]


#Remove word with low frecuency
trashBag <- wordBag[wordBag$Freq < 10,]
wordBag  <- wordBag[wordBag$Freq >= 10,]

write.csv(wordBag,file = "wordBag.csv",row.names= FALSE)
write.csv(trashBag,file = "trashBag.csv",row.names= FALSE)

#Limpiamos memoria
rm(list = ls())
gc()
.rs.restartR()

#Cleaning Corpus----------------------------
#///////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)

setwd("~/Ale/Coursera-SwiftKey")

#Reading the texts per part. Avoiding problems of memory.
texta1 <- readLines("corpus1.txt")
texta2 <- readLines("corpus2.txt")
textB  <- readLines("corpus3.txt")
textc1 <- readLines("corpus4.txt")
textc2 <- readLines("corpus5.txt")
textc3 <- readLines("corpus6.txt")
textc4 <- readLines("corpus7.txt")
textc5 <- readLines("corpus8.txt")

corpus <- list(texta1,texta2,textB,textc1,textc2,textc3,textc4,textc5)

#Cleaning memory.
rm(textB);rm(textc1);rm(textc2);rm(textc3);rm(textc4);rm(textc5)
rm(texta1);rm(texta2)
gc()

for(i in 1:length(corpus)){
    
    #Cleaning corpus again
    corpus[[i]] <- unname(sapply(corpus[[i]], function(text){
        text <- strsplit(text,split=" ")[[1]]
        
        #Words with extra quotes
        pos <- grep(pattern="^'|'$",text)
        
        if(length(pos) != 0){
            correct <- unname(sapply(text[pos],function(txt){
                txt <- gsub("'"," ",txt)
                txt <- str_trim(txt)
                txt <- gsub(" ","'",txt)
                return(txt)
            }))
            text[pos] <- correct
        }
        pos <- grep(pattern="'(.*)'",text)
        
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        #Empty words
        pos <- which(text == "")
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        #Words with repeated letters
        pos <- grep(pattern="qqq|www|eee|rrr|ttt|yyy|uuu|iii|ooo|ppp",text)
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        pos <- grep(pattern="aaa|sss|ddd|fff|ggg|hhh|jjj|kkk|lll",text)
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        pos <- grep(pattern="zzz|xxx|ccc|vvv|bbb|nnn|mmm",text)
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        #Words with laughing
        pos <- grep(pattern=
            "haha|jaja|fsfs|kgkg|hghg|brbr|blah|wow|hehe|xoxo|lolo|lala",text)
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        #abcdario
        pos <- grep(pattern="^abcd",text)
        if(length(pos) != 0){
            text <- text[-pos]
        }
        
        #Salida
        text <- paste0(text,collapse = " ")
        return(text)
    })) 
    
    corpus[[i]] <- unname(sapply(corpus[[i]], function(text){
        text <- stripWhitespace(text)
        return(text)
    })) 
    
    print(i)
}

corpus <- unlist(corpus)

writeLines(corpus,"finalCorpus.txt")

#Limpiamos memoria
rm(list = ls())
gc()
.rs.restartR()

#Removing trash words and creating sample---
#///////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)

set.seed(4321)
setwd("~/Ale/Coursera-SwiftKey")

corpus <- readLines("finalCorpus.txt")
trashBag <- read.csv("trashBag.csv")

#Subset of the corpus
corpus <- corpus[sample(1:length(corpus),100000,replace = FALSE)]


corpus <- unname(sapply(corpus,function(text){
    text <- strsplit(text,split=" ")[[1]]
    
    n <- sum(text %in% trashBag$wordBag)
    text[text %in% trashBag$wordBag] <- rep(" ",n)
    
    #Salida
    text <- paste0(text,collapse = " ")
    return(text)
})) 

corpus <- unname(sapply(corpus, function(text){
    text <- stripWhitespace(text)
    return(text)
})) 

writeLines(corpus,"finalCorpusSample.txt")

#///////////////////////////////////////////////////////////////////////////////
#//////////////////////////RUN THIS JUST ONCE///////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

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

#Subset of the corpus
corpus <- corpus[sample(1:length(corpus),80000,replace = FALSE)]

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
.rs.restartR()

#Creamos los ngrams sin stopWords------------------
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

#Subset of the corpus
corpus <- corpus[sample(1:length(corpus),80000,replace = FALSE)]

corpus <- unname(sapply(corpus,function(text){
    text <- removeWords(text,stopwords("english"))
    return(text)
}))

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

write.csv(df,"6-gramSW.csv",row.names = F)

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

write.csv(df,"5-gramSW.csv",row.names = F)

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

write.csv(df,"4-gramSW.csv",row.names = F)

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

write.csv(df,"3-gramSW.csv",row.names = F)

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

write.csv(df,"2-gramSW.csv",row.names = F)

#Limpiamos memoria
rm(list = ls())
gc()
.rs.restartR()

#///////////////////////////////////////////////////////////////////////////////
#-----------------------------Prediction function-------------------------------
#///////////////////////////////////////////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)
library(tidytext)

setwd("~/Ale/Coursera-SwiftKey")

n2gram <- read.csv("2-gram.csv")
n2gram$term <- as.character(n2gram$term)
n3gram <- read.csv("3-gram.csv")
n3gram$term <- as.character(n3gram$term)
n4gram <- read.csv("4-gram.csv")
n4gram$term <- as.character(n4gram$term)
n5gram <- read.csv("5-gram.csv")
n5gram$term <- as.character(n5gram$term)
n6gram <- read.csv("6-gram.csv")
n6gram$term <- as.character(n6gram$term)
wordBag <- read.csv("wordBag.csv")
wordBag <- wordBag[order(wordBag$Freq,decreasing = T),] 
wordBag$wordBag <- as.character(wordBag$wordBag)

n2gramSW <- read.csv("2-gramSW.csv")
n2gramSW$term <- as.character(n2gramSW$term)
n3gramSW <- read.csv("3-gramSW.csv")
n3gramSW$term <- as.character(n3gramSW$term)
n4gramSW <- read.csv("4-gramSW.csv")
n4gramSW$term <- as.character(n4gramSW$term)
n5gramSW <- read.csv("5-gramSW.csv")
n5gramSW$term <- as.character(n5gramSW$term)
n6gramSW <- read.csv("6-gramSW.csv")
n6gramSW$term <- as.character(n6gramSW$term)

n2gramSW <- NULL
n3gramSW <- NULL
n4gramSW <- NULL
n5gramSW <- NULL
n6gramSW <- NULL


#Funcion de prediccion------------------------------
predictor <- function(phrase, stopWords = TRUE, nro = 5){
    
    phrase <- strsplit(phrase,split=" ")[[1]]
    phrase <- phrase[phrase != ""]
    
    #Check input
    if(length(phrase) == 0){
        return(NULL)    
    }
    #Check
    if(all(!(phrase %in% wordBag$wordBag)) == TRUE){
        return(NULL)    
    }
    
    #if(nro > 20){
    #    nro <- 20    
    #}
    
    #Cleaning input.
    phrase <- unname(sapply(phrase, function(text){
        text <- tolower(text)
        text <- gsub(pattern="'|’", replace = "M",text)
        text <- gsub(pattern="\\W", replace = "",text)
        text <- gsub(pattern="\\d", replace = "",text)
        text <- gsub(pattern="_", replace = " ",text)
        text <- gsub(pattern="M", replace = "'",text)
        text <- as.character(na.omit(iconv(text,to="ASCII")))
        text <- paste0(text, collapse = " ")
        return(text) 
    }))	
    
    phrase <- unname(sapply(phrase, function(text){
        text <- tm :: stripWhitespace(text)
        return(text)
    }))
    
    #Useing principal words
    if(stopWords == FALSE){
        phrase <- removeWords(phrase,stopwords("english"))
        phrase <- phrase[phrase != ""]
    }
    
    #Filtering
    n <- sum(!(phrase %in% wordBag$wordBag))
    if(n != 0){
        phrase <- phrase[(phrase %in% wordBag$wordBag)] 
    }
    
    #Matching Ngram
    if(length(phrase) > 5){
        phrase <- phrase[-(1:(length(phrase)-5))]
        i <- 6
    }else{
        i <- length(phrase) + 1
    }
    phrase <- paste0(phrase,collapse = " ")
    
    
    #While cicle------------------------------
    lastWord <- c()
    probs <- c()
    
    while(i >= 2){
        #Remove stops words?
        if(stopWords == FALSE){
            if(i == 6){
                #Processing the phrase
                auxdf <- n6gramSW[grepl(paste0("^",phrase),n6gramSW$term),]
                
            }else if(i == 5){
                #Processing the phrase
                auxdf <- n5gramSW[grepl(paste0("^",phrase),n5gramSW$term),]
                
            }else if(i == 4){
                #Processing the phrase
                auxdf <- n4gramSW[grepl(paste0("^",phrase),n4gramSW$term),]
                
            }else if(i == 3){
                #Processing the phrase
                auxdf <- n3gramSW[grepl(paste0("^",phrase),n3gramSW$term),]
                
            }else if(i == 2){
                #Processing the phrase
                auxdf <- n2gramSW[grepl(paste0("^",phrase),n2gramSW$term),]
            }
        }else{
            if(i == 6){
                #Processing the phrase
                auxdf <- n6gram[grepl(paste0("^",phrase),n6gram$term),]
                
            }else if(i == 5){
                #Processing the phrase
                auxdf <- n5gram[grepl(paste0("^",phrase),n5gram$term),]
                
            }else if(i == 4){
                #Processing the phrase
                auxdf <- n4gram[grepl(paste0("^",phrase),n4gram$term),]
                
            }else if(i == 3){
                #Processing the phrase
                auxdf <- n3gram[grepl(paste0("^",phrase),n3gram$term),]
                
            }else if(i == 2){
                #Processing the phrase
                auxdf <- n2gram[grepl(paste0("^",phrase),n2gram$term),]
            }
        }
        
        if(!is.null(auxdf)){
            
            #Check2
            if(nrow(auxdf) != 0){
                
                auxdf <- as.data.frame(tapply(auxdf$count,auxdf$term,sum))
                names(auxdf) <- "freq"
                auxdf$term <- rownames(auxdf)
                auxdf <- auxdf[order(auxdf$freq,decreasing = T),] 
                
                
                #Max number of words
                maxFreqs <- as.data.frame(table(auxdf$freq))
                maxFreqs <- maxFreqs[order(maxFreqs$Var1,decreasing = T),]
                maxFreqs$Acum <- cumsum(maxFreqs$Freq) 
                if(max(maxFreqs$Acum) >= 5){
                    pos <- which(maxFreqs$Acum >= 5)[1]
                }else{
                    pos <- which(maxFreqs$Acum >= max(maxFreqs$Acum))[1]
                }
                
                maxFreqs <- as.numeric(as.character(maxFreqs$Var1[1:pos])) 
                
                
                #Searching words and vals
                vals <- c()
                group <- c()
                for(k in 1:length(maxFreqs)){
                    invisible(sapply(auxdf$term[auxdf$freq %in% maxFreqs[k]]
                                     ,function(text){
                                         word <- stri_extract_last_words(text)
                                         if(word %in% lastWord == FALSE){
                                             vals <<- c(vals,
                                                        wordBag$Freq[wordBag$wordBag ==word])
                                             lastWord <<- c(lastWord,word)
                                             group <<- c(group,maxFreqs[k])
                                         }
                                         return(NULL)
                                     })) 
                    #Exit
                    if(length(lastWord) >= 5){
                        break
                    }
                }
                
                #Creating the ordered df
                df <- data.frame(lastWord =lastWord,vals =vals,group =group)
                df <- df[order(df$group,df$vals,decreasing = T),]
                if(nro != 0){
                    if(nrow(df) > nro){
                        df <- df[1:nro,]
                    }
                    prod <- c(1,0.5,0.2,0.05,0.01,rep(0.01,25))
                }else{
                    prod <- rep(1,nrow(df))
                }
                prod <- prod[1:length(unique(df$group))]
                df$prod <- rep(NA,nrow(df))
                
                
                for(j in 1:length(prod)){
                    m <- sum(df$group == unique(df$group)[j])
                    df$prod[df$group==unique(df$group)[j]] <- rep(prod[j],m)
                }

                
                #Calculating frequencies
                df$vals <- df$vals * df$prod 
                probs <- paste0(round(df$vals/sum(df$vals)*100,1),"%")
                lastWord <- as.character(df$lastWord)
                #Finish the loop
                break  
                
            }else{
                phrase <- strsplit(phrase,split=" ")[[1]]
                phrase <- phrase[-1]
                phrase <- paste0(phrase,collapse = " ")
                
                i <- i - 1    
            }
        }else{
            phrase <- strsplit(phrase,split=" ")[[1]]
            phrase <- phrase[-1]
            phrase <- paste0(phrase,collapse = " ")
            
            i <- i - 1    
        }
    }
    df <- data.frame(lastWord = lastWord,probs = probs)

    return(df)
}

# phrase <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
# phrase <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
# phrase <- "I'd give anything to see arctic monkeys this"
# phrase <- "Talking to your mom has the same effect as a hug and helps reduce your"
# phrase <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
# phrase <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
#     
#     
# df <- predictor(phrase,nro = 0)
# df
# 
# df1 <- predictor(phrase,stopWords = FALSE,nro = 0)
# df1
