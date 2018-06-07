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
    corpus <- sapply(corpus, function(text){
        text <- gsub(pattern="\\W", replace = " ",text)
        text <- gsub(pattern="\\d", replace = " ",text)
        text <- tolower(text)
        return(text) 
    }) 	
    corpus <- sapply(corpus, function(text){
        text <- removeWords(text,badWords)
        return(text)
    })
    corpus <- sapply(corpus, function(text){
        text <- stripWhitespace(text)
        return(text)
    }) 
    #Exporting corpus.
    writeLines(corpus,paste0("corpus",k,".txt"))
    
    #Liberating memory
    listTexts[[k]] <- 0
}

#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
library(tm)
library(stringi)
library(stringr)
library(tidytext)

setwd("~/Ale/Coursera-SwiftKey")

#Reading the texts per part. Avoiding problems of memory.
textA <- readLines("corpus1.txt")
textB <- readLines("corpus2.txt")
textc1 <- readLines("corpus3.txt")
textc2 <- readLines("corpus4.txt")
textc3 <- readLines("corpus5.txt")
textc4 <- readLines("corpus6.txt")
textc5 <- readLines("corpus7.txt")

corpus <- c(textA,textB,textc1,textc2,textc3,textc4,textc5)

#Cleaning memory.
rm(textA);rm(textB);rm(textc1);rm(textc2);rm(textc3);rm(textc4);rm(textc5)
gc()

#Function to get the next word
nextWord <- function(pat){
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
    
    return(df)
}

#Question 1
df <- nextWord("(a+ +case+ +of+ +[^ ]+)")
head(df)

#Question 2
df <- nextWord("(it+ +would+ +mean+ +the+ +[^ ]+)")
head(df)

#Question 3
df <- nextWord("(make+ +me+ +the+ +[^ ]+)")
head(df)

#Question 4
df <- nextWord("(struggling+ +but+ +the+ +[^ ]+)")  #Nothing
head(df)

pat <- "(struggling+ +([^ ]+ +){1,5}crowd+)"    #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(struggling+ +([^ ]+ +){1,5}referees+)"#Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(struggling+ +([^ ]+ +){1,5}defense+)" #Just one
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(struggling+ +([^ ]+ +){1,5}players+)" #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

#Question 5
df <- nextWord("(date+ +at+ +the+ +[^ ]+)")     #Nothing
df

pat <- "(romantic+ +([^ ]+ +){1,8}grocery)"     #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(romantic+ +([^ ]+ +){1,8}beach+)"      #Tree
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(romantic+ +([^ ]+ +){1,8}movies+)"     #Six Non sense with the phrase
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(romantic+ +([^ ]+ +){1,8}mall+)"       #One
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

#Question 6
df <- nextWord("(be on my+ +[^ ]+)")
head(df)

#Question 7
df <- nextWord("(in quite some+ +[^ ]+)")
head(df)

#Question 8
df <- nextWord("(with his little+ +[^ ]+)")
df

df <- nextWord("(his little+ +[^ ]+)")
head(df,70)

#Question 9
df <- nextWord("(during the+ +[^ ]+)") #So much information
head(df,40)

pat <- "(during the bad )"       #one 50%
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(during the sad )"       #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(during the hard )"      #one 50%
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(during the worse )"     #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

#Question 10
df <- nextWord("(must be+ +[^ ]+)") #So much information
head(df)

pat <- "(must be callous )"       #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(must be insane )"        #eight
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(must be asleep )"        #two
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]

pat <- "(must be insensitive )"   #Nothing
pos <- grepl(pat,corpus)
corp <- corpus[pos]
length(corp)
corp[1]












































