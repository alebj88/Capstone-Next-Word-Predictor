#///////////////////////////////////////////////////////////////////////////////
#-----------------------------Prediction function-------------------------------
#///////////////////////////////////////////////////////////////////////////////
#Deployment completed: 
#https://alebj88.shinyapps.io/NextWordShiny/

library(tm)
library(stringi)
library(stringr)
library(tidytext)

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
rownames(wordBag) <- 1:nrow(wordBag)

n2gramSW <- NULL
n3gramSW <- NULL
n4gramSW <- NULL
n5gramSW <- NULL
n6gramSW <- NULL

#Funcion de prediccion------------------------------
predictor <- function(phrase, stopWords = TRUE, nro = 5){
    
    withProgress(message = 'Making the prediction', value = 0, {
      
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
        
        #==========
        incProgress(1/10)
        #==========
        
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
        
        #==========
        incProgress(1/10)
        #==========
        
        phrase <- unname(sapply(phrase, function(text){
            text <- tm :: stripWhitespace(text)
            return(text)
        }))
        
        #==========
        incProgress(1/10)
        #==========
        
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
        
        #==========
        incProgress(1/10)
        #==========
        
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
                    
                    #==========
                    incProgress(1/10)
                    #==========
                    
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
                    
                    #==========
                    incProgress(1/10)
                    #==========
                    
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
                    
                    #==========
                    incProgress(1/10)
                    #==========
                    
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
                    
                    #==========
                    incProgress(1/10)
                    #==========
                    
                    for(j in 1:length(prod)){
                        m <- sum(df$group == unique(df$group)[j])
                        df$prod[df$group==unique(df$group)[j]] <- rep(prod[j],m)
                    }
                    
                    #==========
                    incProgress(1/10)
                    #==========
                    
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
    
        #==========
        incProgress(1/10)
        #==========
    })
    return(df)
}
