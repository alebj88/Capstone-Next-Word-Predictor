library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #Render table
    output$prediction <- renderTable({
        #Input
        phrase <- as.character(input$phrase)
        nro <- input$nro
        
        #Check
        if(phrase == ""){
            return(NULL)    
        }
        check <- strsplit(phrase,split="")[[1]]
        if(check[length(check)] != " "){
            return(NULL)
        }

        #Predictor
        df <- predictor(phrase,nro = nro)

        #Output
        if(class(df) == "function"){
            return(NULL)    
        }
        if(is.null(df)){
            return(NULL)    
        }
        if(nrow(df) == 0){
            return(NULL)
        }else{
            names(df) <- c("Next Word","Probability")
            return(df)
        }
    })
    
    #Render table
    output$word <- renderText({
        #Input
        phrase <- as.character(input$phrase)

        #Check
        if(phrase == ""){
            return(" ")    
        }
        phrase <- strsplit(phrase,split=" ")[[1]]
        phrase <- phrase[phrase != ""]
        
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
        
        #Predictor
        phrase <- paste0(phrase, collapse = " ")
        word <- stri_extract_last_words(phrase)
        word <- grep(paste0("^",word),wordBag$wordBag,value = TRUE)[1]
      
        if(is.na(word)){
            return(" ")
        }
        return(word)
    })
    
})
