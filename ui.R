library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  
    #Application title
    titlePanel(strong(em("Next Word Predictor"))),

    navbarPage(title = " ",theme = shinytheme("cerulean"),
        tabPanel("Prediction",
            sidebarLayout(
                sidebarPanel(
                    h3(strong("User Panel"))
                    ,
                    br()
                    ,
                    textInput("phrase","Enter the phrase","")
                    ,
                    numericInput("nro",
                                 "Number of predicted words to be displayed", 
                                 value = 5,min = 1, max = 20, step = 5)
                ),
                mainPanel(
                    h3(strong("Next Word Prediction"))
                    ,
                    h4(span(tableOutput("prediction"),style="color:black"))
                    ,
                    h3(strong("Suggested Word"))
                    ,
                    h3(span(textOutput("word"),style="color:green"))
                )
            )
        ),
        tabPanel("Readme",
            h3(strong("Next Word Predictor APP"))     
            ,
            br()
            ,
            h4("Write a sentence, press 'space' and the APP will predict the next word for you.")    
            ,
            h4("You can choose between 1 and 20 words to be predicted in the User Panel")
            ,
            br()
            ,
            h4("The APP also predicts as you type, finishing the word you are writing.")
            ,
            h4("That word will be show as 'suggested word'.") 
            ,
            br()
            ,
            h5(span("For computational reasons, only a small proportion of data was considered in the building of this application.",style="color:red"))
            ,
            h5(span("This implies that it has a large margin of error.",style="color:red"))
        )
    )
))
