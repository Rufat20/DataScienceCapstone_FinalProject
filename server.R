
library(shiny)
library(wordcloud)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ## Initialization
    currentTokens <- ""
    wordPrediction01 <- ""
    wordPrediction02 <- ""
    wordPrediction03 <- ""
    wordPrediction04 <- ""
    wordPrediction05 <- ""
    
    ngramInput <- reactive({
        txt <- input$userTxt
        
        # Update currentTokens only if last character is a space
        if(nchar(txt)==0 || substr(txt, nchar(txt), nchar(txt)) == " "){
            currentTokens <- txt
        }
        
        currentTokens
    })
    
    wordPredictionDataTable <- reactive({
        Next_word(ngramInput())
    })
    
    output$wordPrediction01 <- renderText({
        wordPredictionDataTable()[1,term]
    })

    output$wordPrediction02 <- renderText({
        wordPredictionDataTable()[2,term]
    })
    
    output$wordPrediction03 <- renderText({
        wordPredictionDataTable()[3,term]
    })
    
    output$wordPrediction04 <- renderText({
        wordPredictionDataTable()[4,term]
    })
    
    output$wordPrediction05 <- renderText({
        wordPredictionDataTable()[5,term]
    })
    
    makecloud <- reactive({
        
        png("wordcloud.png", width=10, height=8, units="in", res=350)
        w<-wordcloud(words=wordPredictionDataTable()$term,
                     freq=wordPredictionDataTable()$Prob,
                     scale = c(5,1),
                     max.words = 50,
                     colors=brewer.pal(8, "Accent"), 
                     random.order = FALSE)
        dev.off()
        
        filename<-"wordcloud.png"
    }) 
    output$wordcloud <- renderImage({
       list(src=makecloud(), alt = "Image being generated!", height = 600)
    },
    deleteFile = FALSE)
})