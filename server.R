library(shiny)
library(tm)
library(stylo)
library(data.table)
library(RWeka)
library(stringr)
library(dplyr)

shinyServer(
    function(input,output) {
        
        # User Input
        txtReturn <- eventReactive(input$button1, {
            paste(input$text1)
        })
        output$userinput <- renderText({ txtReturn() })
        
        # Predicted next words
        nextWord <- eventReactive(input$button1, {
            NextWordPrediction(input$text1, input$words1)
        })
        output$predictedwords <- renderTable({ nextWord() })
 
    }
    
)