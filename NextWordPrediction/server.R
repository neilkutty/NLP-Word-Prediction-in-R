# 
# Next Word Prediction 
# author: Neil Kutty
#
#
#
#  01/31/2018 - expand predictNext function to include five-gram search
#             - add a clean_fx that doesn't return a corpus 
#
#
#
#

library(shiny)

source('predictNext.R')
source('functions.R')


shinyServer(function(input, output) {
    
    predict <- reactive({
        
        text = cleanInput_fx(input$inputText)
        #text_len = length(input$inputText)
        text_len = length(unlist(strsplit(as.character(input$inputText)," ")))
        predictNext(text, text_len)
    })
    
    output$showText <- renderText({
        withProgress(message = "Predicting next word...", value = 0, {
            predict()
            })
        
    })
  
})
