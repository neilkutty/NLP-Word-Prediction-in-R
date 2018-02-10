# Next Word Prediction 
# author: Neil Kutty
# 
##

library(shiny)
library(shinythemes)
library(shinycssloaders)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("superhero"),
  
  titlePanel( title = (h3("Next Word Prediction", align = "center")),
              windowTitle = "Next Word Prediction"),
 

  br(),
  h4('This shiny app predicts the most likely next word a user wants to type based on what they have already typed.'),
  #h5('...App is ready when words appear below text input box...'),        
  sidebarLayout( 
      sidebarPanel(textInput("inputText",
                  "Enter Text here and Predicted Word will appear below:",
                  value="")),
        
        
        
        mainPanel(
            br(),
            h4(
                #withSpinner(
                textOutput("showText")
                #,type = getOption("spinner.type", default = 4),
                #size = getOption("spinner.size", default = 0.8)
                #)
               ),
            width=9
            )
),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
hr(),
h6('author: Neil Kutty'),
h6(a("Github", href = "https://github.com/sampsonsimpson/NLP-Word-Prediction-in-R/tree/master/NextWordPrediction")) )
    )
  

