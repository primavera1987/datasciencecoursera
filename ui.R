
library(shiny)

shinyUI(fluidPage(

    tags$head(
        tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h4, li a {
                        font-family: 'Optima', 'sans-serif';
                        font-weight: 500;
                        line-height: 1.3;
                        color:#428bca;
                        }
                        h1 {
                        font-family: 'Apple Chancery', 'cursive';
                        font-weight: 900; line-height: 1.1; 
                        color: #4d3a7d;
                        }
                        
                        body {
                        background-color: #fff;
                        }
                        
                        "))
        ),
    
    pageWithSidebar(
    headerPanel("Capstone Project - Next Word Prediction"),
    sidebarPanel(
        h3("User Input"),
        br(),
        
        strong(""),
        textInput("text1", "Enter a word/phrase here:", value = "Data Science is"),
        br(),
        
        sliderInput("words1","Number of predicted words to return:",
                    min = 1,
                    max = 5,
                    value = 3),
        br(),
        strong("Click here:"),
        actionButton("button1", "Predict Next Words")
    ),
    mainPanel(
        tabsetPanel(
            
            tabPanel("Word Prediction",
                     
                     h4('Input Word/Phrase:'),
                     verbatimTextOutput("userinput"),
                     
                     h4('Next Word Predictions:'),
                     tableOutput("predictedwords")
                     
            ),

            tabPanel("About",
                     
                     h4("Project"),
                     p("Data Science Capstone: Next Word Prediction Algorithm" ),
                     p("The goal of this exercise is to create a product to highlight the 
                        prediction algorithm that you have built and to provide an interface 
                        that can be accessed by others."),
                     p("For this project you must submit:"),
                     p("1. A Shiny app that takes as input a phrase (multiple words) in a text 
                        box input and outputs a prediction of the next word."),
                     p("2. A slide deck consisting of no more than 5 slides created with R 
                        Studio Presenter 
                       (https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations) 
                       pitching your algorithm and app as if you were presenting to your boss or an investor."),
                     
                     h4("Programmer"),
                     p("Madhavi Reddy"),
                     
                     h4("Data"),
                     p("Data for this project is available at:"),
                     p("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),
                     
                     h4("Source Code"),
                     p("All the source code for this project is available at:"),
                     a(href = "https://github.com/madhavireddysm/Capstone","My Github"),
                     br()
                     )
                     ))
                     ))
)