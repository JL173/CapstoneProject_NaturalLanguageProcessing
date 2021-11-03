#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Prediction"),
    
    # Sidebar for predictive function
    sidebarLayout(
        
        sidebarPanel(
            textInput(inputId = "inputString",
                      label = "Predict my next word",
                      value = ""),
            
            radioButtons(inputId = "modelChoice",
                         label = "Choose Source",
                         choices = c("Twitter",
                                     "News",
                                     "Blogs",
                                     "Song Lyrics",
                                     "Movie Reviews",
                                     "Everything")),
            
            sliderInput(
                "depth", label = "Prediction depth:",
                min = 1, value = 3, max = 3
            ),
            sliderInput(
                "gamma", label = "Gamma:",
                min = 0, value = 0.3, max = 1
            ),
            
            actionButton("runButton", "Run")
        ),
        
        mainPanel(
            
            h3("This is a webapp for a predictive text algorithm."),
            p("Enter some words (a sentence perhaps) into the textbox in the sidebar"),
            p("Choose the source text for our algorithm to work with"),
            p("Choose the depth for our prediction algorithm"),
            p("\n"),
            p("An empty 'text' will show you the most frequent words"),
            
            tabsetPanel(
                tabPanel("Summary",
                         h4("Your input text: "),
                         verbatimTextOutput("inputString"),
                         h4("The words we are using to predict: "),
                         verbatimTextOutput("predictWords"),
                         h4("Your model choice: "),
                         verbatimTextOutput("modelChoice"),
                         h4("Our predictions"),
                         verbatimTextOutput("finalPrediction"),
                         h4("View 'Plot' and 'Table' to see other predictions")
                         ),
                
                tabPanel("Plot",
                         plotOutput("predictionPlot",
                                    width = "90%")
                         ),
                
                tabPanel("Table",
                         dataTableOutput("predictionTable")
                         )
            )
        )
    )
    

))
