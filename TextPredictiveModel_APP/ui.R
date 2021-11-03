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
                min = 0, value = 3, max = 3
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
            p("Hit 'Run' and see how well we did!"),
            p("If you write 'bottom to the' with depth 3, you should get 'top' as a prediction."),
            
            tabsetPanel(
                tabPanel("Summary",
                         h4("Your text: "),
                         verbatimTextOutput("inputString"),
                         h4("Your model choice: "),
                         verbatimTextOutput("modelChoice"),
                         h4("Our predictions")
                         ),
                
                tabPanel("Plot",
                         plotOutput("predictionPlot")
                         ),
                
                tabPanel("Table",
                         h4("These are the possible words that could have been chosen"),
                         dataTableOutput("predictionTable")
                         )
            )
        )
    )
    

))
