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
    titlePanel("Text Prediction - JLG Nov 21"),
    
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
            p("'Everything' is the largest model, may take some time to run"),
            
            sliderInput(
                "depth", label = "Prediction depth:",
                min = 1, value = 3, max = 3
            ),
            sliderInput(
                "gamma", label = "Gamma:",
                min = 0, value = 0.3, max = 1
            ),
            
            actionButton("runButton", "Run"),
            p("Don't see anything? Click Run and wait a moment")
        ),
        
        mainPanel(
            
            h3("This is a webapp for a predictive text algorithm."),
            p("Enter some words (a sentence perhaps) into the textbox in the sidebar"),
            p("Choose the source text for our algorithm to work with"),
            p("Choose the depth for our prediction algorithm"),
            p("An empty 'text' will show you the most frequent words"),
            
            tabsetPanel(
                tabPanel("Summary",
                         h4("Your input text:"),
                         verbatimTextOutput("inputString", placeholder = TRUE),
                         h4("The words we are using to predict: "),
                         verbatimTextOutput("predictWords", placeholder = TRUE),
                         h4("Your model choice:"),
                         verbatimTextOutput("modelChoice", placeholder = TRUE),
                         h4("Our prediction:"),
                         verbatimTextOutput("finalPrediction", placeholder = TRUE),
                         h5("View 'Plot' and 'Table' to see other predictions")
                         ),
                
                tabPanel("Plot",
                         plotOutput("predictionPlot",
                                    width = "90%")
                         ),
                
                tabPanel("Table",
                         dataTableOutput("predictionTable")
                         ),
                tabPanel("Information",
                         h4("What are the Models?"),
                         p("These models are pre-generated and loaded for this webapp.
                           By necessity, they are trimmed for speed and memory."),
                         h4("Weird Predictions?"),
                         p("These models used 'stemmed' words (where word-endings are removed)
                           to ensure words are grouped together. This is not perfect and duplicates
                           may still exist due to spelling errors and shortcomings."),
                         h4("How does it work?"),
                         p("The models are a collection of words and phrases (n-grams) that occur
                           in a text or a collection of texts. To find a prediction a
                           Markov Chain algorithm is used with a Katz Back-Off model 
                           as well as a variety of smoothing and
                           discounting methods to better predict the correct word."),
                         h4("Links"),
                         tags$a(href="https://en.wikipedia.org/wiki/N-gram",
                                "wiki N-gram"), p(""),
                         tags$a(href="https://en.wikipedia.org/wiki/Markov_chain",
                                "wiki Markov Chains"), p(""),
                         tags$a(href="https://en.wikipedia.org/wiki/Katz%27s_back-off_model",
                                "wiki Katz's Back-Off Model"), p(""),
                         tags$a(href="https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation",
                                "wiki Good-Turing Frequency Estimation"), p(""),
                         tags$a(href="https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing",
                                "wiki Kneser-Ney Smoothing"),
                         h4("A Report on three of the models"),
                         tags$a(href="https://rpubs.com/JL17373/821125",
                                "Twitter_News_and_Blogs"))
            )
        )
    )
    

))
