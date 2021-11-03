#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(tm)
library(ggthemes)
library(openNLP)
library(R.utils)
library(data.table)


source("helper2.R")
load("models/imdb_freq_model.RDa") # imdb_model
load("models/lyrics_freq_model.RDa") # lyics_model
load("models/twitter_freq_model.RDa") # twitter_model
load("models/news_freq_model.RDa") # news_model
load("models/blogs_freq_model.RDa") # blogs_model
#load("models/en_US_freq_model.RDa") # en_US_model

#
#
#
#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$inputString <- renderText({
        print(input$inputString)
    })
    output$modelChoice <- renderText({
        print(input$modelChoice)
    })
    output$depth <- renderText({
        print(userDepth)
    })
    output$gamma <- renderText({
        print(userGamma)
    })
    
    
    parameters <- reactive({
        
        userString <- input$inputString
        
        userGamma <- input$gamma
        
        userDepth <- input$depth
        
        if (input$modelChoice == "Twitter"){
            userModel <- twitter_model
        } else if (input$modelChoice == "Blogs"){
            userModel <- blogs_model
        } else if (input$modelChoice == "News"){
            userModel <- news_model
        } else if (input$modelChoice == "Song Lyrics"){
            userModel <- lyrics_model
        } else if (input$modelChoice == "Movie Reviews"){
            userModel <- imdb_model
        } else{
            userModel <- en_US_model
        }
        
    })
    
    predictions <- reactive({
        
        enable(runButton)
        
        if (input$runButton == 0){
            return()
        }
        
        disable(runButton)
        predictions <- data.table()
        
        if (userString != ""){
            predictions <- isolate(Predict(
                userString, userModel,
                ngram = userDepth,
                gamma = userGamma))
        }
        
        enable(runButton)
        predictions
        
    })
    
    output$predictionTable <- renderTable({
        predictions
    })
    
    output$predictionPlot <- renderPlot({
        
        results <- predictions %>% head(10)
        
        results$word <- factor(results$word, levels = results$word[order(results$P, decreasing = FALSE)])
        
        ggplot(results, aes(y = word, x = P)) + 
            theme_wsj() +
            scale_fill_tableau() +
            scale_colour_tableau() +
            geom_col(orientation = "y") +
            labs(title = "Predictions", x = "Probability", y = "Next Word")
        
    })
    
    
    
    
    
    
    
    

})
