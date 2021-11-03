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
load("models/merged_model.RDa") # merged_model

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
        paste("Source = ", input$modelChoice,
              " Depth = ", input$depth,
              " Gamma = ", input$gamma,
        sep = " ")
    })
    output$depth <- renderText({
        print(input$depth)
    })
    output$gamma <- renderText({
        print(input$gamma)
    })
    
    output$predictWords <- renderText({searchWords()})
    
    searchWords <- reactive({
        wordlist <- StringTailngram(input$inputString,
                              ngram = input$depth,
                              stem = FALSE,
                              filter = NULL,
                              SOS = TRUE)
        words <- sapply(wordlist, paste0, collapse="")
        words
    })
    
    output$finalPrediction <- renderText({
        paste(userString(), predictions()[1, "word"], sep = " ")
    })
    
    
    userString <- eventReactive(input$runButton, {
        
        userStr <- input$inputString
        
        userStr
    })
    
    userGamma <- eventReactive(input$runButton, {
        
        userG <- input$gamma
        
        userG
    })
    
    userDepth <- eventReactive(input$runButton, {
        
        userD <- input$depth
        
        userD
    })

    
    model <- eventReactive(input$runButton, {
        
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
        } else if (input$modelChoice == "Everything"){
            userModel <- merged_model
        }
        
        userModel
    })
    
    predictions <- eventReactive(input$runButton, {
        if(is.null(input$inputString)){
            df <- data.table(word = c("no predictions"),
                             P = 1)
        }
        
        
        df <- isolate(Predict(
            userString(), model(),
            ngram = userDepth(),
            gamma = userGamma()))
        
        df
        
    })
    
    
    
    output$predictionTable <- renderDataTable({predictions() %>% head(100)})
        
    
    output$predictionPlot <- renderPlot({
        
        results <- predictions() %>% head(10)
        
        results$word <- factor(results$word, levels = results$word[order(results$P, decreasing = FALSE)])
        
        ggplot(results, aes(y = word, x = P)) + 
            theme_few() +
            scale_fill_tableau() +
            scale_colour_tableau() +
            geom_col(orientation = "y") +
            labs(title = "Predictions", x = "Probability", y = "Next Word")
        
    })
    
    
    
    
    
    
    
    
    
})
