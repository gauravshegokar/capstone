
library(shiny)
library(stringr)
library(tau)
library(dplyr)
library(tm)

shinyServer(function(input, output) {
    output$wait <- renderText("")
    source("./predict.R")
    wordPrediction <- reactive({
        predictNextWord(input$text)})
    
    output$predictedWordLeft <- renderText(wordPrediction()[2])
    output$predictedWordMid <- renderText(wordPrediction()[1])
    output$predictedWordRight <- renderText(wordPrediction()[3])
    
    
})
