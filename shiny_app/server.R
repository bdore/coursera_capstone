#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(slam)
library(dplyr)
library(quanteda)
library(data.table)
library(stringr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  source("prob_calc_nextword.R")
  ready <- "Ready!"
  sv_prediction <- reactive({
    if (input$userInput != ""){
      suggestions <- ngramProb(input$userInput)
    }
  })

   
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  
  output$distPlot <- renderText(ready)
  
  
  output$prediction <- renderText(sv_prediction())
  
})
