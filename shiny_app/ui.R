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
  titlePanel("Text Prediction - Data Science Capstone"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h3("Please wait untill you see the message", span("Ready!", style="color:blue")),
       textInput("userInput", "Input a phrase or word", value = "", width = NULL, placeholder = NULL),
       
       submitButton(text = "Predict text", 
                    icon = NULL, 
                    width = NULL),
       width = 5
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h3(span(textOutput("distPlot"), style="color:blue")),
       textOutput("prediction")
    )
  )
))
