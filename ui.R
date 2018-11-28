
library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(DT)
data("mtcars")
# Define UI for application that plots different variables of mtcars dataset

ui <- fluidPage(
  titlePanel("Capstone Project Text Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Enter Text for Plot Title
      textAreaInput(inputId = "text",
                label = "Enter text with minimum 3 words",
                value = "how are you")
      # Select Auto or Manual Gear Transmission for plot analysis
      #checkboxGroupInput(inputId = "transmision_type",
      #label = "Select Type of Transmission",
      #choices = c(0,1),
      #selected = 0)
    ),
    # Output
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Instructions",
                           h3("How to use this App"),
                           br(),
                           h4("Introduction"),
                           p("This Shiny App is used to predict the next word based on past 3 words of a given line of text"),
                           hr(),
                           h4("Selection Screen"),
                           br(),
                           h5("Input Text"),
                           p("Enter a line of text with minimum 3 words"),
                           br(),
                           h5("Prediction Output"),
                           p("In the tab 'Text Prediction', you will see the top 3 next word predictions for the input text")
                           ),
                  tabPanel("Text Prediction",
                           h3("Text Prediction Below"),
                           h4("The Input text is:"),
                           textOutput(outputId = "input_text"),
                           br(),                              
                           h4("The Top 3 Predictions are:"),
                           br(),
                           textOutput(outputId = "prediction1"),
                           textOutput(outputId = "prediction2"),
                           textOutput(outputId = "prediction3")
                           )
                           )
                           )
)
)
