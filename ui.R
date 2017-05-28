#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualize k-means"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "initMethod", 
                  label = "Choose Initialization Method:", 
                  choices =  c("Random" = "rand",
                               "KA method (Deterministic)" = "detContrib",
                               "Extreme points (Deterministic)" = "detExtreme",
                               "k-means++" = "kpp"), 
                  selected = "rand"),
      sliderInput(inputId = "numCluster", 
                  label = "Number of clusters",
                  value = 4, min = 1, max = 9,
                  step = 1),
       # numericInput(inputId = "numCluster", 
       #              label = "Number of clusters", 
       #              value = 4, min = 1, max = 12, 
       #              step = 1)
      checkboxInput(inputId = "showCenter", 
                    label = "Show resulting cluster centers", 
                    value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Plot and such - change!"),
       plotOutput("kMeansPlot")
    )
  )
))
