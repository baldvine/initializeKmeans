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
    titlePanel("Initializing k-means"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            h4("Initialization is key when using the k-means algorithm. Here, we inspect four methods. Select one and see how they differ."),
            # # Ideally, I'd have a few datasets to choose from:
            selectInput(inputId = "dataSet",
                        label = "Choose Data Set:",
                        choices =  c("Ruspini" = "ruspini",
                                     "xClara" = "xclara",
                                     #"Iris" = "iris",
                                     "Gaussian" = "gauss",
                                     "Uniform" = "unif",
                                     "One Circle" = "oneCircle",
                                     "Two Circles" = "twoCircles",
                                     "Bullseye" = "bullseye"),
                        selected = "ruspini"),
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
                        ticks = FALSE,
                        step = 1),
            # # Can't decide if I prefer slider or box:
            # numericInput(inputId = "numCluster", 
            #              label = "Number of clusters", 
            #              value = 4, min = 1, max = 12, 
            #              step = 1)
            checkboxInput(inputId = "showInitial", 
                          label = "Show initial cluster centers", 
                          value = TRUE),
            checkboxInput(inputId = "showCenter", 
                          label = "Show resulting cluster centers", 
                          value = TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h2("Baldvin Einarsson, PhD"),
            h2("Visualization"),
            helpText(a("See github page for details on initialization methods (and code!)",
                       href = "https://github.com/baldvine/initializeKmeans",
                       target = "_blank")
            ),
            plotOutput("kMeansPlot")
        )
        
    )
))
