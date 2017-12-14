#
#

library(shiny)
library(ggplot2)
library(cluster)
library(flexclust)

source("./initKmeansMethods.R")

shinyServer(function(input, output) {
    
    
    # Choose dataset:
    data2use <- reactive({
        if (input$dataSet == "ruspini") {
            return(ruspini)
        }
        if (input$dataSet == "unif") {
            return(data.frame(x = runif(1000), y = runif(1000)))
        }
        if (input$dataSet == "gauss") {
            return(data.frame(x = rnorm(1000), y = rnorm(1000)))
        }
        if (input$dataSet == "oneCircle") {
            rad <- 2*pi*runif(250)
            return(data.frame(x = cos(rad), y = sin(rad)))
        }
        if (input$dataSet == "twoCircles") {
            outer <- 2*pi*runif(350)
            inner <- 2*pi*runif(200)
            return(data.frame(x = c(cos(outer),0.5*cos(inner)), 
                              y = c(sin(outer),0.5*sin(inner)))
            )
        }
        if (input$dataSet == "bullseye") {
            rad <- 2*pi*runif(250)
            return(data.frame(x = c(cos(rad),rnorm(300, sd = 0.2)), 
                              y = c(sin(rad),rnorm(300, sd = 0.2)))
            )
        }
        })
    
    # Get number of clusters:
    numClusters <- reactive({input$numCluster})
    
    # Get initialization method. Options should be the following:
    # c("Random" = "rand",
    #   "KA method (Deterministic)" = "detContrib",
    #   "Extreme points (Deterministic)" = "detExtreme",
    #   "k-means++" = "kpp"))
    selectedInitMethod <- reactive({input$initMethod})
    
    initClusters <- reactive({
        if (selectedInitMethod() == "rand") {
            return(data2use()[sample(x = 1:nrow(data2use()), 
                                     size = numClusters(), 
                                     replace = FALSE),])
        }
        if (selectedInitMethod() == "detContrib") {
            return(initializeKMeans_contrib(data2use(), 
                                            numClusters(), 
                                            names = names(data2use()))
            )
        }        
        if (selectedInitMethod() == "detExtreme") {
            return(initializeKMeans_extreme(data2use(), 
                                            numClusters(), 
                                            names = names(data2use()))
            )
        }
        if (selectedInitMethod() == "kpp") {
            return(initializeKMeans_pp(data2use(), 
                                       numClusters(), 
                                       names = names(data2use()))
            )
        }
    })
    
    kMeansSolution <- reactive({
        if (selectedInitMethod() %in% c("detContrib", "detExtreme")) {
            numStart <- 1
        } else {
            numStart <- 25
        }
        return(kmeans(x = data2use(), 
                      centers = initClusters(), 
                      nstart = numStart))
    })
    
    output$kMeansPlot <- renderPlot({
        
        showResultCenters <- input$showCenter
        myDF <- cbind(rbind(data2use(),initClusters(),kMeansSolution()$centers),
                      cluster = as.factor(c(kMeansSolution()$cluster,1:numClusters(),1:numClusters())),
                      Type = factor(c(rep("Data",nrow(data2use())),
                                    rep("Initial center",numClusters()),
                                    rep("K-means center",numClusters()))
                      )
        )
        if (!showResultCenters) {
            myDF <- myDF[!grepl("K-means center", myDF$Type),]
        }
        
        myPlot <- 
            ggplot(data = myDF, aes(x,y, color = cluster, shape = Type, size = Type)) +
            geom_point() +
            #scale_color_manual(values = #c("blue","red","gold","purple")) +
            scale_color_brewer(palette = "Set1") + 
            scale_shape_manual(values = c(20,1,8)) +
            scale_size_manual(values = c(4,5,4), guide = FALSE) +
            guides(color = guide_legend(title = "Cluster", 
                                        #override.aes = list(size=5), 
                                        order = 2),
                   shape = guide_legend(title = "Type", order = 1)) +
            theme(plot.title = element_text(hjust = 0.5))
        
        #        generate bins based on input$bins from ui.R
        # x    <- faithful[, 2] 
        # bins <- seq(min(x), max(x), length.out = input$numCluster + 1)
        # 
        # # 
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')

        if (input$dataSet %in% c("oneCircle","twoCircles", "bullseye")) {
            myPlot <- myPlot + coord_equal()
        }
        
        return(myPlot)        
    })
    
})

