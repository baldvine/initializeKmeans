#
#

library(shiny)
library(ggplot2)
library(cluster)
library(flexclust)
library(magrittr)
library(Rcpp)
library(RcppArmadillo)
library(tclust)

useCpp <- TRUE  # C++ versions exist!
if (useCpp) {
    sourceCpp("./initializeKmeans.cpp")
} else {
    source("./initKmeansMethods.R")
}

shinyServer(function(input, output) {
    
    
    # Choose dataset:
    data2use <- reactive({
        if (input$dataSet == "ruspini") {
            return(ruspini)
        }
        if (input$dataSet == "xclara") {
            myData <- xclara
            names(myData) <- c("x","y")
            return(myData)
        }
        if (input$dataSet == "unif") {
            return(data.frame(x = runif(1000), y = runif(1000)))
        }
        if (input$dataSet == "gauss") {
            return(data.frame(x = rnorm(1000), y = rnorm(1000)))
        }
        if (input$dataSet == "oneCircle") {
            rad <- 2*pi*runif(250)
            len <- rnorm(250, mean = sqrt(2), sd = 0.01)
            return(data.frame(x = len*cos(rad), y = len*sin(rad)))
        }
        if (input$dataSet == "twoCircles") {
            outerRad <- 2*pi*runif(350)
            outerLen <- rnorm(350, mean = sqrt(2), sd = 0.02)
            innerRad <- 2*pi*runif(200)
            innerLen <- rnorm(200, mean = 1, sd = 0.01)
            return(data.frame(x = c(outerLen*cos(outerRad),innerLen*cos(innerRad)), 
                              y = c(outerLen*sin(outerRad),innerLen*sin(innerRad)))
            )
        }
        if (input$dataSet == "bullseye") {
            rad <- 2*pi*runif(250)
            len <- rnorm(250, mean = sqrt(2), sd = 0.01)
            return(data.frame(x = c(len*cos(rad),rnorm(300, sd = 0.2)), 
                              y = c(len*sin(rad),rnorm(300, sd = 0.2)))
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
            if (useCpp) {
                return(
                    initializeKMeans_contrib_cpp(as.matrix(data2use()),numClusters()) %>% 
                        as.data.frame() %>% 
                        setNames(nm = names(data2use()))
                    )
            } else {
                return(initializeKMeans_contrib(data2use(),
                                                numClusters(),
                                                names = names(data2use()))
                )
            }
        }        
        if (selectedInitMethod() == "detExtreme") {
            if (useCpp) {
                return(
                    initializeKMeans_extreme_cpp(as.matrix(data2use()),numClusters()) %>% 
                        as.data.frame() %>% 
                        setNames(nm = names(data2use()))
                    )
            } else{
                return(initializeKMeans_extreme(data2use(),
                                                numClusters(),
                                                names = names(data2use()))
                )
            }
        }
        if (selectedInitMethod() == "kpp") {
            if (useCpp) {
                return(
                    initializeKMeans_pp_cpp(as.matrix(data2use()),numClusters()) %>%
                        as.data.frame() %>%
                        setNames(nm = names(data2use()))
                )
            } else {
                return(initializeKMeans_pp(data2use(),
                                           numClusters(),
                                           names = names(data2use()))
                )
            }
        }
    })
    
    # Obtatin kmeans solution:
    kMeansSolution <- reactive({
        if (selectedInitMethod() %in% c("detContrib", "detExtreme")) {
            numStart <- 1
        } else {
            numStart <- 25
        }
        
        return(stats::kmeans(x = data2use(), 
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
            geom_point(alpha = 0.8) +
            #scale_color_manual(values = #c("blue","red","gold","purple")) +
            scale_color_brewer(palette = "Set1") + 
            scale_shape_manual(values = c(20,1,8)) +
            scale_size_manual(values = c(4,5,4), guide = FALSE) +
            guides(color = guide_legend(title = "Cluster", 
                                        #override.aes = list(size=5), 
                                        order = 2),
                   shape = guide_legend(title = "Type", order = 1)) +
            theme(plot.title = element_text(hjust = 0.5), 
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 12))
        
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

