#
#

library(shiny)
library(ggplot2)
library(cluster)
library(flexclust)



shinyServer(function(input, output) {
    
    
    # Let's just use the ruspini data from the cluster package
    # At some point it would be fun to give the option to change
    #     the dataset used
    data2use <- reactive({ruspini})
    
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
            return(data2use()[sample(x = 1:nrow(data2use()), size = numClusters(), replace = FALSE),])
        }
        if (selectedInitMethod() == "detContrib") {
            return(initializeKMeans_contrib(data2use(), numClusters(), names = names(data2use())))
        }        
        if (selectedInitMethod() == "detExtreme") {
            return(initializeKMeans_extreme(data2use(), numClusters(), names = names(data2use())))
        }
        if (selectedInitMethod() == "kpp") {
            return(initializeKMeans_pp(data2use(), numClusters(), names = names(data2use())))
        }
    })
    
    kMeansSolution <- reactive({
        if (selectedInitMethod() %in% c("detContrib", "detExtreme")){
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
                      cluster=as.factor(c(kMeansSolution()$cluster,1:numClusters(),1:numClusters())),
                      Type=factor(c(rep("Data",nrow(data2use())),
                                    rep("Initial center",numClusters()),
                                    rep("K-means center",numClusters()))
                                  )
        )
 
        ggplot(data = myDF, aes(x,y, color=cluster, shape=Type)) +
            geom_point(size=4) +
            #scale_color_manual(values = #c("blue","red","gold","purple")) +
            scale_color_brewer(palette = "Set1") + 
            scale_shape_manual(values = c(20,1,8)) +
            guides(color = guide_legend(title = "Initial Compression", 
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
        
    })
    
})



initializeKMeans_contrib <- function(myData, numK, names){
    
    # Convert to matrix:
    myData <- as.matrix(myData)
    
    # Create the output, called center:
    center <- matrix(NA, nrow=numK, ncol=ncol(myData))
    
    # center with 'colMeans()', which according to
    #     http://gastonsanchez.com/how-to/2014/01/15/Center-data-in-R/
    #     is pretty fast. Scale works pretty well too.
    center_colmeans <- function(x) {
        xcenter = colMeans(x)
        x - rep(xcenter, rep.int(nrow(x), ncol(x)))
    }
    
    # Set the first center as the element nearest to the center:
    selected <-
        which.min(apply(X = center_colmeans(myData),
                        MARGIN = 1,
                        FUN = function(x){sqrt(crossprod(x))})
        )
    center[1,] <- myData[selected,]
    
    # Then, remove the selected seed:
    myData <- myData[-selected,]
    # NB: This is notoriously inefficient in R, but never mind that for now.
    #     I've also implemented this in C++ (via Rcpp)
    
    # Calculate the distance from the other points to the first center:
    Dj <- rep(NA,nrow(myData))
    #    we initialized Dj as a bunch of NA's
    
    for (k in 2:numK){
        
        # This vector will determine the next selected seed:
        sumC <- numeric(nrow(myData))
        
        # Find the distance to the nearest center:
        Dj <-
            mapply(FUN = min,
                   Dj,
                   apply(X = scale(myData, center=center[k-1,], scale = FALSE),
                         MARGIN = 1,
                         FUN = function(x){sqrt(crossprod(x))}),
                   na.rm = TRUE  # This is needed for the initial calucations
            )
        # NB: This works in the first iteration because
        #     we initialized Dj as a bunch of NA's
        
        #Then, for each element, calculate the gain of selecting it:
        for (i in 1:nrow(myData)){
            # Calculate the distance from all points to point i:
            dji <- apply(X = scale(myData, center=myData[i,], scale = FALSE),
                         MARGIN = 1,
                         FUN = function(x){sqrt(crossprod(x))})
            # NB: We have already calculated the Dj's:
            
            # Then, determine the contribution Cji
            Cji <- mapply(max,Dj-dji,0)
            sumC[i] <- sum(Cji)
        }
        
        # Find the element which maximizes sumC:
        selected <- which.max(sumC)
        # ... and set as the new center:
        center[k,] <- myData[selected,]
        
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        Dj <- Dj[-selected]
    }
    
    # We should now have selected numK seeds:
    center <- as.data.frame(center)
    names(center) <- names
    return(center)
}

initializeKMeans_extreme <- function(myData, numK, names){
    
    # Convert to matrix, if not already a matrix:
    myData <- as.matrix(myData)
    
    # Create the output, called center:
    center <- matrix(NA, nrow=numK, ncol=ncol(myData))
    
    #Set the first center as the element with max{||x||}
    selected <-
        which.max(apply(X = myData,
                        MARGIN = 1,
                        FUN = function(x){sqrt(crossprod(x))})
        )
    center[1,] <- myData[selected,]
    
    # Then, remove the selected seed:
    myData <- myData[-selected,]
    # NB: This is notoriously inefficient in R, but never mind that for now.
    #     I've also implemented this in C++ (via Rcpp)
    
    # Calculate the distance from the other points to the first center:
    Dj <- rep(NA,nrow(myData))
    #    we initialized Dj as a bunch of NA's
    
    # Determine the next numK-1 centers:
    for (k in 2:numK){
        
        # Find the distance to the nearest center:
        Dj <-
            mapply(FUN = min,
                   Dj,
                   apply(X = scale(myData, center=center[k-1,], scale = FALSE),
                         MARGIN = 1,
                         FUN = function(x){sqrt(crossprod(x))}),
                   na.rm = TRUE  # This is needed for the initial calucations
            )
        # NB: This works in the first iteration because
        #     we initialized Dj as a bunch of NA's
        
        # Find the element which has maximum distance to its nearest center:
        selected <- which.max(Dj)
        # ... and set as the new center:
        center[k,] <- myData[selected,]
        
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        Dj <- Dj[-selected]
        # NB: This is notoriously inefficient in R, but never mind that for now.
        #     I've also implemented this in C++ (via Rcpp)
    }
    
    # We should now have selected numK seeds:
    center <- as.data.frame(center)
    names(center) <- names
    return(center)
}

initializeKMeans_pp <- function(myData, numK, names){
    
    # Convert to matrix, if not already a matrix:
    myData <- as.matrix(myData)
    
    # Create the output, called center:
    center <- matrix(NA, nrow=numK, ncol=ncol(myData))
    
    #Set the first center at random:
    selected <- sample(x = 1:nrow(myData), size = 1)
    center[1,] <- myData[selected,]
    
    # Then, remove the selected seed:
    myData <- myData[-selected,]
    # NB: This is notoriously inefficient in R, but never mind that for now.
    #     I've also implemented this in C++ (via Rcpp)
    
    # Calculate the distance from the other points to the first center:
    Dj <- rep(NA,nrow(myData))
    #    we initialized Dj as a bunch of NA's
    
    # Determine the next numK-1 centers:
    for (k in 2:numK){
        
        # Find the distance to the nearest center:
        Dj <-
            mapply(FUN = min,
                   Dj,
                   apply(X = scale(myData, center=center[k-1,], scale = FALSE),
                         MARGIN = 1,
                         FUN = function(x){sqrt(crossprod(x))}),
                   na.rm = TRUE  # This is needed for the initial calucations
            )
        # NB: This works in the first iteration because
        #     we initialized Dj as a bunch of NA's
        
        # Select the next element proportional to the distance to the nearest center:
        selected <- sample(x = 1:nrow(myData), size = 1, prob = (Dj/sum(Dj)))
        # ... and set as the new center:
        center[k,] <- myData[selected,]
        
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        Dj <- Dj[-selected]
        # NB: This is notoriously inefficient in R, but never mind that for now.
        #     I've also implemented this in C++ (via Rcpp)
    }
    
    # We should now have selected numK seeds:
    center <- as.data.frame(center)
    names(center) <- names
    return(center)
}