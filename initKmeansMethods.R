initializeKMeans_contrib <- function(myData, numK, names){
    
    # Convert to matrix:
    myData <- as.matrix(myData)
    
    # Create the output, called center:
    center <- matrix(NA, nrow = numK, ncol = ncol(myData))
    
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
    
    # If numK == 1 then we are done!
    if (numK > 1) {
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        # NB: This is notoriously inefficient in R, but never mind that for now.
        #     I've also implemented this in C++ (via Rcpp)
        
        # Calculate the distance from the other points to the first center:
        Dj <- rep(NA,nrow(myData))
        #    we initialized Dj as a bunch of NA's
        
        for (k in 2:numK) {
            
            # This vector will determine the next selected seed:
            sumC <- numeric(nrow(myData))
            
            # Find the distance to the nearest center:
            Dj <-
                mapply(FUN = min,
                       Dj,
                       apply(X = scale(myData, center = center[k-1,], scale = FALSE),
                             MARGIN = 1,
                             FUN = function(x){sqrt(crossprod(x))}),
                       na.rm = TRUE  # This is needed for the initial calucations
                )
            # NB: This works in the first iteration because
            #     we initialized Dj as a bunch of NA's
            
            #Then, for each element, calculate the gain of selecting it:
            for (i in 1:nrow(myData)) {
                # Calculate the distance from all points to point i:
                dji <- apply(X = scale(myData, center = myData[i,], scale = FALSE),
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
    center <- matrix(NA, nrow = numK, ncol = ncol(myData))
    
    #Set the first center as the element with max{||x||}
    selected <-
        which.max(apply(X = myData,
                        MARGIN = 1,
                        FUN = function(x){sqrt(crossprod(x))})
        )
    center[1,] <- myData[selected,]
    
    # If numK == 1 then we are done!
    if (numK > 1) {
    
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        # NB: This is notoriously inefficient in R, but never mind that for now.
        #     I've also implemented this in C++ (via Rcpp)
        
        # Calculate the distance from the other points to the first center:
        Dj <- rep(NA,nrow(myData))
        #    we initialized Dj as a bunch of NA's
        
        # Determine the next numK-1 centers:
        for (k in 2:numK) {
            
            # Find the distance to the nearest center:
            Dj <-
                mapply(FUN = min,
                       Dj,
                       apply(X = scale(myData, center = center[k-1,], scale = FALSE),
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
    center <- matrix(NA, nrow = numK, ncol = ncol(myData))
    
    #Set the first center at random:
    selected <- sample(x = 1:nrow(myData), size = 1)
    center[1,] <- myData[selected,]
    
    # If numK == 1 then we are done!
    if (numK > 1) {
    
        # Then, remove the selected seed:
        myData <- myData[-selected,]
        # NB: This is notoriously inefficient in R, but never mind that for now.
        #     I've also implemented this in C++ (via Rcpp)
        
        # Calculate the distance from the other points to the first center:
        Dj <- rep(NA,nrow(myData))
        #    we initialized Dj as a bunch of NA's
        
        # Determine the next numK-1 centers:
        for (k in 2:numK) {
            
            # Find the distance to the nearest center:
            Dj <-
                mapply(FUN = min,
                       Dj,
                       apply(X = scale(myData, center = center[k-1,], scale = FALSE),
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
    }
    
    # We should now have selected numK seeds:
    center <- as.data.frame(center)
    names(center) <- names
    return(center)
}