
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>
#include <numeric>

using namespace Rcpp;
//using namespace arma;


// [[Rcpp::export]]
NumericMatrix initializeKMeans_contrib_cpp(NumericMatrix myData, int numK) {

    // Get the dimensions:
    int nColumns = myData.ncol();
    int nRows = myData.nrow();
    
    // Create the output, called center:
    NumericMatrix center(numK, nColumns);

    // Distance to nearest center:
    NumericVector Dj(nRows);
    double Dj_tmp;
    // Distance from element j to i:
    NumericVector dji(nRows);
    // Contribution:
    NumericVector Cji(nRows);
    // Total Contribution:
    NumericVector sumC(nRows);
    
    //Set the first center as the element nearest to the center
    // First, we find the data center:
    for(int i=0; i<nColumns; i++){
        center(0,i) = mean(myData(_,i));  // Note the temporary use of center(0,_)
    }
    // Calculate the distance to that center:
    for(int i=0; i<nRows; i++){
        Dj[i] = sqrt(sum(pow((myData(i,_) - center(0,_)), 2)));
        // Note: This is recalculated below once the actual center is found
    }
    // Find the closest one:
    int selected = which_min(Dj);  // NB: This is syntactic sugar!
    // ... and set as first center:
    center(0,_) = myData.row(selected);
    // Calculate the distance to that center:
    for(int i=0; i<nRows; i++){
        Dj[i] = sqrt(sum(pow((myData(i,_) - center(0,_)), 2)));
    }
    
    // Before starting the fun, let's put the selected element last:
    NumericVector tmpVec = myData.row(selected);
    myData.row(selected) = myData.row(nRows-1);
    myData.row(nRows-1) = tmpVec;
    // NB: We also have to do this for Dj:
    double tmp = Dj[selected];
    Dj[selected] = Dj[nRows-1];
    Dj[nRows-1] = tmp;
    
    // Ok, start the fun! Note that the number of elements we want to work
    // with is nRows-k, where k is the cluster number we're interested in
    for(int k=1; k<numK; k++){
        
        // sumC will determine the next selected seed, but only elements nRows-k
        
        if(k>1){
            // Then we need to update the nearest center
            for(int i=0; i<(nRows-k); i++){
                // Calculate the distance to the last found center:
                Dj_tmp = sqrt(sum(pow((myData(i,_) - center(k-1,_)), 2))); // NB: k-1
                // Check if nearest:
                if(Dj_tmp < Dj[i]){
                    Dj[i] = Dj_tmp;
                }  // else we do nothing
            }
        }

        //Then, for each element, calculate the gain of selecting it:
        for(int i=0; i<(nRows-k); i++){
            // Calculate the distance from all points to point i:
            sumC[i] = 0;
            for(int j=0; j<(nRows-k); j++){
                dji[j] = sqrt(sum(pow((myData(j,_) - myData(i,_)), 2)));
                // Then, determine the contribution Cji
                Cji[j] = max(NumericVector::create(Dj[j]-dji[j],0));
                sumC[i] += Cji[j];
            }
            //sumC[i] = sum(Cji);  // NB: This shouldn't include the last elements
        }

        // Find the element which maximizes sumC:
        selected = 0;
        for(int i=1; i<(nRows-k); i++){
            if(sumC[i] > sumC[selected]){
                selected = i;
            }
        }
        // ... and set center k:
        center(k,_) = myData.row(selected);
        
        // Let's put the selected element near the end:
        NumericVector tmpVec = myData.row(selected);
        myData.row(selected) = myData.row(nRows-(k+1));
        myData.row(nRows-(k+1)) = tmpVec;
        // NB: We also have to do this for Dj:
        tmp = Dj[selected];
        Dj[selected] = Dj[nRows-(k+1)];
        Dj[nRows-(k+1)] = tmp;
        
    }
    
    return center;
}






// [[Rcpp::export]]
NumericMatrix initializeKMeans_extreme_cpp(NumericMatrix myData, int numK) {
    
    // Get the dimensions:
    int nColumns = myData.ncol();
    int nRows = myData.nrow();
    
    // Create the output, called center:
    NumericMatrix center(numK, nColumns);
    
    // Distance to nearest center:
    NumericVector D(nRows);
    double D_tmp;

    //Set the first center as the element with max{||x||}
    // First, find the norm:
    for(int i=0; i<nRows; i++){
        D[i] = sqrt(sum(pow(myData(i,_), 2)));
        // Note: This is recalculated below once the actual max is found
    }
    // Find the largest one:
    int selected = which_max(D);  // NB: This is syntactic sugar!
    // ... and set as first center:
    center(0,_) = myData.row(selected);
    // Calculate the distance to that center:
    for(int i=0; i<nRows; i++){
        D[i] = sqrt(sum(pow((myData(i,_) - center(0,_)), 2)));
        // Note: D[selected]=0
    }
    
    // Before starting the fun, let's put the selected element last:
    NumericVector tmpVec = myData.row(selected);
    myData.row(selected) = myData.row(nRows-1);
    myData.row(nRows-1) = tmpVec;
    // NB: We also have to do this for D:
    double tmp = D[selected];
    D[selected] = D[nRows-1];
    D[nRows-1] = tmp;
    
    // Ok, start the fun! Note that the number of elements we want to work
    // with is nRows-k, where k is the cluster number we're interested in
    for(int k=1; k<numK; k++){

        // Find distance to the nearest center
        // NB: When k=1 we're repeating the calculations, but that's ok
        for(int i=0; i<(nRows-k); i++){
            // Calculate the distance to the last found center:
            D_tmp = sqrt(sum(pow((myData(i,_) - center(k-1,_)), 2))); // NB: k-1
            // Check if nearest:
            if(D_tmp < D[i]){
                D[i] = D_tmp;
            }  // else we do nothing
        }
        
        // Find the element which maximizes D:
        selected = 0;
        for(int i=1; i<(nRows-k); i++){
            if(D[i] > D[selected]){
                selected = i;
            }
        }
        // ... and set center k:
        center(k,_) = myData.row(selected);
        
        // Let's put the selected element near the end:
        NumericVector tmpVec = myData.row(selected);
        myData.row(selected) = myData.row(nRows-(k+1));
        myData.row(nRows-(k+1)) = tmpVec;
        // NB: We also have to do this for D:
        tmp = D[selected];
        D[selected] = D[nRows-(k+1)];
        D[nRows-(k+1)] = tmp;
        
    }
    
    return center;
}






// [[Rcpp::export]]
NumericMatrix initializeKMeans_pp_cpp(NumericMatrix myData, int numK) {
    
    // Get the dimensions:
    int nColumns = myData.ncol();
    int nRows = myData.nrow();
    
    // Create the output, called center:
    NumericMatrix center(numK, nColumns);

    // Set up an integer vector of indices:
    IntegerVector indices = seq_len(nRows);
    // This creates a vector 1:nRows, but we need to subtract 1:
    for(int i=0; i<nRows; i++){
        indices[i] = indices[i] - 1;
    }
    
    // Distance to nearest center and probabilities:
    NumericVector D(nRows);
    double D_tmp;
    double D_sum;
    NumericVector probs(nRows);

        
    //Set the first center at random:
    int selected = RcppArmadillo::sample(indices, 1, TRUE, NumericVector::create())[0];
    center(0,_) = myData.row(selected);
    
    // Calculate the distance to that center:
    D_sum = 0;
    for(int i=0; i<nRows; i++){
        D[i] = sqrt(sum(pow((myData(i,_) - center(0,_)), 2)));
        D_sum = D_sum + D[i];
        // Note: D[selected]=0
    }
    // ... and the probabilities:
    for(int i=0; i<nRows; i++){
        probs[i] = D[i]/D_sum;
        // Note: probs[selected]=0
    }
    
    // // Before starting the fun, let's put the selected element last:
    // NumericVector tmpVec = myData.row(selected);
    // myData.row(selected) = myData.row(nRows-1);
    // myData.row(nRows-1) = tmpVec;
    // // NB: We also have to do this for D:
    // double tmp = D[selected];
    // D[selected] = D[nRows-1];
    // D[nRows-1] = tmp;

    //double tmp;
    
    // Ok, start the fun! 
    for(int k=1; k<numK; k++){
        
        // Find distance to the nearest center
        for(int i=0; i<nRows; i++){
            D_sum = 0;
            // Calculate the distance to the last found center:
            D_tmp = sqrt(sum(pow((myData(i,_) - center(k-1,_)), 2))); // NB: k-1
            // Check if nearest:
            if(D_tmp < D[i]){
                D[i] = D_tmp;
            }  // else we do nothing
            D_sum = D_sum + D[i];
            // Note: D[is a center]=0
        }
        // Update the probabilities:
        for(int i=0; i<nRows; i++){
            probs[i] = D[i]/D_sum;
            // Note: probs[is a center]=0
        }
        
        // Randomly select next index, but based on weights D:
        selected = RcppArmadillo::sample(indices, 1, TRUE, D)[0];
        // ... and set center k:
        center(k,_) = myData.row(selected);
        
        // // Let's put the selected element near the end:
        // NumericVector tmpVec = myData.row(selected);
        // myData.row(selected) = myData.row(nRows-(k+1));
        // myData.row(nRows-(k+1)) = tmpVec;
        // // NB: We also have to do this for D:
        // tmp = D[selected];
        // D[selected] = D[nRows-(k+1)];
        // D[nRows-(k+1)] = tmp;
        
    }
    
    return center;
}























