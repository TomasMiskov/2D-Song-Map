## ---------------------------
## Script Name: Hierarchical_clustering
## Author: Tomas Miskov
## Date Created: 2022-02-21
## Purpose: Own implementation of hierarchical clustering
## ---------------------------


#' Main hierarchical clustering function
#' 
#' Perform aglomerative hierarchical clustering
#' 
#' @param distD The input distance matrix/dist object
#' @param sLinkage Type of the linkage method between clusters (complete or single)
#' 
#' @return A list containing 3 elements:
#' * heights: a vector of heights at which the clusters were joined.
#' * joins: pairs of clusters being joined at each iteration.
#' * clusters: cluster indices for all observations at each iteration.
hcluster <- function(distD, sLinkage){
  
  # CATCHING ERRORS
  
  linkages <- list("complete", "single")
  if(!(sLinkage %in% linkages)){
    stop("Invalid linkage. Use either 'complete' or 'single' linkage")
  }
  if(!(is.matrix(distD) | class(distD) == "dist")){
    stop("Invalid data type. Use either matrix of distances or dist object as input")
  }
  
  # INITIALIZATION
  
  if(!(is.matrix(distD))){
    mDist <- as.matrix(distD)                     # matrix of distances
  }
  iN <- nrow(mDist)                               # no. of observations
  
  lH <- rep(NA, iN-1)                             # list of merge heights
  mC <- matrix(0, nrow = iN, ncol = iN)           # matrix of cluster assignments
  mC[1,] <- seq(iN)                               # start with N clusters
  
  mJ <- matrix(nrow = iN-1, ncol = 2)             # matrix of pairwise cluster joins
  
  mClosest <- matrix(nrow = iN, ncol = 3)         # matrix of closest cluster pairs
  mClosest[,1] <- seq(iN)                         # start with N clusters
  mClosest[,3] <- Inf                             # set cluster distances to infinity
  for(i in seq(iN)){
    for(j in seq(iN)){
      if(i != j){
        if(mDist[i,j] < mClosest[i,3]){           # looping over all the distances
          mClosest[i,2] <- j                      # store the closest cluster
          mClosest[i,3] <- mDist[i,j]             # and the corresponding distance
        } 
      }
    }
  }
  
  # MAIN LOOP
  
  for(k in seq(1, iN-1)){
    minHeight <- min(mClosest[,3])                      # find closest cluster pair
    c1 <- mClosest[which(mClosest[,3] == minHeight)][1]
    c2 <- mClosest[which(mClosest[,3] == minHeight)][2] 
    
    lH[k] <- minHeight                                  # store the height
    mJ[k,] <- c(c1, c2)                                 # store the merging labels
    
    for(i in seq(iN)){                                  # store the cluster assignment
      if(mC[k,i] == mC[k,c2]){                          # indices 
        mC[k+1,i] <- mC[k,c1]
      } else {
        mC[k+1,i] <- mC[k,i]
      }
      if(mC[k,i] > mC[k,c2]){
        mC[k+1,i] <- mC[k,i] - 1
      }
    }
  
    for(i in seq(iN)){                                  # edit the distance matrix
      if(sLinkage == "complete"){                       # based on the linkage
        if(mDist[c2, i] > mDist[c1, i]){                # criterion
          mDist[c1, i] = mDist[i, c1] = mDist[c2, i]
        }
      }
      if(sLinkage == "single"){
        if(mDist[c2, i] < mDist[c1, i]){
          mDist[c1, i] = mDist[i, c1] = mDist[c2, i]
        }
      }
    }
    mDist[c1, c1] = 0                        # keep the diagonal at 0
    
    for(i in seq(iN)){                       # eliminate one of the clusters upon
      mDist[c2, i] = mDist[i, c2] = Inf      # joining by setting all its distances
    }                                        # to infinity
    
    mClosest[c2,] <- Inf                     # eliminate the cluster from pairwise
    mClosest[, 3] <- Inf                     # comparison & set all distances to Inf
    
    for(i in seq(iN)){                       # compute new pairwise distances
      for(j in seq(iN)){                     # between all the previous clusters
        if(i != j){                          # and the newly formed cluster
          if(mDist[i,j] < mClosest[i,3]){
            mClosest[i,2] <- j
            mClosest[i,3] <- mDist[i,j]
          } 
        }
      }
    }
  }                                          # repeat N-1 times
  
  return(list("heights" = lH, "joins" = mJ, "clusters" = mC))
}
