## ---------------------------
## Script Name: Hierarchical_clustering_main
## Author: Tomas Miskov
## Date Created: 2022-02-21
## Purpose: Test out my own implementation of hierarchical clustering
## ---------------------------

#--------
# SET UP |
#--------
rm(list=ls())                                         # clean the environment
if (!require("pacman")) install.packages("pacman")    # install pacman
pacman::p_load(cluster)                               # pre-load packages
source("Hierarchical_clustering.R")                   # load local libraries

#------
# DATA |
#------
data <- read.csv("top50NL.csv")
data <- data[,-1]
features <- data[,1:12]
features <- features[, -c(11)]
distD <- dist(features, method = "euclidean")

#---------
# PACKAGE |
#---------
package <- hclust(distD, method = "complete") 

#-------------------
# OWN IMPLEMENTATIO |
#-------------------
myImp <- hcluster(distD, "complete")

#------------
# COMPARISON |
#------------
cat("The mean difference in heights is:", mean(package$height - myImp$heights))
cat("The cluster indices for 3 clusters from the package are:\n", cutree(package, k = 3),
    "\nAnd from my implementation:\n", myImp$clusters[48,])







#-------
# TESTS |
#-------
iN <- attributes(distD)$Size
iN2 <- (1 + sqrt(1 + 8 * length(distD))) / 2

iN <- (1 + sqrt(1 + 8 * length(distD))) / 2       # number of observations
lH <- rep(NA, iN-1)                               # list of merge heights
mC <- matrix(0, nrow = iN-1, ncol = iN)           # matrix of cluster assignments
mC[1,] <- seq(iN)                                 # start with iN clusters
mJ <- matrix(nrow = iN - 1, ncol = 2) 
rownames(mC) <- paste("Step", seq(iN-1))
mC[2,] <- c(rep(1,13), rep(2,20), rep(3, 17))

j <- 4
mD <- as.data.frame(t(combn(unique(mC[j-1,]), 2)))
mD$c1obs <- apply(mD, 1, function(x) list(which(mC[j-1,] == x[1])))
mD$c2obs <- apply(mD, 1, function(x) list(which(mC[j-1,] == x[2])))
mD$combos <- apply(mD, 1, function(x) expand.grid(unlist(x["c1obs"]), unlist(x["c2obs"])))
mD$index <- apply(mD, 1, function (x) apply(x$combos, 1, function (y) iN*(y[1]-1)-((y[1]*(y[1]-1))/2)+y[2]-y[1]))
mD$distances <- apply(mD, 1, function (x) distD[x$index])
mD$linkage <- apply(mD, 1, function (x) max(x$distances))
lH[j-1] <- min(mD$linkage)
vJoin <- mD[which(mD$linkage == min(mD$linkage)), c("V1", "V2")]
mJ[j-1,] <- unlist(vJoin)
mC[j,] <- mC[j-1,]
mC[j,which(mC[j-1,] == max(vJoin))] <- min(vJoin)
mC[j,] <- ifelse(mC[j,] > max(vJoin), mC[j,] - 1, mC[j,])


#------------
# APPROACH 2 |
#------------
distIndex <- function(iN, iI, iJ){
  iInd <- iN*(iI-1)-((iI*(iI-1))/2)+iJ-iI
  return(iInd)
}

mDist <- as.matrix(distD)                         # matrix of distances
iN <- nrow(mDist)
lH <- rep(NA, iN-1)                               # list of merge heights
mC <- matrix(0, nrow = iN, ncol = iN)             # matrix of cluster assignments
mC[1,] <- seq(iN)                                 # start with iN clusters
mJ <- matrix(nrow = iN, ncol = 2)                 # matrix of joins
rownames(mC) <- paste("Step", seq(iN))
# mC[2,] <- c(rep(1,13), rep(2,20), rep(3, 17))

mClosest <- matrix(nrow = iN, ncol = 3)
rownames(mClosest) <- seq(iN)
mClosest[,1] <- -seq(iN)
mClosest[,3] <- Inf
for(i in seq(iN)){
  for(j in seq(iN)){
    if(i != j){
      if(mDist[i,j] < mClosest[i,3]){
        mClosest[i,2] <- -j
        mClosest[i,3] <- mDist[i,j]
      } 
    }
  }
}

for(k in seq(1, iN-1)){
  minHeight <- min(mClosest[,3])
  c1 <- -mClosest[which(mClosest[,3] == minHeight)][1]
  c2 <- -mClosest[which(mClosest[,3] == minHeight)][2]
  
  lH[k] <- minHeight
  mJ[k,] <- c(c1, c2)
  
  # comlete linkage
  for(i in seq(iN)){
    if(mDist[c2, i] > mDist[c1, i]){
      mDist[c1, i] = mDist[i, c1] = mDist[c2, i]
    }
  }
  mDist[c1, c1] = 0
  
  for(i in seq(iN)){
    mDist[c2, i] = mDist[i, c2] = Inf
  }
  
  mClosest[c2,] <- Inf
  mClosest[, 3] <- Inf
  
  for(i in seq(iN)){
    for(j in seq(iN)){
      if(i != j){
        if(mDist[i,j] < mClosest[i,3]){
          mClosest[i,2] <- j
          mClosest[i,3] <- mDist[i,j]
        } 
      }
    }
  }
}
