# ###### #
# Format #
# ###### #

# Description of the variables used to represent:
#   -Data
#   -Clusters
#   -Cluster Assignment
#   -Buckets

# ------- 
#    Data 
# ------- 
# Data is stored in data frame: records
# rows == record
# cols == feature

# ------------
#    Clusters 
# ------------
# Clusters are stored in a list. Each list item represents
# a cluster.

# ----------------------
#    Cluster Assignment 
# ----------------------
# Cluster assignments are stored in a list.
# Each element of the list stores a vector whose length
# equals the number of rows of the points being clustered.
# The elements of the list contain pregressive 
# representations of the clustering assignments over the 
# course of the clustering.



# ############ #
# Dependencies #
# ############ #

library(dplyr)



# ################## #
# Distance Functions #
# ################## #

distFun <- function(row1, row2, method="euclidean") {
  # Generic function for use with data frame rows
  # Given two rows and a method, compute distance
  f <- list(
    euclidean=function(row1, row2) {
      sqrt(sum((row1 - row2)**2))
    }
  )
  if (! method %in% names(f)) { stop("no method")}
  f[[method]](row1, row2)
}



# ####### #
# Buckets #
# ####### #

# ======================================================
# Given a data stream, partition the stream into buckets
# ======================================================
initBuckets <- function(records, ptsPerBucket, growthRate) {
  # records      == data.frame
  # ptsPerBucket == Int
  # growthRate   == Int
  numRecs <- nrow(records)
  numBuckets <- (
    ( log( 1 - (1-growthRate)*numRecs/ptsPerBucket) )
    / log(growthRate)
  )
  
  cumRecords <- function(bucket) {
    ptsPerBucket * (1 - growthRate**bucket) / (1 - growthRate)
  }
  
  buckets <- lapply(1:numBuckets, function(b) {
    start <- ifelse(b == 1, 1, cumRecords(b - 1) + 1)
    end <- cumRecords(b)
    list(records=records[start:end, , drop=FALSE])
  })
  return(buckets)
}



# ################## #
# Initialize Cluster #
# ################## #

# ================================
# Find initial points of a cluster
# ================================
initClusters <- function(records, nClusters, 
                         method="furthest", distType="euclidean") {
  f <- list(#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # ----------------------        
    # Select points randomly
    # ----------------------        
    random = function(records, k) {
      list(sample(1:nrow(records), k))
    },
    
    # ----------------------------------------
    # Select n points as far apart as possible
    # ----------------------------------------
    furthest = function(records, k) {
      # All combinatons of record pairs
      recordsPairs = data.frame(t(combn(1:nrow(records), 2)))
      names(recordsPairs) <- c("id1", "id2")
      
      # Distance between all pairs
      recordsPairs['dist'] <- (
        apply(recordsPairs, 1, function(r) {
          distFun(records[r["id1"], ], records[r["id2"], ], distType)
      }))
      
      # Sort by distance
      recordsPairs <- 
        recordsPairs[order(recordsPairs$dist, decreasing=TRUE), ]
      
      # Assign first two clusters as biggest distance
      assignVec <- rep(0, nrow(records))
      assignVec[as.numeric(recordsPairs[1, c("id1", "id2")])] <- c(1, 2)
      
      # Find the point with the largest minimum distance to all clusters
      while(max(assignVec) < k) {
        selection <- 
          returnPotentialDf(recordsPairs, assignVec) %>%
            group_by(candidates) %>%
            slice(which.min(dist)) %>%
            ungroup %>%
            slice(which.max(dist))
        assignVec[selection$candidates] <- max(assignVec) + 1
      }
      return(list(assignVec))
    }
  )#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Call the selected function to initialize the cluster
  return(f[[method]](records, nClusters))
}

# ==================================================
# Given distances and an assignment vector, return
# a data frame with cols: candidates, assigned, dist
# ==================================================
returnPotentialDf <- function(distByPairs, assignVec) {
  candidates <- which(assignVec == 0)
  assigned   <- which(assignVec != 0)
  
  df <- expand.grid(candidates, assigned)
  colnames(df) <- c("candidates", "assigned")
  df['dist'] <- unlist(apply(df, 1, function(r) {
    id <- sort(r)
    fltr <- ((id[1] == distByPairs['id1']) & 
             (id[2] == distByPairs['id2']))
    return(distByPairs[fltr, 'dist'])
  }))
  return(df)
}



# ##################### #
# Clustering Algorithms #
# ##################### #

# ======================================================
# Given a seeded cluster, partition the remaining points
# ======================================================



# ################## #
# Cluster Operations #
# ################## #

# ================================
# Return the centroid of a cluster
# ================================
getCentroid <- function(records, method="euclidean") {
  f <- list(#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    euclidean = function(records) {
      return(colSums(records) / nrow(records))
    }
  )#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
}





