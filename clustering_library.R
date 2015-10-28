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

initializeBuckets <- function(records, ptsPerBucket, growthRate) {
  # records      == data.frame
  # ptsPerBucket == Int
  # growthRate   == Int
  numRecs <- nrow(records)
  numBuckets <- (log( 1 - (1-growthRate)*numRecs) ) / log(growthRate) - 1
  return(numBuckets)
}