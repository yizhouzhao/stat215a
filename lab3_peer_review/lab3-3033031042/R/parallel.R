# This file runs Ben-Hur et. al.'s "model explored algorithm",
# with similarity scores implemented in R and with the computation
# parallelized via "foreach" loops.

# load libraries
library(foreach)
library(doParallel)

# load in data
load("../data/lingBinary.Rdata")
source("compute_similarity.R")

# set the number of cores to use manually
nCores <- 4
registerDoParallel(nCores) 

# set other limitations on computations
k_max <- 10 # maximum number of clusters to try
nSub <- 100 # number of repeated iterations for each cluster size
m <- 0.5 # sampling proportion, must be between 0.2 and 0.8

# perform (in parallel) the computation
result <- foreach(k = 2:k_max) %:%
  foreach(i = 1:nSub) %dopar% {
  cat('Starting ', i, 'th job.\n', sep = '')
  # sample the first population
  subsample1 <- lingBinary[sample(1:nrow(lingBinary),
         size=m*nrow(lingBinary),
         replace=FALSE),c(7:ncol(lingBinary))]
  # sample the second population
  subsample2 <- lingBinary[sample(1:nrow(lingBinary),
         size=m*nrow(lingBinary),
         replace=FALSE),c(7:ncol(lingBinary))]
  # perform k-means on the two subsamples
  kmeans1 <- kmeans(x=subsample1, centers=k)
  kmeans2 <- kmeans(x=subsample2, centers=k)
  
  # find their intersection and then perform cluster correlation
  intersection <- intersect(rownames(subsample1), rownames(subsample2))
  correl <- cluster_correlation(kmeans1$cluster[intersection],
                                kmeans2$cluster[intersection])
  correl # this will become part of the output object
}

# put results from foreach into a dataframe
result_df <- data.frame(results = unlist(result))

# append column onto result_df that records what k value the result was for
result_df <- cbind(rep(2:k_max, each = nSub), result_df)
names(result_df) <- c("k", "similarity")

# save results to a csv
write.csv(result_df, "results_foreach.csv")