# This file compares the timing of the RCpp versus R versions of
# the correlation similarity computations.

# load in libraries
library('Rcpp')
library('microbenchmark')
library('dendextend') # for Fowlkes-Mallows correlation index calculation

# load in source files for the R and C++ similarity compputation functions
sourceCpp('cpp_similarity.cpp')
source("compute_similarity.R")

# load in data
load("../data/lingBinary.Rdata")

m <- 0.2 # sampling proportion, must be between 0.2 and 0.8
k <- 3 # number of clusters

# draw two subsamples of the data set
subsample1 <- lingBinary[sample(1:nrow(lingBinary),
                               size=m*nrow(lingBinary),
                               replace=FALSE),c(7:ncol(lingBinary))]
subsample2 <- lingBinary[sample(1:nrow(lingBinary),
                                size=m*nrow(lingBinary),
                                replace=FALSE),c(7:ncol(lingBinary))]

# perform k-means
kmeans1 <- kmeans(x=subsample1, centers=k)
kmeans2 <- kmeans(x=subsample2, centers=k)

# find intersection, and the cluster assignments of the intersection
intersection <- intersect(rownames(subsample1), rownames(subsample2))
kmeans_assignments1 <- setNames(as.vector(kmeans1$cluster[intersection]),
                                names(kmeans1$cluster[intersection]))
kmeans_assignments2 <- setNames(as.vector(kmeans2$cluster[intersection]),
                                names(kmeans2$cluster[intersection]))

# Test our R version vs. our C++ version vs. an openly-available package.
microbenchmark(cluster_correlation(kmeans_assignments1, kmeans_assignments2),
               ClusterCorrelationCPP(kmeans_assignments1, kmeans_assignments2,
                                     k),
               FM_index(kmeans_assignments1, kmeans_assignments2, FALSE)[1],
               times=1)

# All the answers are the same:
cluster_correlation(kmeans_assignments1, kmeans_assignments2)
ClusterCorrelationCPP(kmeans_assignments1, kmeans_assignments2, k)
FM_index(kmeans_assignments1, kmeans_assignments2, FALSE)[1]