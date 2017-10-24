# load data
load("data/lingBinary.RData")

# select cols with questions to create binary matrix
ling <- lingBinary %>%
  select(contains("Q"))
ling <- matrix(as.numeric(unlist(ling)), 
               nrow = nrow(ling))

# calculate similarity_measure doing KMeans N times  
# with k clusters on a fraction m of the data
calc_sim <- function(data, similarity_measure, m, k, N){
  # number of datapoints
  n <- nrow(data)
  # size of subset to take
  q <- round(m*n)
  
  # store results in a similarity vector
  S <- matrix(nrow = N)
  
  for (i in seq(N)){
    # get subsample of fraction m on dataset
    ind1 <- sort(sample(n, q))
    sub1 <- data[ind1, ]
      
    # get another subsample
    ind2 <- sort(sample(n, q))
    sub2 <- data[ind2, ]
      
    # calculate cluster labels with k-means
    L1   <- kmeans(sub1, k)$cluster
    L2   <- kmeans(sub2, k)$cluster
      
    # restrict labels to intersection of indices
    ind12  <- intersect(ind1, ind2)
    cl1 <- L1[which(ind1 %in% ind12)]
    cl2 <- L2[which(ind2 %in% ind12)]
      
    # calculate similarity measure between labellings
    S[i] <- similarity_measure(cl1, cl2)
  }
  
  return(S)
}