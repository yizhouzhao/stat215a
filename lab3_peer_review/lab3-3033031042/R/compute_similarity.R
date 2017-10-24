# This file provides the function definition to compute the
# correlation score between two clustering assignments.

cluster_correlation <- function(x, y) {
  # Note that this assumes that x and y are already filtered down to the
  # entries at the intersection of the two cluster assignments.
  #
  # Arguments:
  #   x: the cluster assignments outputted from a kmeans() method
  #   y: the cluster assignments outputted from a kmeans() method
  # Returns:
  #   the correlation between two clusterings, defined in Ben-Hur et. al.
  #   as <L1,L2> / sqrt(<L1,L1><L2,L2>)
  
  # variable to store the number of pairs in common
  common = 0
  
  # infer the number of clusters
  k = length(unique(x))
  
  ### overall logic: for the first clustering assignment, find all pairs
  ### of points that were clustered together, and then check if they were
  ### also clustered together in the second clustering assignment
  # loop through all different cluster values
  for(ki in 1:k) {
    # get all points in that cluster in the first clustering assignment
    current_cluster <- x[x==ki]
    # loop through every pair in that cluster
    for(i in 1:(length(current_cluster)-1)) { #since i<j, loop thru length-1
      for(j in (i+1):length(current_cluster)) {
        # check if those points are also clustered together in the second
        # assignment
        if(y[names(current_cluster[i])]==y[names(current_cluster[j])]){
          # if so then increment the count
          common = common + 1
        }
      }
    }
  }
  
  # calculate the total number of pairs within each clustering
  # (for the denominator)
  total1 = 0
  total2 = 0
  for(ki in 1:k){
    total1 = total1 + choose(length(x[x==ki]),2)
    total2 = total2 + choose(length(y[y==ki]),2)
  }
  return(common / sqrt(total1*total2))
}