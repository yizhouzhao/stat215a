
# compute the matrix representation of a labeling
# given a d vector cl of labels, 
# return a d x d binary matrix C, where C_ij = 1 
# if i and j belong to the same cluster
ComembershipMatrix <- function(cl){
  # labels of different groups
  labels <- unique(cl)
  # sum comembership matrices among each group
  Reduce('+', lapply(labels, function(l){outer(cl==l, cl==l)}))
}

# calculate correlation between cluster labels cl1 and cl2
Corr <- function(cl1,cl2){
  # calculate <c1,c2>
  mat1 <- ComembershipMatrix(cl1)
  mat2 <- ComembershipMatrix(cl2)
  num <- sum(mat1*mat2) # numerator
  # calculate sqrt(<c1,c1>*<c2,c2>)
  denom <- sqrt(sum(mat1) * sum(mat2)) # denominator
  # cosine similarity given by <c1,c2> / sqrt(<c1,c1>*<c2,c2>)
  return(num / denom)
}