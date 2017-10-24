#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
// Returns the entries of a numeric vector with the
// inputted column name
int with_name(NumericVector x, std::string name) {
  return x[name];
}

// [[Rcpp::export]]
// Returns the i-th column name of a numeric vector
std::string get_ith_name(NumericVector x, int i) {
  CharacterVector names = x.names();
  return(Rcpp::as<std::string>(names[i]));
}

// [[Rcpp::export]]
// Calculate the number of combinations (n choose k) in C++.
// Credit: https://stackoverflow.com/questions/9330915/
int nChoosek(int n,int k) {
  if (k > n) return 0;
  if (k * 2 > n) k = n-k;
  if (k == 0) return 1;
  
  int result = n;
  for( int i = 2; i <= k; ++i ) {
    result *= (n-i+1);
    result /= i;
  }
  return result;
}


// [[Rcpp::export]]
float ClusterCorrelationCPP(NumericVector x,
                            NumericVector y, int k) {
  // Calculate the correlation between two clustering assignments, x and y.
  // Arguments:
  //    x (as a numeric vector): the cluster assignments outputted
  //           from a kmeans() method
  //    y (as a numeric vector): the cluster assignments outputted
  //           from a kmeans() method
  //    k (as an int): number of clusters in the assignments
  // Outputs:
  //    the correlation between two clusterings, defined in Ben-Hur et. al.
  //    as <L1,L2> / sqrt(<L1,L1><L2,L2>)
  
  // variable to store the number of pairs in common
  int common = 0;

  // overall logic: for the first clustering assignment, find all pairs
  // of points that were clustered together, and then check if they were
  // also clustered together in the second clustering assignment
  
  // loop through all different cluster values
  for (int ki = 1; ki < k+1; ki++) {
    // get all points in that cluster in the first clustering assignment
    NumericVector currentCluster = x[x==ki];
    // loop through every pair in that cluster
    for (int i = 0; i < currentCluster.length()-1; i++) {
      for(int j = i+1; j < currentCluster.length(); j++) {
      // check if those points are also clustered together in the second
      // assignment  
        if(with_name(y, get_ith_name(currentCluster, i)) == \
           with_name(y, get_ith_name(currentCluster, j))){
          common += 1;
        }
      }
    } 
  }

  // calculate the total number of pairs within each clustering
  // (for the denominator)
     int total1 = 0;
     int total2 = 0;
     for (int ki = 1; ki < k+1; ki++) {
       NumericVector x_subset = x[x==ki];
       NumericVector y_subset = y[y==ki];
       total1 = total1 + nChoosek(x_subset.length(), 2);
       total2 = total2 + nChoosek(y_subset.length(), 2);
     }
     // divide by sqrt of total1 and total2 separately
     // to prevent integer overflow
     return((float) common / (std::sqrt(total1)*std::sqrt(total2))); 
}