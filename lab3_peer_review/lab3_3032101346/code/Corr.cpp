#include <Rcpp.h>
using namespace Rcpp;

// calculate inner product between cluster labels
// assumes cl1 and cl2 have the same length
double InnerProdCPP(Rcpp::NumericVector cl1, Rcpp::NumericVector cl2){
  int d  = cl1.size();
  int tot = 0;
  for(int i = 0; i < d; i++){
    for(int j = 0; j < d; j++){
      // add one if i and j belong to the same group under both labelings
      tot += (cl1[i] == cl1[j]) * (cl2[i] == cl2[j]);
    }
  }
  return(tot);
}

// calculate correlation between cluster labels 
// [[Rcpp::export]]
double CorrCPP(Rcpp::NumericVector cl1, Rcpp::NumericVector cl2){
  // calculate <c1,c2>
  double num = InnerProdCPP(cl1, cl2);
  // calculate sqrt(<c1,c1>*<c2,c2>)
  double denom = sqrt(InnerProdCPP(cl1, cl1) * InnerProdCPP(cl2, cl2));
  // cosine similarity given by <c1,c2> / sqrt(<c1,c1>*<c2,c2>)
  return num / denom;
}