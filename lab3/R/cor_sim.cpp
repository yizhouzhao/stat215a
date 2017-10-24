#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int sumProduct(IntegerVector A, IntegerVector B){
  /*
  A memory-efficient function to get the dot product of cluster A and cluster.
  Input:
      two vectors indicating the clusters of the data
  Output:
      An integer indicating the dot product
  */
  if (A.size() != B.size()){
    Rcpp::Rcerr<<"Error: wrong length of vector/data.frame A,B";
  }
  
  //Here, we have C_{i,i} = 0 C_{i,j} = C_{j,i}
  int total = 0;
  for (int i = 0; i < A.size(); i++){
    for(int j = i+1; j < A.size(); j++){
      if(A[i] == A[j] && B[i] == B[j]){
        total += 1;
      }
    }
  } 
  return 2*total;
}

// [[Rcpp::export]]
int getCoeff(IntegerVector A,IntegerVector B, int coef1, int coef2){
   /*
   A function to the the Coefficient Indicator N_{i,j} i,j \in {0,1} in Bur's paper
   Input:
       Two vectors indicating the clusters, and coef1, coef2 in {0,1}
   Output:
       An integer
   */
  if (A.size() != B.size()){
    Rcpp::Rcerr<<"Error: wrong length of vector/data.frame A,B";
  }
  int total = 0;
  for(int i = 0; i < A.size(); i++){
    for(int j = i+1; j < A.size(); j++){
      if ((A[i] == A[j]) == coef1 && (B[i] == B[j]) == coef2){
        total += 1;
      }
    }
  }
  if(coef1 == 0 && coef2 == 0){
    return 2*total + A.size(); 
  }
  return 2*total;
}

// [[Rcpp::export]]
double corSim(IntegerVector A,IntegerVector B){
   /*A function to get the correlation similarity of two clusters*/
  return sumProduct(A,B)/sqrt(sumProduct(A,A))/sqrt(sumProduct(B,B));
}

// [[Rcpp::export]]
double matchCoeff(IntegerVector A,IntegerVector B){
   /*A function to get the matching coeff of two clusters*/
  return (getCoeff(A,B,0,0) + getCoeff(A,B,1,1)+0.0)/(getCoeff(A,B,0,0)+
          getCoeff(A,B,1,1)+getCoeff(A,B,1,0)+getCoeff(A,B,0,1));
}

// [[Rcpp::export]]
double jaccardCoeff(IntegerVector A,IntegerVector B){
    /*A function to get the Jaccard coef of two clusters*/
  return (getCoeff(A,B,1,1)+0.0)/(getCoeff(A,B,1,1)+getCoeff(A,B,1,0)+getCoeff(A,B,0,1));
}