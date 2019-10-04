#include <RcppArmadillo.h>
#include <stack>
#include <iostream>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace std;

mat update(mat I, mat weight, int i, int n){
  vec index_left(n - i - 1);
  int k = 0;
    for(int j = 0; j < n; j++){
      if(I(2, j) == 0){
        index_left(k) = j;
        // find the index of subway that have not been visited
        k += 1;
      }
    }
  
    int min_index = index_left(0);
    for(int j = 1; j < n-i-1; j++){
      if(I(1, index_left(j)) < I(1, min_index))
        min_index = index_left(j);
      //find index with the minimum distance
  }

  double update_dist;
  for(int j = 0; j < n-i-1; j++){
    update_dist = I(1, min_index) + weight(min_index, index_left(j));
    if(update_dist < I(1, index_left(j))){
      I(1, index_left(j)) = update_dist;
      I(0, index_left(j)) = min_index;
      // update distance and is_visit by comparison.
    }
  }

  I(2, min_index) = 1;
  return I;
}

int output(mat I, int start, int end){
  int last_visit = end - 1;
  cout << "The path from " << start << " to " << end << " is: ";
  while(last_visit != start-1){
    cout << (last_visit+1) << "<--";
    last_visit = I(0, last_visit);
  }
  
  cout << (last_visit+1) << endl;
  
  double dist = I(1, end - 1);
  cout << "distance: " << dist;

  return 0;
}

// [[Rcpp::export]]
mat find_short_path(mat weight, int start){
  int n = weight.n_rows;
  
  mat I = zeros<mat>(3, n);         // information matrix
  for(int i = 0 ; i < n; i++){     
    I(0, i) = start - 1;            // initialize I
    I(1, i) = weight(start - 1, i);
  }
  I(2, start - 1) = 1;
  
  for(int i = 0; i < n-2; i++){
    I = update(I, weight, i, n);    // unpdate I by n-2 times
  }
  // output(I, start, end);
  return I;
}
