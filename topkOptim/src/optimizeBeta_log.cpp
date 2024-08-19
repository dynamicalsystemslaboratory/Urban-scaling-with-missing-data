
#include <Rcpp.h>
using namespace Rcpp;



//' Set of functions that maximize the regression slope for the OLS estimate
//' of univariate linear regression parameters. These functions were written
//' to handle the special case where 
//' 1. only the k-largest values of the target are known,
//' 2. the sum of all target values is known, and
//' 3. both the predictor and the target are positive integers excluding zero



//' Loss function
//'
// [[Rcpp::export]]
NumericVector lossFn_cpp(
   NumericVector z,
   int n,
   NumericVector ln_xr,
   double sum_ln_x,
   NumericVector k
) {
 NumericVector res (z.length(), NA_REAL);
 double ln_zi = 0;
 double ln_zi_aux = 0;
 double v1 = 0;
 double v2 = 0;
 for(int i = 0; i < z.length(); i++){
   if(z[i] + 1 > k[i]){
     continue;
   } else {
     ln_zi = log(z[i]);
     ln_zi_aux = log(z[i] + 1);
     v1 = n * ln_xr[i] * (ln_zi_aux - ln_zi);
     v2 = sum_ln_x * (ln_zi - ln_zi_aux);
     res[i] = v1 + v2;
   }
 }
 return(res);
}


//' Report progress
void reportProgress(double x){
  int barWidth = 70;
  Rcout << "[";
  int pos = barWidth * x;
  for (int i = 0; i < barWidth; ++i) {
    if (i < pos) Rcout << "=";
    else if (i == pos) Rcout << ">";
    else Rcout << " ";
  }
  Rcout << "] " << int(x * 100.0) << " %\r";
  Rcout.flush();
}

//' Optimize
//' 
//' @param xr The x-values not in the top-k.
//' @param x The x-values.
//' @param z0 The vector of starting values for the optimization.
//' @param steps The number of optimization steps.
//' @param maxZ The maximum value of elements of z.
//' @param maximize Whether to maximize.
//' @param verbose Whether to report progress.
//' 
//' @description
//' Using the OLS equation for regression parameters, this function
//' maximizes or minimizes the regression slope subject to the following
//' conditions
//' 1. The predictor is completely observed
//' 2. Only the k-largest values of the target are observed
//' 3. The sum of the entire target variable is observed
//' 
//' This function assumes that the domain of both x and z are non-negative 
//' integers excluding zero.
//' 
//' If the function is called with `maximum != 1`, it will return the solution
//' that minimizes the regression slope.
//' 
// [[Rcpp::export]]
NumericVector optim(
    NumericVector xr,
    NumericVector x,
    NumericVector z0,
    int steps,
    NumericVector maxZ,
    int maximize = 1,
    int verbose = 0
) {
  NumericVector ln_xr (xr.length());
  for(int i = 0; i < xr.length(); i++){
    ln_xr[i] = log(xr[i]);
  }
  double sum_ln_x = 0;
  for(int i = 0; i < x.length(); i++){
    sum_ln_x += log(x[i]);
  }
  NumericVector maxZ_vec (xr.length());
  if(maxZ.length() == 1){
    for(int i = 0; i < xr.length(); i++){
      maxZ_vec[i] = maxZ[0];
    }
  } else if(maxZ.length() == xr.length()) {
    maxZ_vec = maxZ;
  } else {
    throw std::invalid_argument("Maximum z should be length 1 or length z");
  }
  double sgn = 1;
  if(maximize != 1){
    sgn = -1;
  }
  int n = x.length();
  int ind = 0;
  NumericVector z = clone(z0);
  NumericVector stepLoss (z.length());
  for(int i = 0; i < steps; i++){
    
    // Calculate loss
    stepLoss = lossFn_cpp(z, n, ln_xr, sum_ln_x, maxZ_vec);
    for(int j = 0; j < z.length(); j++){
      stepLoss[j] = sgn * stepLoss[j];
    }
    
    // Which.max() with NA handling
    int hasValue = 0;
    double lossMax = 0;
    for(int j = 0; j < z.length(); j++){
      if(hasValue == 0){
        if(!NumericVector::is_na(stepLoss[j])){
          ind = j;
          lossMax = stepLoss[j];
          hasValue = 1;
        } else {
          continue;
        }
      } else {
        if(stepLoss[j] > lossMax){
          ind = j;
          lossMax = stepLoss[j];
        }
      }
    }
    if(hasValue == 0){
      Rcout << "Not possible given constraints. Returning the partial solution." << "\n";
      break;
    }
    
    // Update z
    z[ind] += 1;
    if(verbose == 1){
      reportProgress(double(i) / double(steps));
    }
    
  }
  return(z);
}



//' Optimize on Grouped Data
//'
//' @param x_data See details.
//' @param index See details.
//' @param maximize Whether to maximize.
//' @param verbose Whether to report progress.
//'
//' @description
//' A version of `optim()` designed to be used when the data are grouped.
//'
//' @details
//' This function optimizes the OLS regression slope subject to the same conditions as
//' `optim()`, but allows for the data to be grouped such that 
//' each group has its own optimization constraints and maximum number of 
//' optimization steps. The expectation is that the data represents cities grouped by state.
//' 
//' The \code{x_data} argument should be a dataframe containing city populations.
//' This data frame should have columns called \code{state}, the state in which
//' that city is located; \code{x}, the city population; and \code{isTopK},
//' `1` when that city is in the top-K and `0` otherwise.
//' 
//' The \code{index} object should be a dataframe containing the optimization
//' parameters for each state, to be used as a look-up table. This dataframe 
//' should have columns called \code{state}, \code{steps}, the number of recovered
//' firearms in that state; and \code{maxZ}, the maximum number of firearms that could be 
//' recovered in a non-top-K city.
//'
// [[Rcpp::export]]
NumericVector optim_aggregate(
    DataFrame x_data,
    DataFrame index,
    int maximize = 1,
    int verbose = 0
) {

  // Construct vectors containing information about the optimization status
  IntegerVector xData_isTopK = x_data["isTopK"];
  int xr_len = 0;
  for(int i = 0; i < x_data.nrows(); i++){
    if(xData_isTopK[i] == 0){
      xr_len += 1;
    }
  }
  NumericVector xr (xr_len);
  NumericVector xr_steps (xr_len);
  NumericVector xr_maxZ (xr_len);
  CharacterVector xr_state (xr_len);

  // Use the index as a look-up table
  NumericVector index_steps = index["steps"];
  NumericVector index_maxZ = index["maxZ"];
  CharacterVector index_state = index["state"];
  NumericVector xData_x = x_data["x"];
  CharacterVector xData_state = x_data["state"];
  int k = 0;
  for(int i = 0; i < x_data.nrows(); i++){
    if(xData_isTopK[i] == 0){
      xr[k] = xData_x[i];
      xr_state[k] = xData_state[i];
      for(int j = 0; j < index.nrows(); j++){
        if(index_state[j] == xData_state[i]){
          xr_steps[k] = index_steps[j];
          xr_maxZ[k] = index_maxZ[j];
          break;
        }
      }
      k += 1;
    }
  }
  
  // Precompute log and optimization direction
  NumericVector ln_xr (xr.length());
  for(int i = 0; i < xr.length(); i++){
    ln_xr[i] = log(xr[i]);
  }
  NumericVector x = x_data["x"];
  double sum_ln_x = 0;
  for(int i = 0; i < x.length(); i++){
    sum_ln_x += log(x[i]);
  }

  // Initialize the solution as all ones and
  // a vector to check whether updating an element of z is valid
  NumericVector z (xr_len, 1L);
  IntegerVector z_isValid (xr_len, 1L);
  IntegerVector z_count (index.nrows());

  // Initialize optimization parameters
  int n = x.length();
  int update_ind = 0;
  int steps = sum(index_steps);
  NumericVector stepLoss (z.length());
  double sgn = 1;
  if(maximize != 1){
    sgn = -1;
  }
  
  // Optimize
  for(int i = 0; i < steps; i++){

    // Check whether updating each element of z is valid
    // Elements of z can't be larger than the associated maxZ
    // Elements of z must have guns left in the associated state
    for(int j = 0; j < z.length(); j++){
      if(z[j] == 0){
        continue;
      }
      if(z[j] + 1 > xr_maxZ[j]){
        z_isValid[j] = 0;
      }
      for(int k = 0; k < index.nrows(); k++){
        if(index_state[k] == xr_state[j]){
           if(index_steps[k] <= z_count[k]){
             z_isValid[j] = 0;
             break;
           }
        }
      }
    }
    
    // Calculate loss
    stepLoss = lossFn_cpp(z, n, ln_xr, sum_ln_x, xr_maxZ);
    for(int j = 0; j < z.length(); j++){
      if(z_isValid[j] == 1){
        stepLoss[j] = sgn * stepLoss[j];
      } else{
        stepLoss[j] = NA_REAL;
      }
    }

    // Which.max() with NA handling
    int hasValue = 0;
    double lossMax = 0;
    for(int j = 0; j < z.length(); j++){
      if(hasValue == 0){
        if(!NumericVector::is_na(stepLoss[j])){
          update_ind = j;
          lossMax = stepLoss[j];
          hasValue = 1;
        } else {
          continue;
        }
      } else {
        if(stepLoss[j] > lossMax){
          update_ind = j;
          lossMax = stepLoss[j];
        }
      }
    }
    if(hasValue == 0){
      Rcout << "Not possible given constraints. Returning the partial solution." << "\n";
      break;
    }

    // Update z and the index
    z[update_ind] += 1;
    for(int k = 0; k < index.nrows(); k++){
      if(index_state[k] == xr_state[update_ind]){
        z_count[k] += 1;
        break;
      }
    }
    if(verbose == 1){
      reportProgress(double(i) / double(steps));
    }
  
  }
  return(z);

}


