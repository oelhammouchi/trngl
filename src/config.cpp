#include "config.h"

#include <omp.h>

#include <cctype>
#include <cmath>
#include <cstdlib>

#include "random.h"

//' @export
// [[Rcpp::export]]
void setCBThreads(int n_threads) {
    ClaimsBootConfig::get().set_n_threads(n_threads);
}

//' @export
// [[Rcpp::export]]
int getCBThreads() { return ClaimsBootConfig::get().n_threads(); }

//' @export
// [[Rcpp::export]]
void setCBSeed(int seed) { ClaimsBootConfig::get().set_seed(seed); }

//' @export
// [[Rcpp::export]]
int getCBSeed() { return ClaimsBootConfig::get().seed(); }