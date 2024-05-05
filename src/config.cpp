#include "random.h"

//' trngl configuration functions
//'
//' trngl uses an internal threadsafe random number generator. These
//' functions can be used to configure it.
//'
//' @param n_threads Number of threads to use
//' @param seed Seed for the random number generator
//' @name config
//' @export
// [[Rcpp::export]]
void setTrnglThreads(int n_threads) {
    TrnglRng::get().set_n_threads(n_threads);
}

//' @rdname config
//' @export
// [[Rcpp::export]]
int getTrnglThreads() { return TrnglRng::get().n_threads(); }

//' @rdname config
//' @export
// [[Rcpp::export]]
void setTrnglSeed(int seed) { TrnglRng::get().set_seed(seed); }

//' @rdname config
//' @export
// [[Rcpp::export]]
int getTrnglSeed() { return TrnglRng::get().seed(); }