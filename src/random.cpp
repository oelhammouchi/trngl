#include "random.h"

#include <Rcpp.h>
#include <omp.h>

#include <boost/math/distributions/gamma.hpp>
#include <trng/gamma_dist.hpp>
#include <trng/normal_dist.hpp>
#include <trng/poisson_dist.hpp>
#include <trng/uniform01_dist.hpp>

extern "C" {
double rnorm(double mean, double sd);
double rgamma(double shape, double scale);
double runif();
double rpois(double mean);
}

double rnorm(double mean, double sd) {
    ClaimsBootRng& rng = ClaimsBootRng::get();
    int i_thread = omp_get_thread_num();
    rng.engine().jump(2 * (i_thread / rng.n_threads()));
    trng::normal_dist<double> dist(mean, sd);
    double n = dist(rng.engine());
    return n;
}

double rgamma(double shape, double scale) {
    ClaimsBootRng& rng = ClaimsBootRng::get();
    int i_thread = omp_get_thread_num();
    rng.engine().jump(2 * (i_thread / rng.n_threads()));
    trng::uniform01_dist<double> dist;
    double u = dist(rng.engine());
    return R::qgamma(u, shape, scale, true, false);
}

double runif() {
    ClaimsBootRng& rng = ClaimsBootRng::get();
    int i_thread = omp_get_thread_num();
    rng.engine().jump(2 * (i_thread / rng.n_threads()));
    trng::uniform01_dist<double> dist;
    double u = dist(rng.engine());
    return u;
}

double rpois(double mean) {
    ClaimsBootRng& rng = ClaimsBootRng::get();
    int i_thread = omp_get_thread_num();
    rng.engine().jump(2 * (i_thread / rng.n_threads()));
    trng::uniform01_dist<double> dist;
    double u = dist(rng.engine());
    return R::qpois(u, mean, true, false);
}