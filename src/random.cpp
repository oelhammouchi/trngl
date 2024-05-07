#include "random.h"

#include <Rcpp.h>
#include <boost/math/distributions/poisson.hpp>
#include <trng/gamma_dist.hpp>
#include <trng/normal_dist.hpp>
#include <trng/poisson_dist.hpp>
#include <trng/uniform01_dist.hpp>

#include "config.h"

extern "C" {
double rnorm(double mean, double sd);
double rgamma(double shape, double scale);
double runif();
double rpois(double mean);
}

double rnorm(double mean, double sd) {
    TrnglRng& rng = TrnglRng::get();
    int i_thread = omp_get_thread_num();
    trng::normal_dist<double> dist(mean, sd);
    return dist(rng.thread_engines().at(i_thread));
}

double rgamma(double shape, double scale) {
    TrnglRng& rng = TrnglRng::get();
    int i_thread = omp_get_thread_num();
    trng::uniform01_dist<double> dist;
    double u = dist(rng.thread_engines().at(i_thread));
    return R::qgamma(u, shape, scale, true, false);
}

double runif() {
    TrnglRng& rng = TrnglRng::get();
    int i_thread = omp_get_thread_num();
    trng::uniform01_dist<double> dist;
    double u = dist(rng.thread_engines().at(i_thread));
    return u;
}

double rpois(double mean) {
    TrnglRng& rng = TrnglRng::get();
    int i_thread = omp_get_thread_num();
    trng::uniform01_dist<double> dist;
    double u = dist(rng.thread_engines().at(i_thread));
    boost::math::poisson_distribution poisson(mean);
    return boost::math::quantile(poisson, u);
}