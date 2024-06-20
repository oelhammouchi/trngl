#pragma once

#ifdef _OPENMP
    #include <omp.h>
#else
    #define omp_get_num_threads() 1
    #define omp_get_thread_num() 0
    #define omp_get_max_threads() 1
    #define omp_get_thread_limit() 1
    #define omp_get_num_procs() 1
#endif

#include <Rcpp.h>

#include <cmath>
#include <cstdlib>
#include <map>
#include <string>

#include "random.h"

namespace options {

enum SimType { kSingle = 1, kCalendar = 2, kOrigin = 3 };
enum BootType { kParam = 1, kResid = 2, kPairs = 3 };
enum Dist { kNormal = 1, kGamma = 2, kPoisson = 3 };
enum ResidType { kStandard = 1, kStudent = 2, kLogNorm = 3 };
enum Status { kSuccess = 0, kFailure = 1 };

static const std::map<Rcpp::String, Dist> dist_mapping = {
    {Rcpp::String("normal"), kNormal},
    {Rcpp::String("gamma"), kGamma},
    {Rcpp::String("poisson"), kPoisson}};

static const std::map<Rcpp::String, ResidType> resid_type_mapping = {
    {Rcpp::String("log-normal"), kLogNorm},
    {Rcpp::String("standardised"), kStandard},
    {Rcpp::String("studentised"), kStudent}};

static const std::map<Rcpp::String, SimType> sim_type_mapping = {
    {Rcpp::String("single"), kSingle},
    {Rcpp::String("origin"), kOrigin},
    {Rcpp::String("calendar"), kCalendar}};

static const std::map<Rcpp::String, BootType> boot_type_mapping = {
    {Rcpp::String("parametric"), kParam},
    {Rcpp::String("residuals"), kResid},
    {Rcpp::String("pairs"), kPairs}};

}  // namespace options