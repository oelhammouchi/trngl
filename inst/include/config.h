#pragma once
#include <Rcpp.h>
#include <omp.h>

#include <cmath>
#include <cstdlib>
#include <map>
#include <string>

#include "random.h"

namespace options {

enum SimType { SINGLE = 1, CALENDAR = 2, ORIGIN = 3 };
enum BootType { PARAM = 1, RESID = 2, PAIRS = 3 };
enum Dist { NORMAL = 1, GAMMA = 2, POISSON = 3 };
enum ResidType { STANDARD = 1, STUDENT = 2, LOG_NORM = 3 };

static const std::map<Rcpp::String, Dist> dist_mapping = {
    {Rcpp::String("normal"), NORMAL},
    {Rcpp::String("gamma"), GAMMA},
    {Rcpp::String("poisson"), POISSON}};

static const std::map<Rcpp::String, ResidType> resid_type_mapping = {
    {Rcpp::String("log-normal"), LOG_NORM},
    {Rcpp::String("standardised"), STANDARD},
    {Rcpp::String("studentised"), STUDENT}};

static const std::map<Rcpp::String, SimType> sim_type_mapping = {
    {Rcpp::String("single"), SINGLE},
    {Rcpp::String("origin"), ORIGIN},
    {Rcpp::String("calendar"), CALENDAR}};

static const std::map<Rcpp::String, BootType> boot_type_mapping = {
    {Rcpp::String("parametric"), PARAM},
    {Rcpp::String("residuals"), RESID},
    {Rcpp::String("pairs"), PAIRS}};

}  // namespace options