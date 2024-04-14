#pragma once
#include <Rcpp.h>
#include <omp.h>

#include <cmath>
#include <cstdlib>
#include <map>
#include <string>

class ClaimsBootConfig {
   public:
    static ClaimsBootConfig& get() {
        static ClaimsBootConfig instance;
        return instance;
    };
    static inline int n_threads() { return n_threads_; };
    static inline void set_n_threads(int n_threads) { n_threads_ = n_threads; };
    static inline unsigned long seed() { return seed_; }
    static inline void set_seed(unsigned long seed) { seed_ = seed; }

   private:
    ClaimsBootConfig(){};
    ~ClaimsBootConfig(){};

    static inline unsigned long InitSeed() {
        Rcpp::Environment base = Rcpp::Environment::namespace_env("base");
        Rcpp::Function get_option = base["getOption"];

        Rcpp::RObject res = get_option("claimsBoot.seed");
        unsigned long seed;
        if (res.sexp_type() == INTSXP) {
            Rcpp::IntegerVector seed_opt(res);
            seed = static_cast<unsigned long>(std::abs(seed_opt[0]));
        } else {
            seed = 42;
        }

        return seed;
    }
    static inline int InitNumThreads() {
        bool not_set = false;
        static int n_threads;

        Rcpp::Environment base = Rcpp::Environment::namespace_env("base");
        Rcpp::Function get_option = base["getOption"];

        Rcpp::RObject claimsboot_threads = get_option("claimsBoot.threads");
        if (claimsboot_threads != R_NilValue) {
            n_threads = Rcpp::as<int>(claimsboot_threads);
        } else {
            char* OMP_NUM_THREADS = std::getenv("OMP_NUM_THREADS");
            if (OMP_NUM_THREADS == nullptr) {
                not_set = true;
            } else {
                try {
                    n_threads = std::stoi(std::string(OMP_NUM_THREADS));
                } catch (std::invalid_argument& e) {
                    not_set = true;
                }
            }
        }

        if (not_set) {
            n_threads = std::floor(0.5 * omp_get_num_procs());
            n_threads = std::min(omp_get_thread_limit(), n_threads);
            n_threads = std::min(omp_get_max_threads(), n_threads);
        }

        return n_threads;
    }

    static inline int n_threads_ = InitNumThreads();
    static inline unsigned long seed_ = InitSeed();
    static inline Rcpp::Environment base =
        Rcpp::Environment::namespace_env("base");
    static inline Rcpp::Function get_option = base["getOption"];
};

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