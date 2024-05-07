#pragma once
#include <Rcpp.h>
#include <trng/yarn2.hpp>

#include "config.h"

/**
 * @brief Singleton class managing a parallel RNG engine for
 * the entire package.
 *
 */
class TrnglRng {
   public:
    static inline TrnglRng& get() {
        static TrnglRng instance;
        return instance;
    }

    inline int seed() { return seed_; };
    inline int n_threads() { return n_threads_; };
    inline trng::yarn2& engine() { return engine_; };
    inline void set_seed(unsigned long seed) {
        seed_ = seed;
        engine_.seed(seed);
        thread_engines_ = MakeThreadEngines(*this);
    };
    inline void set_n_threads(int threads) { n_threads_ = threads; };
    inline std::vector<trng::yarn2>& thread_engines() {
        return thread_engines_;
    }

    static unsigned long InitSeed() {
        Rcpp::Environment base = Rcpp::Environment::namespace_env("base");
        Rcpp::Function get_option = base["getOption"];

        Rcpp::RObject res = get_option("trngl.seed");
        unsigned long seed;
        if (res.sexp_type() == INTSXP) {
            Rcpp::IntegerVector seed_opt(res);
            seed = static_cast<unsigned long>(std::abs(seed_opt[0]));
        } else {
            seed = 42;
        }

        return seed;
    }

    static int InitNumThreads() {
        bool not_set = false;
        static int n_threads;

        Rcpp::Environment base = Rcpp::Environment::namespace_env("base");
        Rcpp::Function get_option = base["getOption"];

        Rcpp::RObject trngl_threads = get_option("trngl.threads");
        if (trngl_threads != R_NilValue) {
            n_threads = Rcpp::as<int>(trngl_threads);
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

    static inline std::vector<trng::yarn2> MakeThreadEngines(
        TrnglRng& instance) {
        int n = omp_get_num_procs();
        std::vector<trng::yarn2> thread_engines(n);
        for (int i = 0; i < n; i++) {
            trng::yarn2 local_engine = instance.engine_;
            local_engine.jump(2 * (i / n));
            thread_engines.push_back(local_engine);
        }
        return thread_engines;
    }

   private:
    unsigned long seed_;
    int n_threads_;
    trng::yarn2 engine_;
    std::vector<trng::yarn2> thread_engines_;

    inline TrnglRng() {
        set_seed(InitSeed());
        set_n_threads(InitNumThreads());
    };
    inline ~TrnglRng(){};
    TrnglRng(const TrnglRng&) = delete;
    TrnglRng& operator=(const TrnglRng&) = delete;
};