#pragma once
#include <config.h>

#include <trng/yarn2.hpp>

/**
 * @brief Singleton class managing a parallel RNG engine for
 * the entire package.
 *
 */
class ClaimsBootRng {
   public:
    static inline ClaimsBootRng& get() {
        static ClaimsBootRng instance;
        if (first_call_) {
            instance.set_seed(ClaimsBootConfig::get().seed());
            instance.n_threads_ = ClaimsBootConfig::get().n_threads();
            first_call_ = false;
        }
        return instance;
    }

    inline int seed() { return seed_; };
    inline int n_threads() { return n_threads_; };
    inline trng::yarn2& engine() { return engine_; };
    inline void set_seed(unsigned long seed) {
        seed_ = seed;
        engine_.seed(seed);
    };
    inline void set_n_threads(int threads) { n_threads_ = threads; };

   private:
    static inline bool first_call_ = true;
    unsigned long seed_;
    int n_threads_;
    trng::yarn2 engine_;

    inline ClaimsBootRng(){};
    inline ~ClaimsBootRng(){};
    ClaimsBootRng(const ClaimsBootRng&) = delete;
    ClaimsBootRng& operator=(const ClaimsBootRng&) = delete;
};