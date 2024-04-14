#include <RcppArmadillo.h>
#include <omp.h>

#include <cmath>
#include <progress.hpp>

#include "cli_progress_bar.h"

extern "C" {
void pgb_incr(void* pgb, int by);
}

void pgb_incr(void* pgb, int by) {
    if (!Progress::check_abort()) {
        ((Progress*)pgb)->increment(by);
    }
}