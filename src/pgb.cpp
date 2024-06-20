#include <RcppArmadillo.h>

#include <cmath>
#include <progress.hpp>

#include "cli_progress_bar.h"
#include "config.h"

extern "C" {
void pgb_incr(void* pgb, int by);
}

void pgb_incr(void* pgb, int by) { ((Progress*)pgb)->increment(by); }