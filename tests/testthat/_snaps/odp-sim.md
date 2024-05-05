# coercing bad triangle results in abort [plain]

    Code
      res <- odpParamSim(triangle, "single", dist = "gamma", progress = FALSE,
        n_boot = 100, n_sim = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidSim(triangle, "single", progress = FALSE, n_boot = 100, n_sim = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [ansi]

    Code
      res <- odpParamSim(triangle, "single", dist = "gamma", progress = FALSE,
        n_boot = 100, n_sim = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidSim(triangle, "single", progress = FALSE, n_boot = 100, n_sim = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [unicode]

    Code
      res <- odpParamSim(triangle, "single", dist = "gamma", progress = FALSE,
        n_boot = 100, n_sim = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidSim(triangle, "single", progress = FALSE, n_boot = 100, n_sim = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [fancy]

    Code
      res <- odpParamSim(triangle, "single", dist = "gamma", progress = FALSE,
        n_boot = 100, n_sim = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidSim(triangle, "single", progress = FALSE, n_boot = 100, n_sim = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

# pretty printing sim result works

    Code
      print(res)
    Output
      -- ODP bootstrap simulation test -----------------------------------------------
      * bootstrap iterations: 100
      * simulation iterations: 100
      * status:
       

