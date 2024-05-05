# coercing bad triangle results in abort [plain]

    Code
      res <- odpParamBoot(triangle, dist = "gamma", progress = FALSE, n_boot = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidBoot(triangle, progress = FALSE, n_boot = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [ansi]

    Code
      res <- odpParamBoot(triangle, dist = "gamma", progress = FALSE, n_boot = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidBoot(triangle, progress = FALSE, n_boot = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [unicode]

    Code
      res <- odpParamBoot(triangle, dist = "gamma", progress = FALSE, n_boot = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidBoot(triangle, progress = FALSE, n_boot = 100)
    Condition
      Error in `value[[3L]]()`:
      ! Failed to coerce `trngl` to 'trngl'

# coercing bad triangle results in abort [fancy]

    Code
      res <- odpParamBoot(triangle, dist = "gamma", progress = FALSE, n_boot = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

---

    Code
      res <- odpResidBoot(triangle, progress = FALSE, n_boot = 100)
    Condition
      [1m[33mError[39m in `value[[3L]]()`:[22m
      [1m[22m[33m![39m Failed to coerce `trngl` to 'trngl'

