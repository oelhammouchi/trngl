# as.trngl fails with non-square matrix

    Code
      as.trngl(UKMotor[-1, ])
    Condition
      Error in `as.trngl()`:
      ! `triangle` must square.
      i `triangle` has 6 rows and 7 columns
      i Non-square triangles are not yet supported.

# as.trngl fails with non-cumulative triangle

    Code
      as.trngl(triangle)
    Condition
      Error in `cumCheck()`:
      ! `triangle` must a cumulative
      i Row 7 is not increasing.

# subsetting trngl only works if result is non-defective

    Code
      UKMotor[1, 3] <- 10 * UKMotor[1, 3]
    Condition
      Error in `value[[3L]]()`:
      ! Could not complete assigment
      i Assignment would lead to defective triangle

# trngl pretty-printing works

    Code
      print(UKMotor)
    Output
      +-----------------------------------------+
      |  3511 6726 8992 10704 11763 12350 12690 |
      |  4001 7703 9981 11161 12117 12746       |
      |  4355 8287 10233 11755 12993            |
      |  4295 7750 9773 11093                   |
      |  4150 7897 10217                        |
      |  5102 9650                              |
      |  6283                                   |
      +-----------------------------------------+ 

