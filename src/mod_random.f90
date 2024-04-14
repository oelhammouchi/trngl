module mod_random

  use, intrinsic :: iso_c_binding

  implicit none

  interface
    function rnorm(mean, sd) result(sample) bind(c)
      import
      real(c_double), value :: mean
      real(c_double), value :: sd
      real(c_double) :: sample
    end function rnorm

    function rgamma(shape, scale) result(sample) bind(c)
      import
      real(c_double), value :: shape
      real(c_double), value :: scale
      real(c_double) :: sample
    end function rgamma

    function rpois(lambda) result(sample) bind(c)
      import
      real(c_double), value :: lambda
      real(c_double) :: sample
    end function rpois

    function runif() result(sample) bind(c)
      import
      real(c_double) :: sample
    end function runif
  end interface

end module mod_random
