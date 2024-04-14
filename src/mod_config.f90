module mod_config

  use, intrinsic :: iso_c_binding

  implicit none

  enum, bind(c)
    enumerator :: STANDARD = 1, STUDENT = 2, LOG_NORM = 3
  end enum

  enum, bind(c)
    enumerator :: PARAM = 1, RESID = 2, PAIRS = 3
  end enum

  enum, bind(c)
    enumerator :: NORMAL = 1, GAMMA = 2, POISSON = 3
  end enum

  enum, bind(c)
    enumerator :: SINGLE = 1, CALENDAR = 2, ORIGIN = 3
  end enum

  enum, bind(c)
    enumerator :: SUCCESS, FAILURE
  end enum

end module mod_config
