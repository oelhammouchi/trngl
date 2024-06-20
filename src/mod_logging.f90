module mod_logging

  use, intrinsic :: iso_c_binding

  implicit none

  enum, bind(c)
    enumerator :: SPDLOG_LEVEL_TRACE = 0, &
      SPDLOG_LEVEL_DEBUG = 1, &
      SPDLOG_LEVEL_INFO = 2, &
      SPDLOG_LEVEL_WARN = 3, &
      SPDLOG_LEVEL_ERROR = 4, &
      SPDLOG_LEVEL_CRITICAL = 5, &
      SPDLOG_LEVEL_OFF = 6
  end enum

  interface

    subroutine log_matrix(n_rows, n_cols, mat, msg, level) bind(c)
      import
      integer(c_int), intent(in), value :: n_rows, n_cols
      real(c_double), intent(in) :: mat(n_rows, n_cols)
      character(c_char), dimension(*), intent(in) :: msg
      integer(kind=kind(SPDLOG_LEVEL_TRACE)), value :: level
    end subroutine log_matrix

    subroutine log_int(it, msg, level) bind(c)
      import
      integer(c_int), intent(in), value :: it
      character(c_char), dimension(*), intent(in) :: msg
      integer(kind=kind(SPDLOG_LEVEL_TRACE)), value :: level
    end subroutine log_int
  end interface

end module mod_logging
