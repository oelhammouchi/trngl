module mod_pgb

  use, intrinsic :: iso_c_binding

  implicit none

interface

  function pgb_create(total) result(pgb) bind(c)
    import
    integer(c_int), value :: total
    type(c_ptr) :: pgb
  end function pgb_create

  subroutine pgb_incr(pgb, by) bind(c)
    import
    type(c_ptr), value :: pgb
    integer(c_int), value :: by
  end subroutine pgb_incr

end interface

end module mod_pgb
