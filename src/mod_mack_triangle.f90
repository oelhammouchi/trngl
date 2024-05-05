module mod_mack_triangle
  use, intrinsic :: iso_c_binding
  use mod_random
  use mod_config

  implicit none

  type t_mack_triangle
    real(c_double), allocatable :: triangle(:, :), latest(:)
    integer(c_int) :: n_dev
    real(c_double), allocatable :: scale_facs(:, :), sigmas_jack(:, :)

    real(c_double), allocatable :: ln_means(:, :), ln_sigmas(:, :), ln_shifts(:, :)
    real(c_double), allocatable :: ln_means_boot(:, :), ln_sigmas_boot(:, :), ln_shifts_boot(:, :)
    real(c_double), allocatable :: ln_means_sim(:, :), ln_sigmas_sim(:, :), ln_shifts_sim(:, :)

    real(c_double), allocatable :: indiv_dev_facs(:, :), dev_facs(:), sigmas(:)
    real(c_double), allocatable :: indiv_dev_facs_boot(:, :), dev_facs_boot(:), sigmas_boot(:)
    real(c_double), allocatable :: resids(:, :), resampled_resids(:, :)
    logical(c_bool), allocatable :: obs_mask(:, :), resids_mask(:, :)
    logical :: first_call = .true.

  contains
    procedure, public :: init => mack_init_triangle, compute_resids, resample_boot, resample_sim
    procedure, public :: fit_cl_helper, fit_cl, fit_cl_boot
    final :: destructor
  end type t_mack_triangle

  type t_cl_res
    real(c_double), allocatable :: dev_facs(:), sigmas(:)
    real(c_double), allocatable :: indiv_dev_facs(:, :)

  contains
    procedure, public :: init => init_cl_res
  end type t_cl_res

  interface
    function raise(sig) bind(C, name="raise")
      use iso_c_binding, only: c_int
      integer(c_int) :: raise
      integer(c_int), value :: sig
    end function
  end interface

contains
  function get_latest(triangle) result(res)
    real(c_double), intent(in) :: triangle(:, :)
    real(c_double), allocatable :: res(:)
    integer(c_int) :: j, n_dev

    n_dev = size(triangle, 1)
    allocate (res(n_dev), source=0._c_double)
    do j = 1, n_dev
      res(j) = triangle(n_dev + 1 - j, j)
    end do
  end function get_latest

  pure real(c_double) function extrapolate_sigma(sigmas, col)
    real(c_double), intent(in) :: sigmas(:)
    integer, intent(in) :: col

    extrapolate_sigma = sqrt(min(sigmas(col - 1)**2, sigmas(col - 2)**2, sigmas(col - 1)**4 / sigmas(col - 2)**2))
  end function extrapolate_sigma

  subroutine init_cl_res(this, n_dev)
    integer(c_int) :: n_dev
    class(t_cl_res) :: this

    allocate (this%dev_facs(n_dev - 1), source=0._c_double)
    allocate (this%sigmas(n_dev - 1), source=0._c_double)
    allocate (this%indiv_dev_facs(n_dev - 1, n_dev - 1), source=0._c_double)
  end subroutine init_cl_res

  subroutine mack_init_triangle(this, triangle, obs_mask)
    class(t_mack_triangle), intent(inout) :: this
    real(c_double), intent(in) :: triangle(:, :)
    logical, optional, intent(in) :: obs_mask(:, :)

    this%triangle = triangle
    this%n_dev = size(this%triangle, 1)
    this%latest = get_latest(this%triangle)

    if (present(obs_mask)) then
      this%obs_mask = obs_mask
      this%resids_mask = obs_mask(1:(this%n_dev - 1), 2:this%n_dev)
      this%resids_mask(:, this%n_dev - 1) = .false.
      this%resids_mask(:, this%n_dev - 2) = .false.
    else
      if (this%first_call) then
        allocate (this%obs_mask(this%n_dev, this%n_dev))
        allocate (this%resids_mask(this%n_dev - 1, this%n_dev - 1))
      end if
    end if

    if (this%first_call) then
      allocate (this%indiv_dev_facs(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%dev_facs(this%n_dev - 1), source=0._c_double)
      allocate (this%sigmas(this%n_dev - 1), source=0._c_double)

      allocate (this%indiv_dev_facs_boot(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%dev_facs_boot(this%n_dev - 1), source=0._c_double)
      allocate (this%sigmas_boot(this%n_dev - 1), source=0._c_double)

      allocate (this%scale_facs(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%sigmas_jack(this%n_dev - 1, this%n_dev - 1), source=0._c_double)

      allocate (this%ln_shifts(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_means(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_sigmas(this%n_dev - 1, this%n_dev - 1), source=0._c_double)

      allocate (this%ln_shifts_boot(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_means_boot(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_sigmas_boot(this%n_dev - 1, this%n_dev - 1), source=0._c_double)

      allocate (this%ln_shifts_sim(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_means_sim(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%ln_sigmas_sim(this%n_dev - 1, this%n_dev - 1), source=0._c_double)

      allocate (this%resids(this%n_dev - 1, this%n_dev - 1), source=0._c_double)
      allocate (this%resampled_resids(this%n_dev - 1, this%n_dev - 1), source=0._c_double)

      this%first_call = .false.
    end if
  end subroutine mack_init_triangle

  subroutine destructor(this)
    type(t_mack_triangle) :: this

    if (allocated(this%indiv_dev_facs)) deallocate (this%indiv_dev_facs)
    if (allocated(this%dev_facs)) deallocate (this%dev_facs)
    if (allocated(this%sigmas)) deallocate (this%sigmas)
    if (allocated(this%indiv_dev_facs)) deallocate (this%indiv_dev_facs_boot)
    if (allocated(this%dev_facs_boot)) deallocate (this%dev_facs_boot)
    if (allocated(this%sigmas_boot)) deallocate (this%sigmas_boot)
    if (allocated(this%scale_facs)) deallocate (this%scale_facs)
    if (allocated(this%sigmas_jack)) deallocate (this%sigmas_jack)
    if (allocated(this%ln_shifts)) deallocate (this%ln_shifts)
    if (allocated(this%ln_means)) deallocate (this%ln_means)
    if (allocated(this%ln_sigmas)) deallocate (this%ln_sigmas)
    if (allocated(this%ln_shifts_boot)) deallocate (this%ln_shifts_boot)
    if (allocated(this%ln_means_boot)) deallocate (this%ln_means_boot)
    if (allocated(this%ln_sigmas_boot)) deallocate (this%ln_sigmas_boot)
    if (allocated(this%ln_shifts_sim)) deallocate (this%ln_shifts_sim)
    if (allocated(this%ln_means_sim)) deallocate (this%ln_means_sim)
    if (allocated(this%ln_sigmas_sim)) deallocate (this%ln_sigmas_sim)
    if (allocated(this%resids)) deallocate (this%resids)
    if (allocated(this%resampled_resids)) deallocate (this%resampled_resids)
    if (allocated(this%obs_mask)) deallocate (this%obs_mask)
    if (allocated(this%resids_mask)) deallocate (this%resids_mask)
  end subroutine destructor

  subroutine resample_boot(this)
    class(t_mack_triangle), intent(inout) :: this

    integer(c_int) :: i, j, k, n_dev, n_resids, n_rows
    integer(c_int), parameter :: SIGINT = 2
    integer(c_int) :: rand_idxs(1, 2)
    integer(c_int), allocatable :: idxs(:, :)

    n_dev = size(this%resids, 1) + 1
    n_resids = count(this%resids_mask)
    allocate (idxs(n_resids, 2), source=0)
    k = 1
    do j = 2, n_dev
      n_rows = n_dev + 1 - j
      do i = 1, n_rows
        if (this%resids_mask(i, j - 1)) then
          idxs(k, :) = [i, j - 1]
          k = k + 1
        end if
      end do
    end do

    this%resampled_resids = 0
    do j = 2, n_dev
      n_rows = n_dev + 1 - j
      do i = 1, n_rows
        rand_idxs(1, :) = idxs(1 + int(n_resids * runif()), :)
        this%resampled_resids(i, j - 1) = this%resids(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_sigmas_boot(i, j - 1) = this%ln_sigmas(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_means_boot(i, j - 1) = this%ln_means(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_shifts_boot(i, j - 1) = this%ln_shifts(rand_idxs(1, 1), rand_idxs(1, 2))
      end do
    end do

    deallocate (idxs)
  end subroutine resample_boot

  subroutine resample_sim(this)
    class(t_mack_triangle), intent(inout) :: this
    integer(c_int) :: i, j, k, n_dev, n_resids, n_rows
    integer(c_int) :: rand_idxs(1, 2)
    integer(c_int), allocatable :: idxs(:, :)

    n_dev = size(this%resids, 1) + 1
    n_resids = count(this%resids_mask)
    allocate (idxs(n_resids, 2), source=0)
    k = 1
    do j = 2, n_dev
      n_rows = n_dev + 1 - j
      do i = 1, n_rows
        if (this%resids_mask(i, j - 1)) then
          idxs(k, :) = [i, j - 1]
          k = k + 1
        end if
      end do
    end do

    do j = 2, n_dev
      n_rows = n_dev + 1 - j
      do i = n_rows + 1, n_dev
        rand_idxs(1, :) = idxs(1 + int(n_resids * runif()), :)
        this%resampled_resids(i - 1, j - 1) = this%resids(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_sigmas_sim(i - 1, j - 1) = this%ln_sigmas(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_means_sim(i - 1, j - 1) = this%ln_means(rand_idxs(1, 1), rand_idxs(1, 2))
        this%ln_shifts_sim(i - 1, j - 1) = this%ln_shifts(rand_idxs(1, 1), rand_idxs(1, 2))
      end do
    end do

    deallocate (idxs)
  end subroutine resample_sim

  !> @brief Compute chain ladder development factors and sigmas.
    !! @param triangle Cumulative claims triangle.
    !! @param use_mask[in] Bool indicating whether the triangle's mask should be used
    !! to exclude certain observations from the fit.
  function fit_cl_helper(this, triangle, use_mask) result(res)
    class(t_mack_triangle), intent(inout) :: this
    real(c_double), intent(in) :: triangle(:, :)
    logical, intent(in) :: use_mask
    type(t_cl_res) :: res

    integer(c_int) :: i, j, n_rows, n_dev, n_pts_col
    logical(c_bool), allocatable :: col_mask(:)

    call res%init(this%n_dev)
    n_dev = size(triangle, 1)

    if (use_mask) then
      do j = 1, this%n_dev - 1
        n_rows = this%n_dev - j
        do i = 1, n_rows
          res%indiv_dev_facs(i, j) = triangle(i, j + 1) / triangle(i, j)
        end do
        col_mask = this%obs_mask(1:n_rows, j + 1)
        n_pts_col = count(col_mask)
        res%dev_facs(j) = sum(triangle(1:n_rows, j + 1), mask=col_mask) / sum(triangle(1:n_rows, j), mask=col_mask)
        if (n_pts_col >= 2) then
          res%sigmas(j) = sqrt( &
                    sum(triangle(1:n_rows, j) * (res%indiv_dev_facs(1:n_rows, j) - res%dev_facs(j))**2, mask=col_mask) / n_pts_col &
                          )
        else
          res%sigmas(j) = extrapolate_sigma(res%sigmas, j)
        end if
      end do
    else
      do j = 1, this%n_dev - 1
        n_rows = this%n_dev - j
        do i = 1, n_rows
          res%indiv_dev_facs(i, j) = triangle(i, j + 1) / triangle(i, j)
        end do
        res%dev_facs(j) = sum(triangle(1:n_rows, j + 1)) / sum(triangle(1:n_rows, j))
        if (j < this%n_dev - 1) then
          res%sigmas(j) = sqrt(sum(triangle(1:n_rows, j) * (res%indiv_dev_facs(1:n_rows, j) - res%dev_facs(j))**2) / (n_rows - 1))
        else
          res%sigmas(j) = extrapolate_sigma(res%sigmas, j)
        end if
      end do
    end if
  end function fit_cl_helper

  subroutine fit_cl(this, use_mask)
    class(t_mack_triangle), intent(inout) :: this
    logical, intent(in) :: use_mask
    type(t_cl_res) :: res

    res = this%fit_cl_helper(this%triangle, use_mask)
    this%dev_facs = res%dev_facs
    this%sigmas = res%sigmas
    this%indiv_dev_facs = res%indiv_dev_facs
  end subroutine fit_cl

  subroutine fit_cl_boot(this, triangle_boot, use_mask)
    class(t_mack_triangle), intent(inout) :: this
    real(c_double), intent(in) :: triangle_boot(:, :)
    logical, intent(in) :: use_mask
    type(t_cl_res) :: res

    res = this%fit_cl_helper(triangle_boot, use_mask)
    this%dev_facs_boot = res%dev_facs
    this%sigmas_boot = res%sigmas
    this%indiv_dev_facs_boot = res%indiv_dev_facs
  end subroutine fit_cl_boot

  subroutine compute_resids(this, type)
    class(t_mack_triangle), intent(inout) :: this
    integer(c_int), intent(in) :: type
    logical(c_bool), allocatable :: col_mask(:)

    integer :: i, j, n_rows, n_pts_col
    real(c_double) :: dev_fac_jack

    if (type /= LOG_NORM) then
      this%scale_facs = 0
      do j = 1, this%n_dev - 1
        n_rows = this%n_dev - j
        do i = 1, n_rows
          this%scale_facs(i, j) = sqrt(1 - this%triangle(i, j) / sum(this%triangle(1:n_rows, j)))
        end do
      end do
      this%scale_facs(1, this%n_dev - 1) = 1
    end if

    this%resids = 0
    select case (type)
    case (STANDARD)
      do j = 1, this%n_dev - 1
        n_rows = this%n_dev - j
        do i = 1, n_rows
          this%resids(i, j) = (this%indiv_dev_facs(i, j) - this%dev_facs(j)) * sqrt(this%triangle(i, j)) / &
                              (this%sigmas(j) * this%scale_facs(i, j))
        end do
      end do
    case (STUDENT)
      this%sigmas_jack = 0
      allocate (col_mask(this%n_dev - 1))
      do j = 1, this%n_dev - 1
        n_rows = this%n_dev - j
        do i = 1, n_rows
          col_mask = .true.
          col_mask(i) = .false.
          n_pts_col = n_rows - 1
          dev_fac_jack = sum(this%triangle(1:n_rows, j + 1), mask=col_mask(1:n_rows)) / &
                         sum(this%triangle(1:n_rows, j), mask=col_mask(1:n_rows))
          if (n_pts_col >= 2) then
            this%sigmas_jack(i, j) = sqrt(sum(this%triangle(1:n_rows, j) * &
                                   (this%indiv_dev_facs(1:n_rows, j) - dev_fac_jack)**2, mask=col_mask(1:n_rows)) / (n_pts_col - 1))
          else
            this%sigmas_jack(i, j) = extrapolate_sigma(this%sigmas_jack(i, :), j)
          end if
          this%resids(i, j) = (this%triangle(i, j + 1) - this%dev_facs(j) * this%triangle(i, j)) / &
                              (this%sigmas_jack(i, j) * this%scale_facs(i, j) * sqrt(this%triangle(i, j)))
        end do
      end do
    case (LOG_NORM)
      do j = 1, this%n_dev
        n_rows = this%n_dev - j
        do i = 1, n_rows
          this%ln_shifts(i, j) = this%dev_facs(j) * sqrt(this%triangle(i, j)) / this%sigmas(j)
          this%ln_sigmas(i, j) = sqrt(log(1 + 1 / this%ln_shifts(i, j)**2))
          this%ln_means(i, j) = log(this%ln_shifts(i, j)) - this%ln_sigmas(i, j)**2 / 2
          this%resids(i, j) = (this%triangle(i, j + 1) - this%dev_facs(j) * this%triangle(i, j)) / &
                              (this%sigmas(j) * sqrt(this%triangle(i, j)))
          this%resids(i, j) = (log(this%resids(i, j) + this%ln_shifts(i, j)) - this%ln_means(i, j)) / this%ln_sigmas(i, j)
        end do
      end do
    end select

    if (type /= LOG_NORM) then
      this%resids(1, this%n_dev - 1) = 0
      this%resids(:, this%n_dev - 2) = 0
    end if

    !   n_resids = (this%n_dev**2 - this%n_dev)/2 - 3
    !   if (resids_type /= STUDENTISED) then
    !     resids_mean = 0
    !     do i = 1, this%n_dev - 1
    !       do j = 1, this%n_dev - i
    !         resids_mean = resids_mean + resids(i, j)
    !       end do
    !     end do
    !     resids_mean = resids_mean / n_resids
    !     do i = 1, this%n_dev - 1
    !       do j = 1, this%n_dev - i
    !         resids(i, j) = resids(i, j) - resids_mean
    !       end do
    !     end do
    !   end if
  end subroutine compute_resids

end module mod_mack_triangle
