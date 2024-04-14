module mod_odp_triangle
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic
  use mod_config

  implicit none

  !> @brief Derived type representing an ODP triangle.
  !! @param n_dev Number of rows/columns in the triangle.
  !! @param n_cov Number of covariates in the GLM model.
  !! @param n_fit Number of fitted values in the GLM model.
  !! @param n_obs Number of observations used in the GLM model.
  !! This excludes the masked elements.
  type t_odp_triangle
    integer(c_int) :: n_dev, n_cov, n_fit, n_obs, n_pred
    real(c_double), allocatable :: triangle(:, :), triangle_fit(:, :)
    real(c_double), allocatable :: resids(:, :)
    real(c_double), allocatable :: betas(:), betas_boot(:)
    real(c_double) :: disp, disp_boot
    logical(c_bool), allocatable :: mask(:, :)
    logical :: first_call = .true.

  contains
    procedure, public :: init => odp_init_triangle, fit_glm_helper, fit_glm_boot, fit_glm
    final :: destructor
  end type t_odp_triangle

  type t_glm_res
    real(c_double), allocatable :: betas(:)
    real(c_double) :: disp

  contains
    procedure, public :: init => init_glm_res
  end type t_glm_res

contains
  subroutine print_array(X)
    real(c_double), intent(in) :: X(:, :)

    integer :: i, j, n, m

    n = size(X, 1)
    m = size(X, 2)
    do i = 1, n
      do j = 1, m
        write (*, "(es11.3)", advance="no") X(i, j)
      end do
      print*
    end do
  end subroutine print_array

  subroutine init_glm_res(this, n_dev)
    integer(c_int) :: n_dev
    class(t_glm_res) :: this

    allocate (this%betas(2 * n_dev - 1), source=0._c_double)
  end subroutine init_glm_res

  subroutine odp_init_triangle(this, triangle, mask)
    class(t_odp_triangle) :: this
    real(c_double), intent(in) :: triangle(:, :)
    logical, optional, intent(in) :: mask(:, :)

    integer :: i, j

    this%triangle = triangle
    this%n_dev = size(triangle, 1)
    this%n_cov = 2 * this%n_dev - 1
    this%n_fit = (this%n_dev**2 + this%n_dev) / 2

    if (this%first_call) then
      allocate (this%triangle_fit(this%n_dev, this%n_dev), source=0._c_double)

      if (present(mask)) then
        this%mask = mask
        this%n_obs = count(this%mask)
        this%n_pred = this%n_dev**2 - this%n_obs
      else
        this%n_obs = this%n_fit
        this%n_pred = this%n_dev**2 - this%n_obs
        allocate (this%mask(this%n_dev, this%n_dev), source=.true._c_bool)
        do j = 1, this%n_dev
          do i = this%n_dev + 2 - j, this%n_dev
            this%mask(i, j) = .false.
          end do
        end do
      end if

      allocate (this%betas(this%n_cov), source=0._c_double)
      allocate (this%resids(this%n_dev, this%n_dev), source=0._c_double)

      this%first_call = .false.
    end if
  end subroutine odp_init_triangle

  function fit_glm_helper(this, triangle, use_mask, fit, status) result(res)
    class(t_odp_triangle) :: this
    real(c_double), intent(in) :: triangle(:, :)
    logical, intent(in) :: use_mask, fit
    integer(c_int), intent(out) :: status
    type(t_glm_res) :: res

    real(c_double), allocatable :: X(:, :), y(:)
    real(c_double), allocatable :: y_fit(:), X_fit(:, :), y_reduced(:)
    real(c_double), allocatable :: rhs(:), lhs(:, :), mu(:)
    real(c_double), allocatable :: W(:, :), z(:), eta(:), betas_old(:), betas(:)

    integer(c_int) :: i, j, k, l
    real(c_double) :: diff, eps
    logical(c_bool), allocatable :: mask_loc(:, :)
    integer(c_int) :: info
    integer(c_int) :: lwork
    real(c_double), allocatable :: work(:, :)
    integer(c_int), parameter :: max_iter = 1e3
    integer(c_int) :: n_iter

    call res%init(this%n_dev)

    allocate (mask_loc(this%n_dev, this%n_dev), source=.true._c_bool)
    if (use_mask) then
      mask_loc = this%mask
    else
      do j = 1, this%n_dev
        do i = this%n_dev + 2 - j, this%n_dev
          mask_loc(i, j) = .false.
        end do
      end do
    end if

    ! Compute GLM matrix dimensions.
    this%n_obs = this%n_fit
    if (use_mask) this%n_obs = count(this%mask)

    allocate (X(this%n_obs, this%n_cov), source=0._c_double)
    allocate (y(this%n_obs), source=0._c_double)
    allocate (y_reduced(this%n_obs), source=0._c_double)
    allocate (W(this%n_obs, this%n_obs), source=0._c_double)
    allocate (z(this%n_obs), source=0._c_double)
    allocate (eta(this%n_obs), source=0._c_double)
    allocate (mu(this%n_obs), source=0._c_double)
    allocate (X_fit(this%n_fit, this%n_cov), source=0._c_double)
    allocate (y_fit(this%n_fit), source=0._c_double)

    ! Allocate LAPACK helper variables.
    allocate (rhs(this%n_cov), source=0._c_double)
    allocate (lhs(this%n_cov, this%n_cov), source=0._c_double)
    allocate (betas(this%n_cov), source=0._c_double)
    allocate (betas_old(this%n_cov), source=0._c_double)

    lwork = max(1, this%n_cov * this%n_obs + max(this%n_cov * this%n_obs, 1))
    allocate (work(lwork, lwork), source=0._c_double)

    ! Set up feature matrix and response vector.
    X = 0
    X(:, 1) = 1._c_double

    X_fit = 0
    X_fit(:, 1) = 1._c_double

    l = 1
    k = 1
    do i = 1, this%n_dev
      do j = 1, this%n_dev + 1 - i
        if (mask_loc(i, j)) then
          if (i /= 1) X(k, i) = 1
          if (j /= 1) X(k, this%n_dev + j - 1) = 1
          y(k) = triangle(i, j)
          k = k + 1
        end if
        if (i /= 1) X_fit(l, i) = 1
        if (j /= 1) X_fit(l, this%n_dev + j - 1) = 1
        l = l + 1
      end do
    end do

    ! Initialise GLM coefficients.
    mu = y + 0.1
    eta = log(mu)

    ! Fit Poisson GLM using IRWLS.
    betas = 0
    info = 0
    diff = 1E6
    eps = 1E-10
    n_iter = 0
    status = SUCCESS
    do while (diff > eps .and. n_iter < max_iter)
      betas_old = betas

      W = 0
      do i = 1, this%n_obs
        W(i, i) = sqrt(mu(i))
      end do

      z = eta + y / mu - 1
      lhs = matmul(W, X)
      rhs = matmul(W, z)

      if (any(isnan(rhs)) .or. any(isnan(lhs))) then
        status = FAILURE
        return
      end if

      call dgels('N', this%n_obs, this%n_cov, 1, lhs, this%n_obs, rhs, this%n_obs, work, lwork, info)

      betas = rhs(1:this%n_cov)
      diff = norm2(betas - betas_old)

      eta = matmul(X, betas)
      mu = exp(eta)

      if (any(mu <= 0)) then
        status = FAILURE
        return
      end if
      if (any(isnan(mu)) .or. .not. all(ieee_is_finite(mu))) then
        status = FAILURE
        return
      end if

      n_iter = n_iter + 1
    end do

    if (n_iter == max_iter) then
      status = FAILURE
      return
    end if

    y_fit = exp(matmul(X_fit, betas))
    y_reduced = exp(matmul(X, betas))

    res%disp = sum((y - y_reduced)**2 / y_reduced) / (this%n_obs - this%n_cov)
    res%betas = betas

    if (fit) then
      k = 1
      do i = 1, this%n_dev
        do j = 1, this%n_dev + 1 - i
          this%triangle_fit(i, j) = y_fit(k)
          k = k + 1
        end do
      end do

      do i = 1, this%n_dev
        do j = 1, this%n_dev + 1 - i
          this%resids(i, j) = (triangle(i, j) - this%triangle_fit(i, j)) / sqrt(this%triangle_fit(i, j))
        end do
      end do
    end if
  end function fit_glm_helper

  subroutine fit_glm(this, use_mask, status)
    class(t_odp_triangle), intent(inout) :: this
    logical, intent(in) :: use_mask
    integer, intent(out) :: status

    type(t_glm_res) :: res

    res = this%fit_glm_helper(this%triangle, use_mask, .true., status)
    this%betas = res%betas
    this%disp = res%disp
  end subroutine fit_glm

  subroutine fit_glm_boot(this, triangle_boot, use_mask, status)
    class(t_odp_triangle), intent(inout) :: this
    real(c_double), intent(in) :: triangle_boot(:, :)
    logical, intent(in) :: use_mask
    integer, intent(out) :: status

    type(t_glm_res) :: res

    res = this%fit_glm_helper(triangle_boot, use_mask, .false., status)
    this%betas_boot = res%betas
    this%disp_boot = res%disp
  end subroutine fit_glm_boot

  subroutine destructor(this)
    type(t_odp_triangle) :: this

    if (allocated(this%triangle)) deallocate (this%triangle)
    if (allocated(this%triangle_fit)) deallocate (this%triangle_fit)
    if (allocated(this%resids)) deallocate (this%resids)
    if (allocated(this%betas)) deallocate (this%betas)
    if (allocated(this%betas_boot)) deallocate (this%betas_boot)
    if (allocated(this%mask)) deallocate (this%mask)
  end subroutine destructor

end module mod_odp_triangle
