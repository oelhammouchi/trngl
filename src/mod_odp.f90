module mod_odp

  use, intrinsic :: iso_c_binding
  use, intrinsic :: omp_lib
  use, intrinsic :: ieee_arithmetic
  use mod_config
  use mod_random
  use mod_pgb
  use mod_odp_triangle

  implicit none

contains

  function odp_param_boot(trngl, n_boot, n_sim, dist, pgb, status) result(reserve)
    type(t_odp_triangle), intent(inout) :: trngl
    integer(c_int), intent(in) :: n_boot, n_sim, dist
    type(c_ptr), intent(in), value :: pgb
    integer, intent(inout) :: status

    real(c_double) :: reserve(n_boot * n_sim)
    real(c_double) :: X_pred(trngl%n_pred, trngl%n_cov), y_pred(trngl%n_pred), y_sim(trngl%n_pred)
    real(c_double) :: triangle_boot(trngl%n_dev, trngl%n_dev)

    real(c_double) :: lambda, mean, sd, shape, scale
    integer(c_int) :: i, j, k, i_boot, i_sim, n_fails, max_fails

    max_fails = 1e6
    n_fails = 0
    call trngl%fit_glm(use_mask=.true., status=status)

    i_boot = 1
    main_loop: do while (i_boot <= n_boot .and. n_fails < max_fails)
      status = SUCCESS
      triangle_boot = 0
      do i = 1, trngl%n_dev
        do j = 1, trngl%n_dev + 1 - i
          if (dist == NORMAL) then
            mean = trngl%triangle_fit(i, j)
            sd = sqrt(trngl%disp * trngl%triangle_fit(i, j))
            triangle_boot(i, j) = rnorm(mean, sd)
            if (triangle_boot(i, j) < 0) cycle main_loop

          else if (dist == GAMMA) then
            shape = trngl%triangle_fit(i, j) / trngl%disp
            scale = trngl%disp
            triangle_boot(i, j) = rgamma(shape, scale)
            if (triangle_boot(i, j) <= 0) cycle main_loop

          else if (dist == POISSON) then
            lambda = trngl%triangle_fit(i, j) / trngl%disp
            triangle_boot(i, j) = trngl%disp * rpois(lambda)
            if (triangle_boot(i, j) < 0) cycle main_loop
          end if
        end do
      end do

      call trngl%fit_glm_boot(triangle_boot, use_mask=.false., status=status)
      if (status == FAILURE) then
        n_fails = n_fails + 1
        cycle main_loop
      end if

      X_pred = 0.0
      X_pred(:, 1) = 1.0

      k = 1
      do i = 2, trngl%n_dev
        do j = trngl%n_dev + 1 - i + 1, trngl%n_dev
          X_pred(k, i) = 1
          X_pred(k, trngl%n_dev + j - 1) = 1
          k = k + 1
        end do
      end do

      y_pred = exp(matmul(X_pred, trngl%betas_boot))
      do i_sim = 1, n_sim
        y_sim = 0
        do i = 1, trngl%n_pred
          if (dist == NORMAL) then
            mean = y_pred(i)
            sd = sqrt(trngl%disp_boot * y_pred(i))
            y_sim(i) = rnorm(mean, sd)
            if (y_sim(i) < 0) cycle main_loop

          else if (dist == GAMMA) then
            shape = y_pred(i)**2 / (trngl%disp_boot * y_pred(i))
            scale = (trngl%disp_boot * y_pred(i)) / y_pred(i)
            y_sim(i) = rgamma(shape, scale)

          else if (dist == POISSON) then
            lambda = y_pred(i) / trngl%disp_boot
            y_sim(i) = trngl%disp_boot * rpois(lambda)
            if (y_sim(i) < 0) then
              cycle main_loop
            end if
          end if
        end do

        reserve(n_sim * (i_boot - 1) + i_sim) = sum(y_sim)
        call pgb_incr(pgb, 1)
      end do

      i_boot = i_boot + 1
    end do main_loop

    status = merge(FAILURE, SUCCESS, n_fails == max_fails)
  end function odp_param_boot

  function odp_resid_boot(trngl, n_boot, n_sim, pgb, status) result(reserve)
    type(t_odp_triangle), intent(inout) :: trngl
    integer(c_int), intent(in) :: n_boot, n_sim
    type(c_ptr), intent(in), value :: pgb
    integer, intent(inout) :: status

    real(c_double) :: reserve(n_boot * n_sim)
    integer(c_int) :: n_pts
    real(c_double), allocatable :: X_pred(:, :), y_pred(:)
    real(c_double), allocatable :: triangle_boot(:, :), betas_boot(:), resids_boot(:, :)
    real(c_double), allocatable :: triangle_pred(:, :)

    real(c_double) :: resid_sim

    real(c_double), allocatable :: flat_resids(:)

    integer(c_int) :: i, j, k, i_boot, i_sim, n_fails, max_fails

    allocate (X_pred(trngl%n_pred, trngl%n_cov))
    allocate (y_pred(trngl%n_pred))
    allocate (triangle_boot(trngl%n_dev, trngl%n_dev))
    allocate (betas_boot(trngl%n_cov))
    allocate (resids_boot(trngl%n_dev, trngl%n_dev))
    allocate (triangle_pred(trngl%n_dev, trngl%n_dev))

    max_fails = 1e6
    n_fails = 0

    n_pts = count(trngl%mask)
    allocate (flat_resids(n_pts), source=0._c_double)
    call trngl%fit_glm(use_mask=.false., status=status)
    flat_resids = pack(trngl%resids, trngl%mask)

    i_boot = 1
    main_loop: do while (i_boot <= n_boot .and. n_fails <= max_fails)
      status = SUCCESS
      triangle_boot = 0
      resids_boot = 0
      do i = 1, trngl%n_dev
        do j = 1, trngl%n_dev + 1 - i
          resids_boot(i, j) = flat_resids(1 + int(n_pts * runif()))
          triangle_boot(i, j) = resids_boot(i, j) * sqrt(trngl%triangle_fit(i, j)) + trngl%triangle_fit(i, j)
          if (triangle_boot(i, j) <= 0) cycle main_loop
        end do
      end do

      call trngl%fit_glm_boot(triangle_boot, use_mask=.false., status=status)
      if (status == FAILURE) then
        n_fails = n_fails + 1
        cycle main_loop
      end if

      X_pred = 0.0
      X_pred(:, 1) = 1.0

      k = 1
      do i = 2, trngl%n_dev
        do j = trngl%n_dev + 1 - i + 1, trngl%n_dev
          X_pred(k, i) = 1
          if (j /= 1) X_pred(k, trngl%n_dev + j - 1) = 1
          k = k + 1
        end do
      end do

      y_pred = exp(matmul(X_pred, trngl%betas_boot))
      do i_sim = 1, n_sim
        triangle_pred = 0
        k = 1
        do i = 2, trngl%n_dev
          do j = trngl%n_dev + 1 - i + 1, trngl%n_dev
            triangle_pred(i, j) = y_pred(k)
            k = k + 1
          end do
        end do

        k = 1
        do i = 2, trngl%n_dev
          do j = trngl%n_dev + 1 - i + 1, trngl%n_dev
            resid_sim = flat_resids(1 + int(n_pts * runif()))
            triangle_pred(i, j) = triangle_pred(i, j) + resid_sim * sqrt(triangle_pred(i, j))
          end do
        end do

        reserve(n_sim * (i_boot - 1) + i_sim) = real(sum(triangle_pred), kind=c_double)
        call pgb_incr(pgb, 1)
      end do

      i_boot = i_boot + 1
    end do main_loop

    status = merge(FAILURE, SUCCESS, n_fails == max_fails)
  end function odp_resid_boot

 function odp_param_boot_cpp(n_dev, triangle, n_boot, n_sim, dist, mask, reserve, pgb) result(status) bind(c, name="odp_param_boot")
    integer(c_int), intent(in), value :: n_boot, n_sim, n_dev
    logical(c_bool), intent(in) :: mask(n_dev, n_dev)
    real(c_double), intent(in) :: triangle(n_dev, n_dev)
    integer(c_int), intent(in), value :: dist
    real(c_double), intent(out) :: reserve(n_boot * n_sim)
    type(c_ptr), intent(in), value :: pgb

    integer(c_int) :: status
    type(t_odp_triangle) :: trngl

    call trngl%init(triangle, logical(mask))
    reserve = odp_param_boot(trngl, n_boot, n_sim, dist, pgb, status)
  end function odp_param_boot_cpp

  function odp_resid_boot_cpp(n_dev, triangle, n_boot, n_sim, mask, reserve, pgb) result(status) bind(c, name="odp_resid_boot")
    integer(c_int), intent(in), value :: n_boot, n_sim, n_dev
    logical(c_bool), intent(in) :: mask(n_dev, n_dev)
    real(c_double), intent(in) :: triangle(n_dev, n_dev)
    real(c_double), intent(out) :: reserve(n_boot * n_sim)
    type(c_ptr), intent(in), value :: pgb

    integer(c_int) :: status
    type(t_odp_triangle) :: trngl

    call trngl%init(triangle, logical(mask))
    reserve = odp_resid_boot(trngl, n_boot, n_sim, pgb, status)
  end function odp_resid_boot_cpp

end module mod_odp
