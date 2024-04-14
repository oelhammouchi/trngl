module mod_mack
  use, intrinsic :: iso_c_binding
  use, intrinsic :: omp_lib
  use mod_config
  use mod_random
  use mod_pgb
  use mod_mack_triangle

  implicit none

contains

  !> @brief Perform a parametric bootstrap of the Mack chain ladder model.
  !! @param[in, out] trngl Cumulative claims triangle.
  !! @param[in] n_boot Number of bootstrap simulations.
  !! @param[in] cond Boolean indicating whether to perform a conditional bootstrap.
  !! @param[in] dist Distribution to use in the bootstrap.
  !! @param[out] status Integer indicating status of the call.
  !! @return reserve An array of bootstrapped reserve estimates.
  function mack_param_boot(trngl, n_boot, n_sim, cond, dist, pgb, status) result(reserve)
    type(t_mack_triangle), intent(inout) :: trngl
    integer(c_int), intent(in) :: n_boot, n_sim
    logical, intent(in) :: cond
    integer, intent(in) :: dist
    type(c_ptr), intent(in), value :: pgb
    integer, intent(inout) :: status

    real(c_double) :: reserve(n_boot * n_sim)
    integer :: i, j, i_diag, i_boot, i_sim, n_rows
    real(c_double) :: triangle_boot(trngl%n_dev, trngl%n_dev), triangle_sim(trngl%n_dev, trngl%n_dev)
    real(c_double) :: mean, sd, shape, scale
    logical :: progress

    progress = c_associated(pgb)
    call trngl%fit_cl(use_mask=.true.)

    i_boot = 1
    main_loop: do while (i_boot <= n_boot)
      status = SUCCESS
      if (cond) then
        triangle_boot(:, 1) = trngl%triangle(:, 1)
        do j = 2, trngl%n_dev
          n_rows = trngl%n_dev + 1 - j
          do i = 1, n_rows
            if (dist == NORMAL) then
              mean = trngl%dev_facs(j - 1) * trngl%triangle(i, j - 1)
              sd = trngl%sigmas(j - 1) * sqrt(trngl%triangle(i, j - 1))
              triangle_boot(i, j) = rnorm(mean, sd)
              if (triangle_boot(i, j) <= 0) status = FAILURE

            else if (dist == GAMMA) then
              shape = (trngl%dev_facs(j - 1)**2 * trngl%triangle(i, j - 1)) / trngl%sigmas(j - 1)**2
              scale = trngl%sigmas(j - 1)**2 / trngl%dev_facs(j - 1)
              triangle_boot(i, j) = rgamma(shape, scale)
              if (triangle_boot(i, j) <= 0) status = FAILURE
            end if
          end do
        end do

      else
        triangle_boot(:, 1) = trngl%triangle(:, 1)
        do j = 2, trngl%n_dev
          n_rows = trngl%n_dev + 1 - j
          do i = 1, n_rows
            if (dist == NORMAL) then
              mean = trngl%dev_facs(j - 1) * triangle_boot(i, j - 1)
              sd = trngl%sigmas(j - 1) * sqrt(triangle_boot(i, j - 1))
              triangle_boot(i, j) = rnorm(mean, sd)
              if (triangle_boot(i, j) <= 0) status = FAILURE

            else if (dist == GAMMA) then
              shape = (trngl%dev_facs(j - 1)**2 * triangle_boot(i, j - 1)) / trngl%sigmas(j - 1)**2
              scale = trngl%sigmas(j - 1)**2 / trngl%dev_facs(j - 1)
              triangle_boot(i, j) = rgamma(shape, scale)
              if (triangle_boot(i, j) <= 0) status = FAILURE
            end if
          end do
        end do
      end if

      if (status == FAILURE) cycle main_loop
      call trngl%fit_cl_boot(triangle_boot, use_mask=.false.)

      sim_loop: do i_sim = 1, n_sim
        triangle_sim = trngl%triangle
        if (dist == NORMAL) then
          do i_diag = 1, trngl%n_dev - 1
            do i = i_diag + 1, trngl%n_dev
              j = trngl%n_dev + i_diag + 1 - i
              mean = trngl%dev_facs_boot(j - 1) * triangle_sim(i, j - 1)
              sd = trngl%sigmas_boot(j - 1) * sqrt(triangle_sim(i, j - 1))
              if (i == 2 .and. j == 7) then
                triangle_sim(i, j) = rnorm(mean, sd)
              else
                triangle_sim(i, j) = rnorm(mean, sd)
              end if
              if (triangle_sim(i, j) <= 0) status = FAILURE
            end do
          end do

        else if (dist == GAMMA) then
          do i_diag = 1, trngl%n_dev - 1
            do i = i_diag + 1, trngl%n_dev
              j = trngl%n_dev + i_diag + 1 - i
              shape = (trngl%dev_facs_boot(j - 1)**2 * triangle_sim(i, j - 1)) / trngl%sigmas_boot(j - 1)**2
              scale = trngl%sigmas_boot(j - 1)**2 / trngl%dev_facs_boot(j - 1)
              triangle_sim(i, j) = rgamma(shape, scale)
              if (triangle_sim(i, j) <= 0) status = FAILURE
            end do
          end do
        end if

        if (status == FAILURE) cycle sim_loop
        reserve(n_sim * (i_boot - 1) + i_sim) = sum(triangle_sim(:, trngl%n_dev)) - sum(get_latest(triangle_sim))
      end do sim_loop

      i_boot = i_boot + 1
      if (progress) call pgb_incr(pgb, n_sim)
    end do main_loop
  end function mack_param_boot

  !> @brief Perform a semiparamteric bootstrap of the Mack chain ladder model.
  !! @param[in, out] trngl Cumulative claims triangle.
  !! @param[in] n_boot Number of bootstrap simulations.
  !! @param[in] cond Boolean indicating whether to perform a conditional bootstrap.
  !! @param[in] resid_type Type of residuals to use in the bootstrap.
  !! @param[out] status Integer indicating status of the call.
  !! @return reserve An array of bootstrapped reserve estimates.
  function mack_resid_boot(trngl, n_boot, n_sim, cond, resid_type, pgb, status) result(reserve)
    integer, intent(in) :: n_boot, n_sim
    type(t_mack_triangle), intent(inout) :: trngl
    logical, intent(in) :: cond
    integer, intent(in) :: resid_type
    type(c_ptr), intent(in), value :: pgb
    integer, intent(out) :: status

    real(c_double) :: reserve(n_boot * n_sim)
    integer :: i, j, i_diag, i_boot, i_sim, n_rows
    real(c_double) :: triangle_boot(trngl%n_dev, trngl%n_dev), triangle_sim(trngl%n_dev, trngl%n_dev)
    real(c_double) :: mean, sd
    logical :: progress

    integer :: myvar

    myvar = 0

    progress = c_associated(pgb)
    call trngl%fit_cl(use_mask=.false.)
    call trngl%compute_resids(type=resid_type)

    i_boot = 1
    main_loop: do while (i_boot <= n_boot)
      status = SUCCESS
      call trngl%resample_boot()
      if (cond) then
        triangle_boot(:, 1) = trngl%triangle(:, 1)
        cond_boot_loop: do j = 1, trngl%n_dev - 1
          n_rows = trngl%n_dev - j
          do i = 1, n_rows
            if (resid_type == LOG_NORM) then
              triangle_boot(i, j + 1) = exp(trngl%resampled_resids(i, j) * trngl%ln_sigmas(i, j) + &
                                            trngl%ln_means(i, j)) - trngl%ln_shifts(i, j)

              triangle_boot(i, j + 1) = trngl%dev_facs(j) * trngl%triangle(i, j) + &
                                        trngl%sigmas(j) * sqrt(trngl%triangle(i, j)) * triangle_boot(i, j + 1)
            else
              mean = trngl%dev_facs(j) * trngl%triangle(i, j)
              sd = trngl%sigmas(j) * sqrt(trngl%triangle(i, j))
              triangle_boot(i, j + 1) = mean + sd * trngl%resampled_resids(i, j)
            end if

            if (triangle_boot(i, j + 1) <= 0) then
              status = FAILURE
              exit cond_boot_loop
            end if
          end do
        end do cond_boot_loop

      else
        triangle_boot(:, 1) = trngl%triangle(:, 1)
        uncond_boot_loop: do j = 1, trngl%n_dev - 1
          n_rows = trngl%n_dev - j
          do i = 1, n_rows
            if (resid_type == LOG_NORM) then
              triangle_boot(i, j + 1) = exp(trngl%resampled_resids(i, j) * trngl%ln_sigmas_boot(i, j) + &
                                            trngl%ln_means_boot(i, j)) - trngl%ln_shifts_boot(i, j)
              triangle_boot(i, j + 1) = trngl%dev_facs(j) * triangle_boot(i, j) + &
                                        trngl%sigmas(j) * sqrt(triangle_boot(i, j)) * triangle_boot(i, j + 1)
            else
              mean = trngl%dev_facs(j) * triangle_boot(i, j)
              sd = trngl%sigmas(j) * sqrt(triangle_boot(i, j))
              triangle_boot(i, j + 1) = mean + sd * trngl%resampled_resids(i, j)
            end if

            if (triangle_boot(i, j + 1) <= 0) then
              status = FAILURE
              exit uncond_boot_loop
            end if
          end do
        end do uncond_boot_loop
      end if

      if (status == FAILURE) then
        cycle main_loop
      end if
      call trngl%fit_cl_boot(triangle_boot, use_mask=.false.)

      do i_sim = 1, n_sim
        call trngl%resample_sim()
        triangle_sim = trngl%triangle
        sim_loop: do i_diag = 1, trngl%n_dev
          do i = i_diag + 1, trngl%n_dev
            j = trngl%n_dev + i_diag + 1 - i
            if (resid_type == LOG_NORM) then
              ! triangle_sim temporarily holds value of epsilon
              triangle_sim(i, j) = exp(trngl%resampled_resids(i - 1, j - 1) * trngl%ln_sigmas_sim(i - 1, j - 1) + &
                                       trngl%ln_means_sim(i - 1, j - 1)) - trngl%ln_shifts_sim(i - 1, j - 1)

              triangle_sim(i, j) = trngl%dev_facs_boot(j - 1) * triangle_sim(i, j - 1) + &
                                   trngl%sigmas_boot(j - 1) * sqrt(triangle_sim(i, j - 1)) * triangle_sim(i, j)

            else
              mean = triangle_sim(i, j - 1) * trngl%dev_facs_boot(j - 1)
              sd = trngl%sigmas_boot(j - 1) * sqrt(triangle_sim(i, j - 1))
              triangle_sim(i, j) = mean + sd * trngl%resampled_resids(i - 1, j - 1)
            end if
            if (triangle_sim(i, j) <= 0 .or. isnan(triangle_sim(i, j))) then
              status = FAILURE
              exit sim_loop
            end if
          end do
        end do sim_loop

        if (status == FAILURE) then
          cycle main_loop
        end if
        reserve(n_sim * (i_boot - 1) + i_sim) = sum(triangle_sim(:, trngl%n_dev)) - sum(get_latest(triangle_sim))
      end do

      i_boot = i_boot + 1
      if (progress) call pgb_incr(pgb, n_sim)
    end do main_loop
  end function mack_resid_boot

  !> @brief Perform a Mack pairs bootstrap on @p trngl
  !! @param[in, out] trngl Cumulative claims triangle.
  !! @param[in] n_boot Number of bootstrap simulations.
  !! @return reserve An array of bootstrapped reserve estimates.
  function mack_pairs_boot(trngl, n_boot, n_sim, pgb) result(reserve)
    integer, intent(in) :: n_boot, n_sim
    type(t_mack_triangle), intent(inout) :: trngl
    type(c_ptr), intent(in), value :: pgb

    real(c_double) :: reserve(n_boot * n_sim)
    integer :: i, j, k, i_diag, i_boot, i_sim, i_thread, n_rows, n_obs
    real(c_double) :: mean, sd
    real(c_double) :: triangle_sim(trngl%n_dev, trngl%n_dev)
    integer :: status

    integer, allocatable :: pair_idxs(:), resampled_pair_idxs(:)
    real(c_double), allocatable :: resampled_pairs(:, :)
    logical :: progress

    progress = c_associated(pgb)
    i_thread = omp_get_thread_num()

    i_boot = 1
    main_loop: do while (i_boot <= n_boot)
      do j = 1, trngl%n_dev - 1
        n_rows = trngl%n_dev - j
        n_obs = count(trngl%obs_mask(:, j + 1))

        allocate (pair_idxs(n_obs))
        allocate (resampled_pair_idxs(n_rows))
        allocate (resampled_pairs(n_rows, 2), source=0._c_double)

        k = 1
        do i = 1, n_rows
          if (trngl%obs_mask(i, j + 1)) then
            pair_idxs(k) = i
            k = k + 1
          end if
        end do
        do i = 1, n_rows
          resampled_pair_idxs(i) = pair_idxs(1 + int(n_obs * runif()))
        end do
        resampled_pairs(1:n_rows, :) = trngl%triangle(resampled_pair_idxs(1:n_rows), j:(j + 1))

        trngl%dev_facs_boot(j) = sum(resampled_pairs(1:n_rows, 2)) / sum(resampled_pairs(1:n_rows, 1))
        if (j < trngl%n_dev - 1) then
          trngl%sigmas_boot(j) = sqrt(sum(trngl%triangle(1:n_rows, j) * (trngl%triangle(1:n_rows, j + 1) / &
                                                           trngl%triangle(1:n_rows, j) - trngl%dev_facs_boot(j))**2) / (n_rows - 1))
        else
          trngl%sigmas_boot(j) = extrapolate_sigma(trngl%sigmas_boot, j)
        end if

        deallocate (pair_idxs)
        deallocate (resampled_pair_idxs)
        deallocate (resampled_pairs)
      end do

      do i_sim = 1, n_sim
        triangle_sim = trngl%triangle
        status = SUCCESS
        do i_diag = 1, trngl%n_dev - 1
          do i = i_diag + 1, trngl%n_dev
            j = trngl%n_dev + i_diag + 1 - i
            mean = trngl%dev_facs_boot(j - 1) * triangle_sim(i, j - 1)
            sd = trngl%sigmas_boot(j - 1) * sqrt(triangle_sim(i, j - 1))
            triangle_sim(i, j) = rnorm(mean, sd)
            if (triangle_sim(i, j) <= 0) then
              status = FAILURE
            end if
          end do
        end do

        if (status == FAILURE) cycle main_loop
        reserve(n_sim * (i_boot - 1) + i_sim) = sum(triangle_sim(:, trngl%n_dev)) - sum(get_latest(triangle_sim))
      end do

      i_boot = i_boot + 1
      if (progress) call pgb_incr(pgb, n_sim)
    end do main_loop
  end function mack_pairs_boot

  subroutine mack_param_boot_cpp(n_dev, triangle, n_boot, n_sim, cond, dist, mask, reserve, &
                                 pgb) bind(c, name="mack_param_boot")
    integer(c_int), intent(in), value :: n_dev
    real(c_double), intent(in) :: triangle(n_dev, n_dev)
    integer(c_int), intent(in), value :: n_boot, n_sim
    logical(c_bool), intent(in), value :: cond
    integer(c_int), intent(in), value :: dist
    logical(c_bool), intent(in) :: mask(n_dev, n_dev)
    real(c_double), intent(out) :: reserve(n_boot * n_sim)
    type(c_ptr), intent(in), value :: pgb

    integer :: status
    type(t_mack_triangle) :: trngl

    call trngl%init(triangle, logical(mask))
    reserve = mack_param_boot(trngl, n_boot, n_sim, logical(cond), dist, pgb, status)
  end subroutine mack_param_boot_cpp

  subroutine mack_resid_boot_cpp(n_dev, triangle, n_boot, n_sim, cond, resid_type, mask, reserve, &
                                 pgb) bind(c, name="mack_resid_boot")
    integer(c_int), intent(in), value :: n_dev
    real(c_double), intent(in) :: triangle(n_dev, n_dev)
    integer(c_int), intent(in), value :: n_boot, n_sim
    logical(c_bool), intent(in), value :: cond
    integer(c_int), intent(in), value :: resid_type
    logical(c_bool), intent(in) :: mask(n_dev, n_dev)
    real(c_double), intent(out) :: reserve(n_boot * n_sim)
    type(c_ptr), intent(in), value :: pgb

    integer :: status
    type(t_mack_triangle) :: trngl

    call trngl%init(triangle, logical(mask))
    reserve = mack_resid_boot(trngl, n_boot, n_sim, logical(cond), resid_type, pgb, status)
  end subroutine mack_resid_boot_cpp

  subroutine mack_pairs_boot_cpp(n_dev, triangle, n_boot, n_sim, mask, reserve, pgb) bind(c, name="mack_pairs_boot")
    integer(c_int), intent(in), value :: n_dev
    real(c_double), intent(in) :: triangle(n_dev, n_dev)
    integer(c_int), intent(in), value :: n_boot, n_sim
    logical(c_bool), intent(in) :: mask(n_dev, n_dev)
    real(c_double), intent(out) :: reserve(n_boot * n_sim)
    type(c_ptr), intent(in), value :: pgb

    type(t_mack_triangle) :: trngl

    call trngl%init(triangle, logical(mask))
    call trngl%fit_cl(use_mask=.false.)
    reserve = mack_pairs_boot(trngl, n_boot, n_sim, pgb)
  end subroutine mack_pairs_boot_cpp
end module mod_mack
