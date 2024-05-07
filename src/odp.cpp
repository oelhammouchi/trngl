#include <RcppArmadillo.h>

#include <progress.hpp>
#include <progress_bar.hpp>

#include "cli_progress_bar.h"
#include "config.h"
#include "mask.h"

extern "C" {

void odp_param_boot(int n_dev, double* triangle, int n_boot, int n_sim,
                    options::Dist dist, bool* mask, double* reserve, void* pgb);

void odp_resid_boot(int n_dev, double* triangle, int n_boot, int n_sim,
                    bool* mask, double* reserve, void* pgb);
}

// [[Rcpp::export(.odpParamBoot)]]
Rcpp::List odpParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist,
                        int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    Mask mask(triangle.n_rows);
    arma::vec reserve(n_boot * n_sim);

    CliProgressBar* pb;
    Progress* pgb;
    if (progress) {
        pb = new CliProgressBar;
        pgb = new Progress(n_boot * n_sim, true, *pb);
    } else {
        pb = nullptr;
        pgb = nullptr;
    }

    odp_param_boot(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                   options::dist_mapping.at(dist), mask.begin(),
                   reserve.begin(), pgb);

    if (progress) {
        delete pgb;
        delete pb;
    }

    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";
    return res;
};

// [[Rcpp::export(.odpResidBoot)]]
Rcpp::List odpResidBoot(Rcpp::NumericMatrix trngl, int n_boot, int n_sim,
                        bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    Mask mask(triangle.n_rows);
    arma::vec reserve(n_boot * n_sim);

    Progress* pgb;
    CliProgressBar* pb;
    if (progress) {
        pb = new CliProgressBar;
        pgb = new Progress(n_boot * n_sim, true, *pb);
    } else {
        pb = nullptr;
        pgb = nullptr;
    }

    odp_resid_boot(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                   mask.begin(), reserve.begin(), pgb);

    if (progress) {
        delete pgb;
        delete pb;
    }

    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";
    return res;
};

Rcpp::List odpSim(arma::mat triangle, options::SimType sim_type, int n_boot,
                  int n_sim, bool progress, options::BootType boot_type,
                  options::Dist dist_type) {
    triangle.replace(arma::datum::nan, 0);
    int n_dev = triangle.n_rows;
    int n_pts = boot_type == options::RESID
                    ? (std::pow(n_dev, 2) + n_dev) / 2
                    : (std::pow(n_dev, 2) + n_dev) / 2 - 2;

    Mask mask(n_dev);
    arma::mat reserves;

    Rcpp::List res;
    res["n_boot"] = n_boot;
    res["n_sim"] = n_sim;
    std::map<int, Rcpp::IntegerVector> col_mapping_;

    Progress* pgb;
    CliProgressBar* pb;
    int n_threads = TrnglRng::get().n_threads();
    switch (sim_type) {
        case options::SINGLE: {
            if (progress) {
                pb = new CliProgressBar;
                pgb = new Progress(n_pts * n_boot * n_sim, true, *pb);
            } else {
                pb = nullptr;
                pgb = nullptr;
            }

            reserves = arma::mat(n_boot * n_sim, n_pts);
            int col_idx = 0;

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) collapse(2) default(none) \
                shared(reserves, col_idx, col_mapping_, pgb) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i = 0; i < n_dev; i++) {
                for (int j = 0; j < n_dev - i; j++) {
                    if (boot_type == options::PARAM) {
                        if ((i == 0 && j == n_dev - 1) ||
                            (i == n_dev - 1 && j == 0)) {
                            continue;
                        }
                    }

                    bool abort_check;
                    if (progress) {
                        abort_check = !Progress::check_abort();
                    } else {
                        abort_check = true;
                    }

                    int k;
                    // clang-format off
                    #pragma omp atomic capture
                    // clang-format on
                    {
                        k = col_idx;
                        col_idx++;
                    }

                    // clang-format off
                    #pragma omp critical
                    // clang-format on
                    {
                        col_mapping_.insert(
                            {k, Rcpp::IntegerVector{i + 1, j + 1}});
                    }

                    if (abort_check) {
                        Mask mask_in(n_dev);
                        mask_in = mask;
                        mask_in(i, j) = false;

                        switch (boot_type) {
                            case options::PARAM: {
                                odp_param_boot(n_dev, triangle.begin(), n_boot,
                                               n_sim, dist_type,
                                               mask_in.begin(),
                                               reserves.colptr(k), pgb);
                                break;
                            }
                            case options::RESID: {
                                odp_resid_boot(n_dev, triangle.begin(), n_boot,
                                               n_sim, mask_in.begin(),
                                               reserves.colptr(k), pgb);
                                break;
                            }
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"single", "odp"};
            break;
        }
        case options::CALENDAR: {
            if (progress) {
                pb = new CliProgressBar;
                pgb = new Progress(n_dev * n_boot * n_sim, true, *pb);
            } else {
                pb = nullptr;
                pgb = nullptr;
            }

            reserves = arma::mat(n_boot * n_sim, n_dev);

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, col_mapping_) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i_diag = 0; i_diag < n_dev; i_diag++) {
                bool abort_check;
                if (progress) {
                    abort_check = !Progress::check_abort();
                } else {
                    abort_check = true;
                }

                // clang-format off
                #pragma omp critical
                // clang-format on
                {
                    col_mapping_.insert(
                        {i_diag, Rcpp::IntegerVector{i_diag + 1}});
                }

                if (abort_check) {
                    Mask mask_in = mask;
                    for (int j = 1; j < n_dev; j++) {
                        int i = n_dev - i_diag - j - 1;
                        if (i < 0) continue;
                        mask_in(i, j) = false;
                    }

                    if (boot_type == options::PARAM) {
                        mask_in(0, n_dev - 1) = true;
                        mask_in(n_dev - 1, 0) = true;
                    }
                    switch (boot_type) {
                        case options::PARAM: {
                            odp_param_boot(n_dev, triangle.begin(), n_boot,
                                           n_sim, dist_type, mask_in.begin(),
                                           reserves.colptr(i_diag), pgb);
                            break;
                        }
                        case options::RESID: {
                            odp_resid_boot(n_dev, triangle.begin(), n_boot,
                                           n_sim, mask_in.begin(),
                                           reserves.colptr(i_diag), pgb);
                            break;
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"calendar", "odp"};
            break;
        }
        case options::ORIGIN: {
            if (progress) {
                pb = new CliProgressBar;
                pgb = new Progress((n_dev - 1) * n_boot * n_sim, true, *pb);
            } else {
                pb = nullptr;
                pgb = nullptr;
            }

            reserves = arma::mat(n_boot * n_sim, n_dev - 1);
            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, col_mapping_) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i = 0; i < n_dev - 1; i++) {
                bool abort_check;
                if (progress) {
                    abort_check = !Progress::check_abort();
                } else {
                    abort_check = true;
                }

                // clang-format off
                #pragma omp critical
                // clang-format on
                { col_mapping_.insert({i, Rcpp::IntegerVector{i + 1}}); }

                if (abort_check) {
                    Mask mask_in = mask;
                    mask_in.row(i) = false;
                    mask_in(i, 0) = true;

                    if (i == 0 && boot_type == options::PARAM) {
                        mask_in(0, n_dev - 1) = true;
                    }
                    switch (boot_type) {
                        case options::PARAM: {
                            odp_param_boot(n_dev, triangle.begin(), n_boot,
                                           n_sim, dist_type, mask_in.begin(),
                                           reserves.colptr(i), pgb);
                            break;
                        }
                        case options::RESID: {
                            odp_resid_boot(n_dev, triangle.begin(), n_boot,
                                           n_sim, mask_in.begin(),
                                           reserves.colptr(i), pgb);
                            break;
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"origin", "odp"};
            break;
        }
    }

    if (progress) {
        delete pgb;
        delete pb;
    }

    // Assigning to list drops the 'class' attribute for some reason
    Rcpp::CharacterVector res_class = res.attr("class");
    res["reserves"] = reserves;

    int n = col_mapping_.size();
    Rcpp::List col_mapping(n);
    for (int i = 0; i < n; i++) {
        col_mapping[i] = col_mapping_.at(i);
    }
    res["col_mapping"] = col_mapping;
    res.attr("class") = res_class;

    return res;
};

// [[Rcpp::export(.odpParamSim)]]
Rcpp::List odpParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                       Rcpp::String dist, int n_boot, int n_sim,
                       bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return odpSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                  n_sim, progress, options::PARAM,
                  options::dist_mapping.at(dist));
}

// [[Rcpp::export(.odpResidSim)]]
Rcpp::List odpResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                       int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return odpSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                  n_sim, progress, options::RESID, options::NORMAL);
}