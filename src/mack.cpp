#include <RcppArmadillo.h>

#include <algorithm>
#include <numeric>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <trng/uniform01_dist.hpp>
#include <trng/yarn2.hpp>
#include <vector>

#include "cli_progress_bar.h"
#include "config.h"
#include "mask.h"

extern "C" {
void mack_param_boot_cpp(int n_dev, double* triangle, int n_boot, int n_sim,
                         bool cond, options::Dist dist, bool* mask,
                         double* reserve, void* pgb);
void mack_resid_boot_cpp(int n_dev, double* triangle, int n_boot, int n_sim,
                         bool cond, options::ResidType resid_type, bool* mask,
                         double* reserve, void* pgb);
void mack_pairs_boot_cpp(int n_dev, double* triangle, int n_boot, int n_sim,
                         bool* mask, double* reserve, void* pgb);
}

//' @export
// [[Rcpp::export(.mackParamBoot)]]
Rcpp::List mackParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist,
                         bool cond, int n_boot = 1e3, int n_sim = 1e3,
                         bool progress = true) {
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

    mack_param_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim, cond,
                        options::dist_mapping.at(dist), mask.begin(),
                        reserve.begin(), pgb);

    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";

    if (progress) {
        delete pgb;
        delete pb;
    }

    return res;
};

//' @export
// [[Rcpp::export(.mackResidBoot)]]
Rcpp::List mackResidBoot(Rcpp::NumericMatrix trngl, Rcpp::String resid_type,
                         bool cond, int n_boot = 1e3, int n_sim = 1e3,
                         bool progress = true) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    arma::vec reserve(n_boot * n_sim);
    Mask mask = Mask(triangle.n_rows);

    CliProgressBar* pb;
    Progress* pgb;
    if (progress) {
        pb = new CliProgressBar;
        pgb = new Progress(n_boot * n_sim, true, *pb);
    } else {
        pb = nullptr;
        pgb = nullptr;
    }

    mack_resid_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim, cond,
                        options::resid_type_mapping.at(resid_type),
                        mask.begin(), reserve.begin(), pgb);
    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";

    if (progress) {
        delete pgb;
        delete pb;
    }

    return res;
};

//' @export
// [[Rcpp::export(.mackPairsBoot)]]
Rcpp::List mackPairsBoot(Rcpp::NumericMatrix trngl, int n_boot = 1e3,
                         int n_sim = 1e3, bool progress = true) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    arma::vec reserve(n_boot * n_sim);
    Mask mask(triangle.n_rows);

    Progress* pgb;
    CliProgressBar* pb;
    if (progress) {
        pb = new CliProgressBar;
        pgb = new Progress(n_boot * n_sim, true, *pb);
    } else {
        pb = nullptr;
        pgb = nullptr;
    }

    mack_pairs_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                        mask.begin(), reserve.begin(), pgb);
    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";

    if (progress) {
        delete pgb;
        delete pb;
    }

    return res;
};

Rcpp::List mackSim(arma::mat triangle, options::SimType sim_type, int n_boot,
                   int n_sim, bool progress, bool cond,
                   options::BootType boot_type, options::Dist dist_type,
                   options::ResidType resid_type) {
    triangle.replace(arma::datum::nan, 0);
    int n_dev = triangle.n_rows;
    int n_pts = (std::pow((n_dev - 1), 2) + (n_dev - 1)) / 2;

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
                firstprivate(dist_type, resid_type, cond, progress)
            // clang-format on
            for (int i = 0; i < n_dev; i++) {
                for (int j = 1; j < n_dev - i; j++) {
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

                        if (boot_type == options::PAIRS ||
                            boot_type == options::PARAM)
                            mask_in(0, n_dev - 1) = true;
                        switch (boot_type) {
                            case options::PAIRS: {
                                mack_pairs_boot_cpp(
                                    n_dev, triangle.begin(), n_boot, n_sim,
                                    mask_in.begin(), reserves.colptr(k), pgb);
                                break;
                            }
                            case options::PARAM: {
                                mack_param_boot_cpp(n_dev, triangle.begin(),
                                                    n_boot, n_sim, cond,
                                                    dist_type, mask_in.begin(),
                                                    reserves.colptr(k), pgb);
                                break;
                            }
                            case options::RESID: {
                                mack_resid_boot_cpp(n_dev, triangle.begin(),
                                                    n_boot, n_sim, cond,
                                                    resid_type, mask_in.begin(),
                                                    reserves.colptr(k), pgb);
                                break;
                            }
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"single", "mack"};
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
                firstprivate(dist_type, resid_type, cond, progress)
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

                    if (boot_type == options::PAIRS ||
                        boot_type == options::PARAM)
                        mask_in(0, n_dev - 1) = true;
                    switch (boot_type) {
                        case options::PAIRS: {
                            mack_pairs_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, mask_in.begin(),
                                                reserves.colptr(i_diag), pgb);
                            break;
                        }
                        case options::PARAM: {
                            mack_param_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, cond, dist_type,
                                                mask_in.begin(),
                                                reserves.colptr(i_diag), pgb);
                            break;
                        }
                        case options::RESID: {
                            mack_resid_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, cond, resid_type,
                                                mask_in.begin(),
                                                reserves.colptr(i_diag), pgb);
                            break;
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"calendar", "mack"};
            break;
        }
        case options::ORIGIN: {
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
                firstprivate(dist_type, resid_type, cond, progress)
            // clang-format on
            for (int i = 0; i < n_dev; i++) {
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
                    if (boot_type == options::PAIRS ||
                        boot_type == options::PARAM)
                        mask_in(0, n_dev - 1) = true;
                    switch (boot_type) {
                        case options::PAIRS: {
                            mack_pairs_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, mask_in.begin(),
                                                reserves.colptr(i), pgb);
                            break;
                        }
                        case options::PARAM: {
                            mack_param_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, cond, dist_type,
                                                mask_in.begin(),
                                                reserves.colptr(i), pgb);
                            break;
                        }
                        case options::RESID: {
                            mack_resid_boot_cpp(n_dev, triangle.begin(), n_boot,
                                                n_sim, cond, resid_type,
                                                mask_in.begin(),
                                                reserves.colptr(i), pgb);
                            break;
                        }
                    }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"origin", "mack"};
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

//' @export
// [[Rcpp::export(.mackPairsSim)]]
Rcpp::List mackPairsSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, true, options::BootType::PAIRS,
                   options::Dist::NORMAL, options::STANDARD);
}

//' @export
// [[Rcpp::export(.mackParamSim)]]
Rcpp::List mackParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        bool cond, Rcpp::String dist, int n_boot, int n_sim,
                        bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, cond, options::PARAM,
                   options::dist_mapping.at(dist), options::STANDARD);
}

//' @export
// [[Rcpp::export(.mackResidSim)]]
Rcpp::List mackResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        bool cond, Rcpp::String resid_type, int n_boot,
                        int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, cond, options::RESID, options::NORMAL,
                   options::resid_type_mapping.at(resid_type));
}
