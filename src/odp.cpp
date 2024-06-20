#include <RcppArmadillo.h>
#include <spdlog/fmt/fmt.h>

#include <progress.hpp>
#include <progress_bar.hpp>

#include "cli_progress_bar.h"
#include "config.h"
#include "mask.h"

extern "C" {

options::Status odp_param_boot(int n_dev, double* triangle, int n_boot,
                               int n_sim, options::Dist dist, bool* mask,
                               double* reserve, void* pgb);

options::Status odp_resid_boot(int n_dev, double* triangle, int n_boot,
                               int n_sim, bool* mask, double* reserve,
                               void* pgb);
}

// [[Rcpp::export(.odpParamBoot)]]
Rcpp::List odpParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist,
                        int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    Mask mask(triangle.n_rows);
    arma::vec reserve(n_boot * n_sim);

    CliProgressBar pb;
    Progress pgb(n_boot * n_sim, progress, pb);
    odp_param_boot(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                   options::dist_mapping.at(dist), mask.begin(),
                   reserve.begin(), &pgb);

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

    CliProgressBar pb;
    Progress pgb(n_boot * n_sim, progress, pb);
    odp_resid_boot(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                   mask.begin(), reserve.begin(), &pgb);

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
    int n_pts = boot_type == options::kResid
                    ? (std::pow(n_dev, 2) + n_dev) / 2
                    : (std::pow(n_dev, 2) + n_dev) / 2 - 2;

    Mask mask(n_dev);
    arma::mat reserves;
    options::Status status = options::kSuccess;

    Rcpp::List res;
    res["n_boot"] = n_boot;
    res["n_sim"] = n_sim;
    std::map<int, Rcpp::IntegerVector> col_mapping_;

    CliProgressBar pb;
    int n_threads = TrnglRng::get().n_threads();

    switch (sim_type) {
        case options::kSingle: {
            Progress pgb(n_pts * n_boot * n_sim, progress, pb);
            reserves = arma::mat(n_boot * n_sim, n_pts);
            int col_idx = 0;

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) collapse(2) default(none) \
                shared(reserves, col_idx, col_mapping_, pgb, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i = 0; i < n_dev; i++) {
                for (int j = 0; j < n_dev - i; j++) {
                    if (boot_type == options::kParam) {
                        if ((i == 0 && j == n_dev - 1) ||
                            (i == n_dev - 1 && j == 0)) {
                            continue;
                        }
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

                    Mask mask_in(n_dev);
                    mask_in = mask;
                    mask_in(i, j) = false;

                    options::Status status_;
                    switch (boot_type) {
                        case options::kParam: {
                            status_ = odp_param_boot(n_dev, triangle.begin(),
                                                     n_boot, n_sim, dist_type,
                                                     mask_in.begin(),
                                                     reserves.colptr(k), &pgb);
                            break;
                        }
                        case options::kResid: {
                            status_ = odp_resid_boot(
                                n_dev, triangle.begin(), n_boot, n_sim,
                                mask_in.begin(), reserves.colptr(k), &pgb);
                            break;
                        }
                        case options::kPairs:
                            break;
                    }

                        // clang-format off
                        #pragma omp critical
                    // clang-format on
                    { status = status_; }
                }
            }

            res.attr("class") = Rcpp::CharacterVector{"single", "odp"};
            break;
        }
        case options::kCalendar: {
            Progress pgb(n_dev * n_boot * n_sim, progress, pb);
            reserves = arma::mat(n_boot * n_sim, n_dev);

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, col_mapping_, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i_diag = 0; i_diag < n_dev; i_diag++) {
                // clang-format off
                #pragma omp critical
                // clang-format on
                {
                    col_mapping_.insert(
                        {i_diag, Rcpp::IntegerVector{i_diag + 1}});
                }

                Mask mask_in = mask;
                for (int j = 1; j < n_dev; j++) {
                    int i = n_dev - i_diag - j - 1;
                    if (i < 0) continue;
                    mask_in(i, j) = false;
                }

                options::Status status_;
                if (boot_type == options::kParam) {
                    mask_in(0, n_dev - 1) = true;
                    mask_in(n_dev - 1, 0) = true;
                }
                switch (boot_type) {
                    case options::kParam: {
                        status_ = odp_param_boot(
                            n_dev, triangle.begin(), n_boot, n_sim, dist_type,
                            mask_in.begin(), reserves.colptr(i_diag), &pgb);
                        break;
                    }
                    case options::kResid: {
                        status_ = odp_resid_boot(n_dev, triangle.begin(),
                                                 n_boot, n_sim, mask_in.begin(),
                                                 reserves.colptr(i_diag), &pgb);
                        break;
                    }
                    case options::kPairs:
                        break;
                }

                    // clang-format off
                    #pragma omp critical
                // clang-format on
                { status = status_; }
            }

            res.attr("class") = Rcpp::CharacterVector{"calendar", "odp"};
            break;
        }
        case options::kOrigin: {
            Progress pgb((n_dev - 1) * n_boot * n_sim, progress, pb);
            reserves = arma::mat(n_boot * n_sim, n_dev - 1);

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, col_mapping_, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, progress)
            // clang-format on
            for (int i = 0; i < n_dev - 1; i++) {
                // clang-format off
                #pragma omp critical
                // clang-format on
                { col_mapping_.insert({i, Rcpp::IntegerVector{i + 1}}); }

                Mask mask_in = mask;
                mask_in.row(i) = false;
                mask_in(i, 0) = true;

                if (i == 0 && boot_type == options::kParam) {
                    mask_in(0, n_dev - 1) = true;
                }

                options::Status status_;
                switch (boot_type) {
                    case options::kParam: {
                        status_ = odp_param_boot(
                            n_dev, triangle.begin(), n_boot, n_sim, dist_type,
                            mask_in.begin(), reserves.colptr(i), &pgb);
                        break;
                    }
                    case options::kResid: {
                        status_ = odp_resid_boot(n_dev, triangle.begin(),
                                                 n_boot, n_sim, mask_in.begin(),
                                                 reserves.colptr(i), &pgb);
                        break;
                    }
                    case options::kPairs:
                        break;
                }

                    // clang-format off
                    #pragma omp critical
                // clang-format on
                { status = status_; }
            }
            res.attr("class") = Rcpp::CharacterVector{"origin", "odp"};
            break;
        }
    }

    if (status == options::kFailure) {
        Rcpp::stop(
            "Could not complete the simulation before reaching "
            "the maximum number of failures. Most likely, "
            "this means the input triangle is highly "
            "pathological.");
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
    res["status"] = status == options::kFailure ? "failure" : "success";
    res.attr("class") = res_class;

    return res;
};

// [[Rcpp::export(.odpParamSim)]]
Rcpp::List odpParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                       Rcpp::String dist, int n_boot, int n_sim,
                       bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return odpSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                  n_sim, progress, options::kParam,
                  options::dist_mapping.at(dist));
}

// [[Rcpp::export(.odpResidSim)]]
Rcpp::List odpResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                       int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return odpSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                  n_sim, progress, options::kResid, options::kNormal);
}