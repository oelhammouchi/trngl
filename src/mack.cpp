#include <RcppArmadillo.h>
#include <RcppThread.h>
#include <spdlog/fmt/fmt.h>

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
options::Status mack_param_boot_cpp(int n_dev, double* triangle, int n_boot,
                                    int n_sim, bool cond, options::Dist dist,
                                    bool* mask, double* reserve, void* pgb);
options::Status mack_resid_boot_cpp(int n_dev, double* triangle, int n_boot,
                                    int n_sim, bool cond,
                                    options::ResidType resid_type, bool* mask,
                                    double* reserve, void* pgb);
options::Status mack_pairs_boot_cpp(int n_dev, double* triangle, int n_boot,
                                    int n_sim, bool* mask, double* reserve,
                                    void* pgb);
}

// [[Rcpp::export(.mackParamBoot)]]
Rcpp::List mackParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist,
                         bool cond, int n_boot = 1e3, int n_sim = 1e3,
                         bool progress = true) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    Mask mask(triangle.n_rows);
    arma::vec reserve(n_boot * n_sim);

    CliProgressBar pb;
    Progress pgb(n_boot * n_sim, progress, pb);

    mack_param_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim, cond,
                        options::dist_mapping.at(dist), mask.begin(),
                        reserve.begin(), &pgb);

    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";
    return res;
};

// [[Rcpp::export(.mackResidBoot)]]
Rcpp::List mackResidBoot(Rcpp::NumericMatrix trngl, Rcpp::String resid_type,
                         bool cond, int n_boot = 1e3, int n_sim = 1e3,
                         bool progress = true) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    arma::vec reserve(n_boot * n_sim);
    Mask mask = Mask(triangle.n_rows);

    CliProgressBar pb;
    Progress pgb(n_boot * n_sim, progress, pb);

    mack_resid_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim, cond,
                        options::resid_type_mapping.at(resid_type),
                        mask.begin(), reserve.begin(), &pgb);
    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";
    return res;
};

// [[Rcpp::export(.mackPairsBoot)]]
Rcpp::List mackPairsBoot(Rcpp::NumericMatrix trngl, int n_boot = 1e3,
                         int n_sim = 1e3, bool progress = true) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    triangle.replace(arma::datum::nan, 0);
    arma::vec reserve(n_boot * n_sim);
    Mask mask(triangle.n_rows);

    CliProgressBar pb;
    Progress pgb(n_boot * n_sim, progress, pb);

    mack_pairs_boot_cpp(triangle.n_rows, triangle.begin(), n_boot, n_sim,
                        mask.begin(), reserve.begin(), &pgb);
    Rcpp::List res;
    res["reserve"] = reserve;
    res["nboot"] = n_boot;
    res.attr("class") = "boot.res";
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
                shared(reserves, col_idx, col_mapping_, pgb, pb, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, resid_type, cond, progress)
            // clang-format on
            for (int i = 0; i < n_dev; i++) {
                for (int j = 1; j < n_dev - i; j++) {
                    if (status == options::kFailure) continue;

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

                    if (boot_type == options::kPairs ||
                        boot_type == options::kParam)
                        mask_in(0, n_dev - 1) = true;

                    options::Status status_;
                    switch (boot_type) {
                        case options::kPairs: {
                            status_ = mack_pairs_boot_cpp(
                                n_dev, triangle.begin(), n_boot, n_sim,
                                mask_in.begin(), reserves.colptr(k), &pgb);
                            break;
                        }
                        case options::kParam: {
                            status_ = mack_param_boot_cpp(
                                n_dev, triangle.begin(), n_boot, n_sim, cond,
                                dist_type, mask_in.begin(), reserves.colptr(k),
                                &pgb);
                            break;
                        }
                        case options::kResid: {
                            status_ = mack_resid_boot_cpp(
                                n_dev, triangle.begin(), n_boot, n_sim, cond,
                                resid_type, mask_in.begin(), reserves.colptr(k),
                                &pgb);
                            break;
                        }
                    }

                        // clang-format off
                        #pragma omp critical
                    // clang-format on
                    { status = status_; }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"single", "mack"};
            break;
        }
        case options::kCalendar: {
            Progress pgb(n_dev * n_boot * n_sim, progress, pb);
            reserves = arma::mat(n_boot * n_sim, n_dev);

            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, pb, col_mapping_, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, resid_type, cond, progress)
            // clang-format on
            for (int i_diag = 0; i_diag < n_dev; i_diag++) {
                if (status == options::kFailure) continue;

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

                if (boot_type == options::kPairs ||
                    boot_type == options::kParam)
                    mask_in(0, n_dev - 1) = true;

                options::Status status_;
                switch (boot_type) {
                    case options::kPairs: {
                        status_ = mack_pairs_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim,
                            mask_in.begin(), reserves.colptr(i_diag), &pgb);
                        break;
                    }
                    case options::kParam: {
                        status_ = mack_param_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim, cond,
                            dist_type, mask_in.begin(), reserves.colptr(i_diag),
                            &pgb);
                        break;
                    }
                    case options::kResid: {
                        status_ = mack_resid_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim, cond,
                            resid_type, mask_in.begin(),
                            reserves.colptr(i_diag), &pgb);
                        break;
                    }

                    // clang-format off
                    #pragma omp critical
                        // clang-format on
                        { status = status_; }
                }
            }
            res.attr("class") = Rcpp::CharacterVector{"calendar", "mack"};
            break;
        }
        case options::kOrigin: {
            Progress pgb((n_dev - 1) * n_boot * n_sim, progress, pb);
            reserves = arma::mat(n_boot * n_sim, n_dev - 1);
            // clang-format off
            #pragma omp parallel for num_threads(n_threads) default(none) \
                shared(reserves, pgb, pb, col_mapping_, status) \
                firstprivate(boot_type, mask, triangle, n_boot, n_sim, n_dev) \
                firstprivate(dist_type, resid_type, cond, progress)
            // clang-format on
            for (int i = 0; i < n_dev - 1; i++) {
                if (status == options::kFailure) continue;

                    // clang-format off
                #pragma omp critical
                // clang-format on
                { col_mapping_.insert({i, Rcpp::IntegerVector{i + 1}}); }

                Mask mask_in = mask;
                mask_in.row(i) = false;
                mask_in(i, 0) = true;

                if (boot_type == options::kPairs ||
                    boot_type == options::kParam)
                    mask_in(0, n_dev - 1) = true;

                options::Status status_;
                switch (boot_type) {
                    case options::kPairs: {
                        status_ = mack_pairs_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim,
                            mask_in.begin(), reserves.colptr(i), &pgb);
                        break;
                    }
                    case options::kParam: {
                        status_ = mack_param_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim, cond,
                            dist_type, mask_in.begin(), reserves.colptr(i),
                            &pgb);
                        break;
                    }
                    case options::kResid: {
                        status_ = mack_resid_boot_cpp(
                            n_dev, triangle.begin(), n_boot, n_sim, cond,
                            resid_type, mask_in.begin(), reserves.colptr(i),
                            &pgb);
                        break;
                    }
                }

                    // clang-format off
                    #pragma omp critical
                // clang-format on
                { status = status_; }
            }
            res.attr("class") = Rcpp::CharacterVector{"origin", "mack"};
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

// [[Rcpp::export(.mackPairsSim)]]
Rcpp::List mackPairsSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        int n_boot, int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, true, options::BootType::kPairs,
                   options::Dist::kNormal, options::kStandard);
}

// [[Rcpp::export(.mackParamSim)]]
Rcpp::List mackParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        bool cond, Rcpp::String dist, int n_boot, int n_sim,
                        bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, cond, options::kParam,
                   options::dist_mapping.at(dist), options::kStandard);
}

// [[Rcpp::export(.mackResidSim)]]
Rcpp::List mackResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type,
                        bool cond, Rcpp::String resid_type, int n_boot,
                        int n_sim, bool progress) {
    arma::mat triangle = Rcpp::as<arma::mat>(trngl);
    return mackSim(triangle, options::sim_type_mapping.at(sim_type), n_boot,
                   n_sim, progress, cond, options::kResid, options::kNormal,
                   options::resid_type_mapping.at(resid_type));
}
