#include <RcppArmadillo.h>
#include <rcpp_sink.h>
#include <spdlog/fmt/ostr.h>
#include <spdlog/sinks/rotating_file_sink.h>
#include <spdlog/spdlog.h>

#include <memory>
#include <string>

extern "C" {
void log_matrix(int n_rows, int n_cols, double* mat, char* msg,
                spdlog::level::level_enum level);
void log_int(int it, char* msg, spdlog::level::level_enum level);
}

template <>
struct fmt::formatter<arma::mat> : formatter<string_view> {
    auto format(arma::mat mat, format_context& ctx) const;
};

auto fmt::formatter<arma::mat>::format(arma::mat mat,
                                       format_context& ctx) const {
    std::stringbuf buffer;
    std::ostream stream(&buffer);
    stream.flags(std::ios::fixed);
    stream.precision(2);
    stream.width(10);
    mat.raw_print(stream);

    return formatter<basic_string_view<char>, char>::format(
        {buffer.str().c_str(), buffer.str().size()}, ctx);
}

// [[Rcpp::export]]
void initLogger() {
    std::shared_ptr<spdlog::logger> console_logger =
        spdlog::get("trngl-console") ? spdlog::get("trngl-console")
                                     : spdlog::r_sink_mt("trngl-console");
    spdlog::set_pattern("[%H:%M:%S.%f] [%L] [thread %t] %v");

    std::shared_ptr<spdlog::logger> rotating_logger =
        spdlog::get("trngl-file")
            ? spdlog::get("trngl-file")
            : spdlog::rotating_logger_mt("trngl-file", "logs/rotating.txt",
                                         1048576 * 5, 3);

    spdlog::set_default_logger(console_logger);
}

//' @export
// [[Rcpp::export]]
void setLogger(std::string logger) {
    static std::vector<std::string> LOGGERS{"trngl-console", "trngl-file"};

    std::vector<int> comp(LOGGERS.size());
    std::transform(LOGGERS.begin(), LOGGERS.end(), comp.begin(),
                   [&](std::string str) { return str.compare(logger); });

    if (std::none_of(comp.begin(), comp.end(), [&](int i) { return i == 0; })) {
        throw std::runtime_error("Unknown logger");
    };

    spdlog::set_default_logger(spdlog::get(logger));
}

//' @export
// [[Rcpp::export]]
void setLogLevel(std::string level) {
    spdlog::set_level(spdlog::level::from_str(level));
}

void log_matrix(int n_rows, int n_cols, double* mat, char* msg,
                spdlog::level::level_enum level) {
    arma::mat mat_(mat, n_rows, n_cols);
    spdlog::log(level, msg, mat_);
}

void log_int(int it, char* msg, spdlog::level::level_enum level) {
    spdlog::log(level, msg, it);
}