// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// setTrnglThreads
void setTrnglThreads(int n_threads);
RcppExport SEXP _trngl_setTrnglThreads(SEXP n_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n_threads(n_threadsSEXP);
    setTrnglThreads(n_threads);
    return R_NilValue;
END_RCPP
}
// getTrnglThreads
int getTrnglThreads();
RcppExport SEXP _trngl_getTrnglThreads() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getTrnglThreads());
    return rcpp_result_gen;
END_RCPP
}
// setTrnglSeed
void setTrnglSeed(int seed);
RcppExport SEXP _trngl_setTrnglSeed(SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    setTrnglSeed(seed);
    return R_NilValue;
END_RCPP
}
// getTrnglSeed
int getTrnglSeed();
RcppExport SEXP _trngl_getTrnglSeed() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getTrnglSeed());
    return rcpp_result_gen;
END_RCPP
}
// mackParamBoot
Rcpp::List mackParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist, bool cond, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackParamBoot(SEXP trnglSEXP, SEXP distSEXP, SEXP condSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type dist(distSEXP);
    Rcpp::traits::input_parameter< bool >::type cond(condSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackParamBoot(trngl, dist, cond, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// mackResidBoot
Rcpp::List mackResidBoot(Rcpp::NumericMatrix trngl, Rcpp::String resid_type, bool cond, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackResidBoot(SEXP trnglSEXP, SEXP resid_typeSEXP, SEXP condSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type resid_type(resid_typeSEXP);
    Rcpp::traits::input_parameter< bool >::type cond(condSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackResidBoot(trngl, resid_type, cond, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// mackPairsBoot
Rcpp::List mackPairsBoot(Rcpp::NumericMatrix trngl, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackPairsBoot(SEXP trnglSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackPairsBoot(trngl, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// mackPairsSim
Rcpp::List mackPairsSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackPairsSim(SEXP trnglSEXP, SEXP sim_typeSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type sim_type(sim_typeSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackPairsSim(trngl, sim_type, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// mackParamSim
Rcpp::List mackParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type, bool cond, Rcpp::String dist, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackParamSim(SEXP trnglSEXP, SEXP sim_typeSEXP, SEXP condSEXP, SEXP distSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type sim_type(sim_typeSEXP);
    Rcpp::traits::input_parameter< bool >::type cond(condSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type dist(distSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackParamSim(trngl, sim_type, cond, dist, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// mackResidSim
Rcpp::List mackResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type, bool cond, Rcpp::String resid_type, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_mackResidSim(SEXP trnglSEXP, SEXP sim_typeSEXP, SEXP condSEXP, SEXP resid_typeSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type sim_type(sim_typeSEXP);
    Rcpp::traits::input_parameter< bool >::type cond(condSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type resid_type(resid_typeSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(mackResidSim(trngl, sim_type, cond, resid_type, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// odpParamBoot
Rcpp::List odpParamBoot(Rcpp::NumericMatrix trngl, Rcpp::String dist, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_odpParamBoot(SEXP trnglSEXP, SEXP distSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type dist(distSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(odpParamBoot(trngl, dist, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// odpResidBoot
Rcpp::List odpResidBoot(Rcpp::NumericMatrix trngl, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_odpResidBoot(SEXP trnglSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(odpResidBoot(trngl, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// odpParamSim
Rcpp::List odpParamSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type, Rcpp::String dist, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_odpParamSim(SEXP trnglSEXP, SEXP sim_typeSEXP, SEXP distSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type sim_type(sim_typeSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type dist(distSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(odpParamSim(trngl, sim_type, dist, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}
// odpResidSim
Rcpp::List odpResidSim(Rcpp::NumericMatrix trngl, Rcpp::String sim_type, int n_boot, int n_sim, bool progress);
RcppExport SEXP _trngl_odpResidSim(SEXP trnglSEXP, SEXP sim_typeSEXP, SEXP n_bootSEXP, SEXP n_simSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type trngl(trnglSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type sim_type(sim_typeSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(odpResidSim(trngl, sim_type, n_boot, n_sim, progress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_trngl_setTrnglThreads", (DL_FUNC) &_trngl_setTrnglThreads, 1},
    {"_trngl_getTrnglThreads", (DL_FUNC) &_trngl_getTrnglThreads, 0},
    {"_trngl_setTrnglSeed", (DL_FUNC) &_trngl_setTrnglSeed, 1},
    {"_trngl_getTrnglSeed", (DL_FUNC) &_trngl_getTrnglSeed, 0},
    {"_trngl_mackParamBoot", (DL_FUNC) &_trngl_mackParamBoot, 6},
    {"_trngl_mackResidBoot", (DL_FUNC) &_trngl_mackResidBoot, 6},
    {"_trngl_mackPairsBoot", (DL_FUNC) &_trngl_mackPairsBoot, 4},
    {"_trngl_mackPairsSim", (DL_FUNC) &_trngl_mackPairsSim, 5},
    {"_trngl_mackParamSim", (DL_FUNC) &_trngl_mackParamSim, 7},
    {"_trngl_mackResidSim", (DL_FUNC) &_trngl_mackResidSim, 7},
    {"_trngl_odpParamBoot", (DL_FUNC) &_trngl_odpParamBoot, 5},
    {"_trngl_odpResidBoot", (DL_FUNC) &_trngl_odpResidBoot, 4},
    {"_trngl_odpParamSim", (DL_FUNC) &_trngl_odpParamSim, 6},
    {"_trngl_odpResidSim", (DL_FUNC) &_trngl_odpResidSim, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_trngl(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
