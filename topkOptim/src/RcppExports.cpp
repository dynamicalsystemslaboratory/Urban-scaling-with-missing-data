// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// lossFn_cpp
NumericVector lossFn_cpp(NumericVector z, int n, NumericVector ln_xr, double sum_ln_x, NumericVector k);
RcppExport SEXP _topkOptim_lossFn_cpp(SEXP zSEXP, SEXP nSEXP, SEXP ln_xrSEXP, SEXP sum_ln_xSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ln_xr(ln_xrSEXP);
    Rcpp::traits::input_parameter< double >::type sum_ln_x(sum_ln_xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(lossFn_cpp(z, n, ln_xr, sum_ln_x, k));
    return rcpp_result_gen;
END_RCPP
}
// optim
NumericVector optim(NumericVector xr, NumericVector x, NumericVector z0, int steps, NumericVector maxZ, int maximize, int verbose);
RcppExport SEXP _topkOptim_optim(SEXP xrSEXP, SEXP xSEXP, SEXP z0SEXP, SEXP stepsSEXP, SEXP maxZSEXP, SEXP maximizeSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xr(xrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z0(z0SEXP);
    Rcpp::traits::input_parameter< int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type maxZ(maxZSEXP);
    Rcpp::traits::input_parameter< int >::type maximize(maximizeSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(optim(xr, x, z0, steps, maxZ, maximize, verbose));
    return rcpp_result_gen;
END_RCPP
}
// optim_aggregate
NumericVector optim_aggregate(DataFrame x_data, DataFrame index, int maximize, int verbose);
RcppExport SEXP _topkOptim_optim_aggregate(SEXP x_dataSEXP, SEXP indexSEXP, SEXP maximizeSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x_data(x_dataSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type index(indexSEXP);
    Rcpp::traits::input_parameter< int >::type maximize(maximizeSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_aggregate(x_data, index, maximize, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_topkOptim_lossFn_cpp", (DL_FUNC) &_topkOptim_lossFn_cpp, 5},
    {"_topkOptim_optim", (DL_FUNC) &_topkOptim_optim, 7},
    {"_topkOptim_optim_aggregate", (DL_FUNC) &_topkOptim_optim_aggregate, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_topkOptim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
