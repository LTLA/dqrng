#include <Rcpp.h>
// [[Rcpp::depends(dqrng)]]
#include <dqrng.h>

// [[Rcpp::export]]
bool consecutive_calls(int seed) {
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector u1 = dqrng::dqrunif(10);
  Rcpp::NumericVector u2 = dqrng::dqrunif(10);
  return Rcpp::is_true(Rcpp::all(u1 == u2));
}

// [[Rcpp::export]]
bool seed_uniform(int seed) {
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector u1 = dqrng::dqrunif(10);
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector u2 = dqrng::dqrunif(10);
  return Rcpp::is_true(Rcpp::all(u1 == u2));
}

// [[Rcpp::export]]
bool seed_uniform_scalar(int seed) {
  dqrng::dqset_seed(seed);
  double u1 = dqrng::dquniform();
  dqrng::dqset_seed(seed);
  double u2 = dqrng::dquniform();
  return u1 == u2;
}

// [[Rcpp::export]]
bool seed_normal(int seed) {
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector n1 = dqrng::dqrnorm(10);
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector n2 = dqrng::dqrnorm(10);
  return Rcpp::is_true(Rcpp::all(n1 == n2));
}

// [[Rcpp::export]]
bool seed_normal_scalar(int seed) {
  dqrng::dqset_seed(seed);
  double n1 = dqrng::dqnormal();
  dqrng::dqset_seed(seed);
  double n2 = dqrng::dqnormal();
  return n1 == n2;
}

// [[Rcpp::export]]
bool seed_exponential(int seed) {
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector e1 = dqrng::dqrexp(10);
  dqrng::dqset_seed(seed);
  Rcpp::NumericVector e2 = dqrng::dqrexp(10);
  return Rcpp::is_true(Rcpp::all(e1 == e2));
}

// [[Rcpp::export]]
bool seed_exponential_scalar(int seed) {
  dqrng::dqset_seed(seed);
  double u1 = dqrng::dqexponential();
  dqrng::dqset_seed(seed);
  double u2 = dqrng::dqexponential();
  return u1 == u2;
}
