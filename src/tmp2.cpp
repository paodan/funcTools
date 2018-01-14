#include <Rcpp.h>
using namespace Rcpp;

//' testing
//' @param x asdf
//' @return asdfasdf
//' @keywords internal
//'
// [[Rcpp::interfaces(r, cpp)]]
int times1(int x) {
  return x * 1;
}
