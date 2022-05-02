#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame spiral(int coils, int points, double radius, double inner_radius) {
#include <cmath>
  // create the columns
  double n = coils * points;
  double awayStep = radius/n;
  double aroundStep = coils/n;
  double aroundRadians = aroundStep * 2 * M_PI;
  NumericVector x(n);
  NumericVector y(n);
  for(int i = 0; i < n; ++i) {
    double away = awayStep * i + inner_radius;
    // double away0 = away + dist[i];
    double around = i * aroundRadians;
    x[i] = cos(around) * away;
    y[i] = sin(around) * away;
  }
  // return a new data frame
  return DataFrame::create(_["x"]= x, _["y"]= y);
}
