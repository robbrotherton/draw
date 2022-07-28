#include <Rcpp.h>
using namespace Rcpp;

// // declare getPoints function
NumericVector getPoints(NumericVector input_points, double time);

// [[Rcpp::export]]
DataFrame bezier(DataFrame points, int n_out = 100) {

  NumericVector t(n_out);
  NumericVector newX(n_out);
  NumericVector newY(n_out);

  // first, need to create the sequence of 'time' values
  // this is the values between 0 and 1 that will become points
  double t_increment = 1.0/(n_out-1);

  for(int i = 0; i < n_out; i++) {

    t[i] = t_increment * i;

  }

  // now, for each 'time' value, we do something
  for(int j = 0; j < n_out; j++) {

    // each time we start with the original points
    NumericVector xPoints = points["x"];
    NumericVector yPoints = points["y"];

    // as long as there's more than one coordinate (i.e. more than one line)
    // we need to reduce the number of lines, until just one remains
    while(xPoints.size() > 1) {

      xPoints = getPoints(xPoints, t[j]);
      yPoints = getPoints(yPoints, t[j]);

    }

    // now there's just one point; that's the new value on the curve
    newX[j] = xPoints[0];
    newY[j] = yPoints[0];

  }

  return DataFrame::create(_["x"]= newX, _["y"]= newY);

}

// define getPoints
NumericVector getPoints(NumericVector input_points, double time) {

  int nSegs = input_points.size() - 1;
  NumericVector out(nSegs);

  for (int k = 0; k < nSegs; k++) {
    out[k] = input_points[k] + (input_points[k + 1] - input_points[k]) * time;
  }

  return out;

}

/*** R
# points <- data.frame(x = c(0,.2,.8,1),
#                      y = c(0,0,1,1))
# # points <- data.frame(x = runif(4),
# #                      y = runif(4))
# tdf <- bezier(points)
# ggplot2::ggplot(tdf) +
#   ggplot2::geom_point(data = points, ggplot2::aes(x, y)) +
#   ggplot2::geom_path(ggplot2::aes(x, y))
*/
