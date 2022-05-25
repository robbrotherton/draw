// C++ Implementation. To find the point of
// intersection of two lines
// https://www.geeksforgeeks.org/program-for-point-of-intersection-of-two-lines/
#include <Rcpp.h>
#include <bits/stdc++.h>

// using namespace std;
using namespace Rcpp;


double dist(NumericVector P1, NumericVector P2);


// [[Rcpp::export]]
DataFrame lineLineIntersection(NumericVector P1,
                               NumericVector P2,
                               NumericVector P3,
                               NumericVector P4,
                               bool include_lineend = true) {

  bool intersection;

  // // Line AB represented as a1x + b1y = c1
  double dx1 = P1[0] - P2[0]; // b1
  double dy1 = P2[1] - P1[1]; // a1
  double s1 = dy1*(P1[0]) + dx1*(P1[1]);
  //
  // // Line CD represented as a2x + b2y = c2
  double dx2 = P3[0] - P4[0]; // b2
  double dy2 = P4[1] - P3[1]; // a2
  double s2 = dy2*(P3[0]) + dx2*(P3[1]);

  double determinant = dy1 * dx2 - dy2 * dx1;

  if (determinant == 0) {
    // The lines are parallel.
    return DataFrame::create(_["x"]= NA_REAL, _["y"]= NA_REAL);
  }

  // Point of intersection; may be beyond the end of the lines
  double x = (dx2*s1 - dx1*s2)/determinant;
  double y = (dy1*s2 - dy2*s1)/determinant;

  // Check the fraction of the lines where they meet. If it's not in the range
  // [0, 1], the lines don't intersect
  double lambda1 = -((x-P1[0])*dx1 + (y-P1[1])*-dy1)/(dx1 * dx1 + dy1 * dy1);
  double lambda2 = -((x-P3[0])*dx2 + (y-P3[1])*-dy2)/(dx2 * dx2 + dy2 * dy2);

  if (include_lineend) {
    intersection = ((lambda1 >= -0.0001) & (lambda1 <= 1.0001) &
                    (lambda2 >= -0.0001) & (lambda2 <= 1.0001));
  } else {
    intersection = ((lambda1 > 0) & (lambda1 < 1) &
                    (lambda2 > 0) & (lambda2 < 1));
  }

  if (intersection) {

    return DataFrame::create(_["x"]= x, _["y"]= y);

  } else {

    return DataFrame::create(_["x"]= NA_REAL, _["y"]= NA_REAL);
  }

}


// lineLineIntersection(c(0, 0), c(10, 10), c(5, 0), c(5, 10))
// lineLineIntersection(c(-1, 0), c(1, 0), c(-.5, 1), c(-.5, -1))
// lineLineIntersection(c(0, 0), c(1, 1), c(2, 0), c(3, 10))


// [[Rcpp::export]]
LogicalVector pointsInPolygons(DataFrame points, DataFrame polygons) {

  NumericVector x = points["x"];
  NumericVector y = points["y"];
  int n_points = x.size();

  // Rcout << n_points;
  // return(0);

  NumericVector poly_x = polygons["x"];
  NumericVector poly_y = polygons["y"];
  NumericVector poly_group = polygons["group"];
  int n_polys = max(poly_group);
  // return(0);

  LogicalVector res(n_points);
  bool on_line;

  for (int i = 0; i < n_points; ++i) {

    double this_x = x[i];
    double this_y = y[i];

    for (int j = 1; j <= n_polys; ++j) {

      NumericVector these_poly_x = poly_x[poly_group == j];
      NumericVector these_poly_y = poly_y[poly_group == j];

      if( (this_x < min(these_poly_x)) |
          (this_x > max(these_poly_x)) |
          (this_y > max(these_poly_y)) |
          (this_y > max(these_poly_y))) {
        continue;
      }

      double max_x = max(these_poly_x) + 1;
      int n_intersections = 0;

      NumericVector P1 = NumericVector::create(this_x, this_y);
      NumericVector P2 = NumericVector::create(max_x, this_y);

      for (int k = 0; k < (these_poly_x.size() - 1); ++k) {

        on_line = false;

        // # Want a version of line.line.intersection that returns TRUE if there is an
        // # intersection, FALSE if not. Then count the number of TRUEs. If it's
        // # even, the point is outside the polygon; if it's odd, the point is
        // # inside.

        NumericVector P3 = NumericVector::create(these_poly_x[k], these_poly_y[k]);
        NumericVector P4 = NumericVector::create(these_poly_x[k+1], these_poly_y[k+1]);

        // First, check if the point is ON the line of the polygon. If it is,
        // return TRUE and break
        if (dist(P3, P1) + dist(P4, P1) == dist(P3, P4)) {
          res[i] = true;
          on_line = true;
          break;
        }


        NumericVector this_res = lineLineIntersection(P1, P2, P3, P4, true)["x"];

        if(sum(is_na(this_res)) > 0 ) {
          ++n_intersections;
        }

      }

      if(!on_line & (n_intersections % 2 != 0)) {
        res[i] = true;
        break;
      }

    }

    // res[i] = FALSE;

  }

  return(res);

}

double dist(NumericVector P1, NumericVector P2) {

  double dx = P1[0] - P2[0];
  double dy = P1[1] - P2[1];
  return sqrt(dx * dx + dy * dy);

}


/*** R

# basic test
test_points <- data.frame(x = 1:5, y = 1:5)
test_polygons <- dplyr::bind_rows(.id = "group",
                                  square() + 2,
                                  square() + 4) %>%
  dplyr::mutate(group = as.numeric(group))

test_points$inside <- pointsInPolygons(test_points, test_polygons)

ptm <- proc.time()
pointsInPolygons(test_points, test_polygons)
proc.time() - ptm

ptm <- proc.time()
points_in_polygons(test_points, test_polygons)
proc.time() - ptm

ggplot2::ggplot() +
  ggplot2::geom_path(data = test_polygons, ggplot2::aes(x, y, group = group)) +
  ggplot2::geom_point(data = test_points, ggplot2::aes(x, y, color = inside))

# more complex test
n_points <- 1000
n_polys <- 50
test_points <- data.frame(x = runif(n_points, min = -500, max = 500),
                          y = runif(n_points, min = -500, max = 500))
offsets <- data.frame(group = 1:n_polys,
                      x_off = runif(n_polys, min = -500, max = 500),
                      y_off = runif(n_polys, min = -500, max = 500))
test_polygons <- purrr::map_df(.x = 1:n_polys,
                               .f = ~square()*rnorm(1, mean = 150, sd = 20),
                               .id = "group") %>%
  dplyr::mutate(group = as.numeric(group)) %>%
  dplyr::left_join(offsets) %>%
  dplyr::mutate(x = x + x_off, y = y + y_off)

ggplot2::ggplot() +
  ggplot2::geom_path(data = test_polygons, ggplot2::aes(x, y, group = group)) +
  ggplot2::geom_point(data = test_points, ggplot2::aes(x, y))

test_points$inside <- pointsInPolygons(test_points, test_polygons)

ptm <- proc.time()
pointsInPolygons(test_points, test_polygons)
proc.time() - ptm

ptm <- proc.time()
points_in_polygons(test_points, test_polygons)
proc.time() - ptm

ggplot2::ggplot() +
  ggplot2::geom_path(data = test_polygons, ggplot2::aes(x, y, group = group)) +
  ggplot2::geom_point(data = test_points, ggplot2::aes(x, y, color = inside))


*/
