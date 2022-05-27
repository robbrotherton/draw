// C++ Implementation. To find the point of
// intersection of two lines
// https://www.geeksforgeeks.org/program-for-point-of-intersection-of-two-lines/
#include <Rcpp.h>
#include <bits/stdc++.h>

// using namespace std;
using namespace Rcpp;

// Define helpers
double dist(NumericVector P1, NumericVector P2);
List rm_null(List x);

// [[Rcpp::export]]
DataFrame lineLineIntersection(NumericVector P1,
                               NumericVector P2,
                               NumericVector P3,
                               NumericVector P4,
                               bool include_lineend = true,
                               bool return_logical = false) {

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
    if(return_logical) {
      return false;
    } else {
      return DataFrame::create(_["x"]= NA_REAL, _["y"]= NA_REAL);
    }
  }

  // Point of intersection; may be beyond the end of the lines
  double x = (dx2*s1 - dx1*s2)/determinant;
  double y = (dy1*s2 - dy2*s1)/determinant;

  // Check the fraction of the lines where they meet. If it's not in the range
  // [0, 1], the lines don't intersect
  double lambda1 = -((x-P1[0])*dx1 + (y-P1[1])*-dy1)/(dx1 * dx1 + dy1 * dy1);
  double lambda2 = -((x-P3[0])*dx2 + (y-P3[1])*-dy2)/(dx2 * dx2 + dy2 * dy2);

  if (include_lineend) {
    intersection = ((lambda1 >= 0) & (lambda1 <= 1) &
                    (lambda2 >= 0) & (lambda2 <= 1));
  } else {
    intersection = ((lambda1 > 0) & (lambda1 < 1) &
                    (lambda2 > 0) & (lambda2 < 1));
  }

  if (intersection) {
    if(return_logical) {
      return true;
    } else {
      return DataFrame::create(_["x"]= x, _["y"]= y);
    }

  } else {
    if(return_logical) {
      return false;
    } else {
      return DataFrame::create(_["x"]= NA_REAL, _["y"]= NA_REAL);
    }

  }

}


// [[Rcpp::export]]
DataFrame line_intersection(NumericVector P1,
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
    intersection = ((lambda1 >= 0) & (lambda1 <= 1) &
      (lambda2 >= 0) & (lambda2 <= 1));
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


// [[Rcpp::export]]
bool line_intersection_lgl(NumericVector P1,
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
    return false;
  }

  // Point of intersection; may be beyond the end of the lines
  double x = (dx2*s1 - dx1*s2)/determinant;
  double y = (dy1*s2 - dy2*s1)/determinant;

  // Check the fraction of the lines where they meet. If it's not in the range
  // [0, 1], the lines don't intersect
  double lambda1 = -((x-P1[0])*dx1 + (y-P1[1])*-dy1)/(dx1 * dx1 + dy1 * dy1);
  double lambda2 = -((x-P3[0])*dx2 + (y-P3[1])*-dy2)/(dx2 * dx2 + dy2 * dy2);

  if (include_lineend) {
    intersection = ((lambda1 >= 0) & (lambda1 <= 1) &
      (lambda2 >= 0) & (lambda2 <= 1));
  } else {
    intersection = ((lambda1 > 0) & (lambda1 < 1) &
      (lambda2 > 0) & (lambda2 < 1));
  }

  if (intersection) {
    return true;
  } else {
    return false;
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
          (this_y < min(these_poly_y)) |
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


        bool this_res = lineLineIntersection(P1, P2, P3, P4, true, true)["x"];

        if(this_res) {
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


// [[Rcpp::export]]
bool point_in_polygon(double x, double y, DataFrame polygon, bool include_perimeter = true) {

  NumericVector poly_x = polygon["x"];
  NumericVector poly_y = polygon["y"];
  NumericVector poly_g(poly_x.size());

  bool group_col = polygon.containsElementNamed("group");

  if(group_col) {
    poly_g = polygon["group"];
  } else {
    for(int i = 0; i < poly_x.size(); ++i) {
      poly_g[i] = 1;
    }
  }

  // Need to be able to accommodate groups of polygons, e.g. for letters with
  // multiple sections

  NumericVector poly_group_ids = unique(poly_g);
  int n_polys = poly_group_ids.size();

  // Rcout << poly_group_ids;

  for (int j = 0; j < n_polys; ++j) {

    NumericVector this_poly_x = poly_x[poly_g == poly_group_ids[j]];
    NumericVector this_poly_y = poly_y[poly_g == poly_group_ids[j]];

    // if( (x < min(this_poly_x)) |
    //     (x > max(this_poly_x)) |
    //     (y < min(this_poly_y)) |
    //     (y > max(this_poly_y))) {
    //   continue;
    // }

    double max_x = max(this_poly_x) + 1;
    int n_intersections = 0;

    NumericVector P1 = NumericVector::create(x, y);
    NumericVector P2 = NumericVector::create(max_x, y);

    // Now check each polygon edge and add up the intersections. If it's
    // even, the point is outside the polygon; if it's odd, the point is
    // inside.
    for (int k = 0; k < (this_poly_x.size() - 1); ++k) {

      // on_line = false;

      NumericVector P3 = NumericVector::create(this_poly_x[k],   this_poly_y[k]);
      NumericVector P4 = NumericVector::create(this_poly_x[k+1], this_poly_y[k+1]);

      // First, check if the point is ON the perimeter of the polygon. If it
      // is, return TRUE

      if (dist(P3, P1) + dist(P4, P1) == dist(P3, P4)) {
        return true;
      }

      bool this_res = line_intersection_lgl(P1, P2, P3, P4, true);

      // Rcout << this_res;

      if(this_res) {
        ++n_intersections;
      }

    }

    if(n_intersections % 2 != 0) {
      return(true);
    }

  }

  return(false);

}

// [[Rcpp::export]]
LogicalVector points_in_polygon(DataFrame points, DataFrame polygon) {

  int n_points = points.nrow();
  NumericVector points_x = points["x"];
  NumericVector points_y = points["y"];
  LogicalVector res(n_points);

  for (int i = 0; i < n_points; ++i) {

    double x = points_x[i];
    double y = points_y[i];

    res[i] = point_in_polygon(x, y, polygon);

  }

  return res;

}


double dist(NumericVector P1, NumericVector P2) {

  double dx = P1[0] - P2[0];
  double dy = P1[1] - P2[1];
  return sqrt(dx * dx + dy * dy);

}


List rm_null(List x) {
  int n = x.size();
  LogicalVector to_keep(n);
  for (int i = 0; i < n; i++) {
    to_keep[i] = !Rf_isNull(x[i]);
  }
  return x[to_keep];
}

// [[Rcpp::export]]
List clip_paths(DataFrame hatch_segs, DataFrame polygon) {

  // It would be easier if the hatch input was segments (x, y, xend, yend)
  int h_segs =   hatch_segs.nrow();
  int p_segs =      polygon.nrow() - 1;

  NumericVector x    = hatch_segs["x"];
  NumericVector y    = hatch_segs["y"];
  NumericVector xend = hatch_segs["xend"];
  NumericVector yend = hatch_segs["yend"];

  NumericVector poly_x    = polygon["x"];
  NumericVector poly_y    = polygon["y"];
  // NumericVector poly_xend = polygon["xend"];
  // NumericVector poly_yend = polygon["yend"];
  NumericVector poly_group= polygon["group"];

  // # We'll have a data.frame for each hatch segment, then bind_rows() at the end
  List res_x(h_segs);
  // int index = 0;

  for(int i = 0; i < h_segs; ++i) {

    NumericVector P1 = NumericVector::create(x[i], y[i]);
    NumericVector P2 = NumericVector::create(xend[i], yend[i]);

    NumericVector ints_x(p_segs);
    NumericVector ints_y(p_segs);
    NumericVector ints_d(p_segs);
    NumericVector ints_l(p_segs);

    for(int j = 0; j < p_segs; ++j) {

      if((j < (p_segs)) & (poly_group[j+1] != poly_group[j])) {
        // Skip rows where the next row is from a different polygon
        ints_x[j] = NA_REAL;
        ints_y[j] = NA_REAL;
        ints_d[j] = NA_REAL;
        ints_l[j] = NA_REAL;
        continue;
      }

      NumericVector P3 = NumericVector::create(poly_x[j], poly_y[j]);
      NumericVector P4 = NumericVector::create(poly_x[j+1], poly_y[j+1]);

      DataFrame ints_df = line_intersection(P1, P2, P3, P4);

      ints_x[j] = ints_df["x"];
      ints_y[j] = ints_df["y"];
      ints_d[j] = dist(P1, NumericVector::create(ints_x[j], ints_y[j]));
      ints_l[j] = i+1;

      if((j == p_segs - 1) &
         (ints_x[j] == ints_x[0]) &
         (ints_y[j] == ints_y[0])) {
        // If this intersection is the same as the previous one, disregard. This
        // can happen when a line clips the end of two adjoining polygon
        // segments.
        ints_x[j] = NA_REAL;
        ints_y[j] = NA_REAL;
        ints_d[j] = NA_REAL;
        ints_l[j] = NA_REAL;
      }

      if((j > 0) &
         (ints_x[j] == ints_x[j-1]) &
         (ints_y[j] == ints_y[j-1])) {
        // If this intersection is the same as the previous one, disregard. This
        // can happen when a line clips the end of two adjoining polygon
        // segments.
        ints_x[j] = NA_REAL;
        ints_y[j] = NA_REAL;
        ints_d[j] = NA_REAL;
        ints_l[j] = NA_REAL;
      }

    }


    // Now drop any NAs (and duplicates?) and check how many intersections there are
    ints_x = na_omit(ints_x);
    ints_y = na_omit(ints_y);
    ints_d = na_omit(ints_d);
    ints_l = na_omit(ints_l);

    if((ints_x.size() > 0)) { //  & (ints_x.size() % 2 == 0)
      res_x[i] = DataFrame::create(_["x"] = ints_x,
                                   _["y"] = ints_y,
                                   _["d"] = ints_d);
    }

    // res_x = null_omit(res);
    //       line_intersections_df <- line_intersections_df |>
    //         dplyr::distinct() |>
    //         tidyr::drop_na()
    //
    //         index <- index + 1
    //
    // # Need some checks here before adding a result to the list.
    // # Is there just a single point? Can happen if a hatch line clips one corner
    // # Or 3 points? Or any odd number?
    // # Does the hatch segment duplicate a segment of the polygon?
    //
    //       if(nrow(line_intersections_df) %% 2 == 0) {
    //
    //         res[[index]] <- line_intersections_df |>
    //         dplyr::mutate(d = (x - P1[1])^2 + (y - P1[2]^2)^2)
    //
    //         if(index %% 2 == 0) {
    //           res[[index]] <- res[[index]] |>
    //           dplyr::arrange(-d)
    //         } else {
    //           res[[index]] <- res[[index]] |>
    //           dplyr::arrange(d)
    //         }
    //
    //       }
    //
  }
  //
    return res_x;

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
