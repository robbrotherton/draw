// C++ Implementation to find the point of intersection of two lines
// https://www.geeksforgeeks.org/program-for-point-of-intersection-of-two-lines/

#include <Rcpp.h>
#include <bits/stdc++.h>

// using namespace std;
using namespace Rcpp;

// Declare helper functions
double dist(NumericVector P1, NumericVector P2);
List rm_null(List x);
bool approxEqual(double a, double b, double e = 0.0001);
IntegerVector sorted_indices(NumericVector x);


// [[Rcpp::export]]
DataFrame line_intersection(NumericVector P1,
                            NumericVector P2,
                            NumericVector P3,
                            NumericVector P4,
                            bool include_lineend = true,
                            bool interior_only = true) {

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

  if (determinant == 0.0) {
    // The lines are parallel.
    return DataFrame::create(_["x"]= NA_REAL, _["y"]= NA_REAL);
  }

  // Point of intersection; may be beyond the end of the lines
  double x = (dx2*s1 - dx1*s2)/determinant;
  double y = (dy1*s2 - dy2*s1)/determinant;

  if(!interior_only) {
    return DataFrame::create(_["x"]= x, _["y"]= y);
  }

  // Check the fraction of the lines where they meet. If it's not in the range
  // [0, 1], the lines don't intersect
  double lambda1 = -((x-P1[0])*dx1 + (y-P1[1])*-dy1)/(dx1 * dx1 + dy1 * dy1);
  double lambda2 = -((x-P3[0])*dx2 + (y-P3[1])*-dy2)/(dx2 * dx2 + dy2 * dy2);

  if (include_lineend) {
    intersection = ((lambda1 >= -0.000001) & (lambda1 <= 1.0000001) &
                    (lambda2 >= -0.000001) & (lambda2 <= 1.0000001));
  } else {
    intersection = ((lambda1 > 0.0) & (lambda1 < 1.0) &
                    (lambda2 > 0.0) & (lambda2 < 1.0));
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

    if( (x < min(this_poly_x)) |
        (x > max(this_poly_x)) |
        (y < min(this_poly_y)) |
        (y > max(this_poly_y))) {
      continue;
    }

    double max_x = max(this_poly_x) + 1;
    // int n_intersections = 0;

    NumericVector ints_x(this_poly_x.size());
    NumericVector ints_y(this_poly_x.size());

    NumericVector P1 = NumericVector::create(x, y);
    NumericVector P2 = NumericVector::create(max_x, y);

    // Now check each polygon edge and add up the intersections. If it's
    // even, the point is outside the polygon; if it's odd, the point is
    // inside.
    for (int k = 0; k < (this_poly_x.size() - 1); ++k) {

      // on_line = false;

      NumericVector y_lims = NumericVector::create(this_poly_y[k], this_poly_y[k+1]);
      if((y < min(y_lims)) | (y > max(y_lims))) {
        // ++skipped;
        ints_x[k] = NA_REAL;
        ints_y[k] = NA_REAL;
        continue;
      }

      NumericVector P3 = NumericVector::create(this_poly_x[k],   this_poly_y[k]);
      NumericVector P4 = NumericVector::create(this_poly_x[k+1], this_poly_y[k+1]);

      // First, check if the point is ON the perimeter of the polygon. If it
      // is, return TRUE

      if (dist(P3, P1) + dist(P4, P1) == dist(P3, P4)) {
        // Rcout << skipped;
        return true;
      }

      // Otherwise, get the coordinates of any intersections. We need the
      // coordinates, rather than just TRUE/FALSE, so we can check for
      // duplicates later.
      DataFrame res = line_intersection(P1, P2, P3, P4, true);
      NumericVector res_x = res["x"];
      NumericVector res_y = res["y"];

      ints_x[k] = res_x[0];
      ints_y[k] = res_y[0];

    }

    // ints_x = na_omit(ints_x);
    // ints_y = na_omit(ints_y);

    // Check for duplicate points.
    for(int d = 0; d < ints_x.size(); ++d) {

      if((d == 0) &
         (ints_x[d] == ints_x[ints_x.size() - 1]) &
         (ints_y[d] == ints_y[ints_x.size() - 1])) {

        ints_x[d] = NA_REAL;
        ints_y[d] = NA_REAL;
      }

      if((d > 0) &
         (ints_x[d] == ints_x[d-1]) &
         (ints_y[d] == ints_y[d-1])) {

        ints_x[d] = NA_REAL;
        ints_y[d] = NA_REAL;
      }

    }

    // Now find the number of non-NAs. THis is the number of intersections
    ints_x = na_omit(ints_x);
    int n_intersections = ints_x.size();

    if((n_intersections > 0) &
       (n_intersections % 2 == 0)) {
      // Rcout << skipped;
      return true;
    }

  }

  // Rcout << skipped;
  return false;

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

      // If this intersection is the same as the previous one, disregard. This
      // can happen when a line clips the end of two adjoining polygon
      // segments.
      if((j > 0) &
         (ints_x[j] == ints_x[j-1]) &
         (ints_y[j] == ints_y[j-1])) {

        ints_x[j] = NA_REAL;
        ints_y[j] = NA_REAL;
        ints_d[j] = NA_REAL;
        ints_l[j] = NA_REAL;
      }

      // Same check, but comparing the last result to the first to complete the
      // circle
      if((j == p_segs - 1) &
         (ints_x[j] == ints_x[0]) &
         (ints_y[j] == ints_y[0])) {

        ints_x[j] = NA_REAL;
        ints_y[j] = NA_REAL;
        ints_d[j] = NA_REAL;
        ints_l[j] = NA_REAL;
      }

    }

    // Now drop any NAs
    // ints_x = na_omit(ints_x);
    // ints_y = na_omit(ints_y);
    // ints_d = na_omit(ints_d);
    // ints_l = na_omit(ints_l);

    // And only include a result when there's more than 1 point
    // Might need a check for an even number of points too?
    if((ints_x.size() > 0)) { //  & (ints_x.size() % 2 == 0)
      res_x[i] = DataFrame::create(_["x"] = ints_x,
                                   _["y"] = ints_y,
                                   _["d"] = ints_d);
    }

  }

  // Remove nulls from list here
  return res_x;

}


// CLIP 2 -------------
//   case when...
//   P1 in P2 in
//   could be entirely inside (no intersections) ::: if ints == 0, KEEP
//     could pass out and back in (even number of intersections) ::: keep points and ints
//     shouldn't have an odd number of intersections...
//       P1 out P2 out
//     could be entirely outside (no intersections) ::: if ints == 0, DISCARD
//       or could pass through polygon (even number of intersections) ::: keep ints, discard P1 & P2
//       shouldn't have an odd number of intersections...
//         P1 in P2 out
//       could cross one polygon segment, exiting ::: keep P! and int, discard P2
//       or could pass out, in, out (odd number of intersections)
//       shouldn't have an even number of intersections
//       P1 out P2 in
//       could cross one polygon segment, entering ::: keep int and P2, discard P1
//       or could pass in, out, in (odd number of intersections)
//       shouldn't have an even number of intersections
//       so the rule is... always keep ints and drop any points that are out.
//     this should always result in an even number of points, which can be arranged
//       by distance and grouped in pairs.

// [[Rcpp::export]]
List clip_paths_complex(DataFrame hatch_segs, DataFrame polygon) {

  // It would be easier if the hatch input was segments (x, y, xend, yend)
  int h_segs =   hatch_segs.nrow();
  int p_segs =      polygon.nrow() - 1;

  // return h_segs;

  NumericVector x    = hatch_segs["x"];
  NumericVector y    = hatch_segs["y"];
  NumericVector xend = hatch_segs["xend"];
  NumericVector yend = hatch_segs["yend"];

  NumericVector poly_x    = polygon["x"];
  NumericVector poly_y    = polygon["y"];
  NumericVector poly_group= polygon["group"];

  // # We'll have a data.frame for each hatch segment, then bind_rows() at the end
  List res_x(h_segs);
  int segment_id = 0;
  double prev_x = NA_REAL;
  double prev_y = NA_REAL;
  // int index = 0;

  for(int i = 0; i < h_segs; ++i) {

    NumericVector P1 = NumericVector::create(x[i], y[i]);
    NumericVector P2 = NumericVector::create(xend[i], yend[i]);

    NumericVector ints_x(p_segs + 2, NA_REAL);
    NumericVector ints_y(p_segs + 2, NA_REAL);
    NumericVector ints_d(p_segs + 2, NA_REAL);
    // NumericVector ints_l(p_segs + 2, NA_REAL);
    // NumericVector ints_g(p_segs + 2, NA_REAL);

    // ints_x[0] = x[i];
    // ints_y[0] = y[i];
    // ints_x[p_segs + 1] = xend[i];
    // ints_y[p_segs + 1] = yend[i];

    for(int j = 0; j < p_segs; ++j) {

      // Skip rows where the next row is from a different polygon
      if((j < (p_segs)) & (poly_group[j+1] != poly_group[j])) {
        continue;
      }

      NumericVector P3 = NumericVector::create(poly_x[j], poly_y[j]);
      NumericVector P4 = NumericVector::create(poly_x[j+1], poly_y[j+1]);

      DataFrame ints_df = line_intersection(P1, P2, P3, P4);

      ints_x[j+1] = ints_df["x"];
      ints_y[j+1] = ints_df["y"];
      ints_d[j+1] = dist(P1, NumericVector::create(ints_x[j+1], ints_y[j+1]));
      // ints_l[j+1] = i+1;
      // ints_g[j+1] = segment_id;

    }

    // Include P1 and P2 if necessary
    if(point_in_polygon(P1[0], P1[1], polygon)) {
      ints_x[0] = P1[0];
      ints_y[0] = P1[1];
      ints_d[0] = 0.0;
      // ints_l[0] = i+1;
      // ints_g[0] = segment_id;
    }

    if(point_in_polygon(P2[0], P2[1], polygon)) {
      ints_x[p_segs + 1] = P2[0];
      ints_y[p_segs + 1] = P2[1];
      ints_d[p_segs + 1] = dist(P1, P2);
      // ints_l[0] = i+1;
      // ints_g[0] = segment_id;
    }

    // Check for any duplicated points
    for(int k = 1; k < p_segs + 2; ++k) {

      //  Check each point against each other point and NA matches
      //  This is probably overkill and could be made more thoughtful.
      // I think it's only necessary to check K against k-1 and 1 against [last]
      for(int x = 0; x < k; ++x) {
        if((approxEqual(ints_d[k], ints_d[x]))) {

          ints_x[k] = NA_REAL;
          ints_y[k] = NA_REAL;
          ints_d[k] = NA_REAL;
        }
      }

      // if((k == 0) &
      //    (ints_d[0] == ints_d[(p_segs + 1)])) {
      //    // (ints_x[k] == ints_x[ints_x.size() - 1]) &
      //    // (ints_y[k] == ints_y[ints_x.size() - 1])) {
      //
      //   ints_x[p_segs + 1] = NA_REAL;
      //   ints_y[p_segs + 1] = NA_REAL;
      //   ints_d[p_segs + 1] = NA_REAL;
      //   ints_l[p_segs + 1] = NA_REAL;
      // }

      // if((k > 0) &
      //    (approxEqual(ints_d[k], ints_d[k-1]))) {
      //   // (ints_x[k] == ints_x[k-1]) &
      //   // (ints_y[k] == ints_y[k-1])) {
      //
      //   // Rcout << k << "^ samesies!\n";
      //
      //   ints_x[k] = NA_REAL;
      //   ints_y[k] = NA_REAL;
      //   ints_d[k] = NA_REAL;
      //   ints_l[k] = NA_REAL;
      // }
    }

    // Now drop any NAs
    ints_x = na_omit(ints_x);
    ints_y = na_omit(ints_y);
    ints_d = na_omit(ints_d);
    // ints_l = na_omit(ints_l);

    // If there's less than 2 valid intersections, nothing to record; move on to
    // the next line
    if(ints_x.size() < 2) continue;

    // Sort by distance
    IntegerVector idx = sorted_indices(ints_d);
    ints_x = ints_x[idx];
    ints_y = ints_y[idx];
    ints_d = ints_d[idx];

    // Now label segment groups
    NumericVector ints_l(ints_x.size());
    NumericVector ints_g(ints_x.size());

    bool same_as_previous = approxEqual(ints_x[0], prev_x) &
      approxEqual(ints_y[0], prev_y);

    if(same_as_previous) --segment_id;

    for(int s = 0; s < ints_x.size(); ++s) {
      if(s % 2 == 0) ++segment_id;
      ints_g[s] = segment_id;
      ints_l[s] = i+1;

    }

    // Now check if first point is same as the last point of the previous
    // segment.
    // if(!approx_equal(ints_x[0], prev_x) |
    //    !approx_equal(ints_y[0], prev_y)) {
    //   ++segment_id;
    // }
    //
    prev_x = ints_x[ints_x.size()-1];
    prev_y = ints_y[ints_y.size()-1];

    // And only include a result when there's more than 1 point
    // Might need a check for an even number of points too?
    // if((ints_x.size() > 0)) { //  & (ints_x.size() % 2 == 0)
    res_x[i] = DataFrame::create(_["x"] = ints_x,
                                 _["y"] = ints_y,
                                 _["d"] = ints_d,
                                 _["line"] = ints_l,
                                 _["seg_id"] = ints_g);
    // }

  }

  // Remove nulls from list here
  res_x = rm_null(res_x);
  return res_x;

}





// [[Rcpp::export]]
List clip_paths_complex_outside(DataFrame hatch_segs, DataFrame polygon) {

  // It would be easier if the hatch input was segments (x, y, xend, yend)
  int h_segs =   hatch_segs.nrow();
  int p_segs =      polygon.nrow() - 1;

  // return h_segs;

  NumericVector x    = hatch_segs["x"];
  NumericVector y    = hatch_segs["y"];
  NumericVector xend = hatch_segs["xend"];
  NumericVector yend = hatch_segs["yend"];

  NumericVector poly_x    = polygon["x"];
  NumericVector poly_y    = polygon["y"];
  NumericVector poly_group= polygon["group"];

  // # We'll have a data.frame for each hatch segment, then bind_rows() at the end
  List res_x(h_segs);
  int segment_id = 0;
  double prev_x = NA_REAL;
  double prev_y = NA_REAL;
  // int index = 0;

  for(int i = 0; i < h_segs; ++i) {

    NumericVector P1 = NumericVector::create(x[i], y[i]);
    NumericVector P2 = NumericVector::create(xend[i], yend[i]);

    NumericVector ints_x(p_segs + 2, NA_REAL);
    NumericVector ints_y(p_segs + 2, NA_REAL);
    NumericVector ints_d(p_segs + 2, NA_REAL);
    // NumericVector ints_l(p_segs + 2, NA_REAL);
    // NumericVector ints_g(p_segs + 2, NA_REAL);

    // ints_x[0] = x[i];
    // ints_y[0] = y[i];
    // ints_x[p_segs + 1] = xend[i];
    // ints_y[p_segs + 1] = yend[i];

    for(int j = 0; j < p_segs; ++j) {

      // Skip rows where the next row is from a different polygon
      if((j < (p_segs)) & (poly_group[j+1] != poly_group[j])) {
        continue;
      }

      NumericVector P3 = NumericVector::create(poly_x[j], poly_y[j]);
      NumericVector P4 = NumericVector::create(poly_x[j+1], poly_y[j+1]);

      DataFrame ints_df = line_intersection(P1, P2, P3, P4);

      ints_x[j+1] = ints_df["x"];
      ints_y[j+1] = ints_df["y"];
      ints_d[j+1] = dist(P1, NumericVector::create(ints_x[j+1], ints_y[j+1]));
      // ints_l[j+1] = i+1;
      // ints_g[j+1] = segment_id;

    }

    // Include P1 and P2 if necessary
    if(!point_in_polygon(P1[0], P1[1], polygon)) {
      ints_x[0] = P1[0];
      ints_y[0] = P1[1];
      ints_d[0] = 0.0;
      // ints_l[0] = i+1;
      // ints_g[0] = segment_id;
    }

    if(!point_in_polygon(P2[0], P2[1], polygon)) {
      ints_x[p_segs + 1] = P2[0];
      ints_y[p_segs + 1] = P2[1];
      ints_d[p_segs + 1] = dist(P1, P2);
      // ints_l[0] = i+1;
      // ints_g[0] = segment_id;
    }

    // Check for any duplicated points
    for(int k = 1; k < p_segs + 2; ++k) {

      //  Check each point against each other point and NA matches
      //  This is probably overkill and could be made more thoughtful.
      // I think it's only necessary to check K against k-1 and 1 against [last]
      for(int x = 0; x < k; ++x) {
        if((approxEqual(ints_d[k], ints_d[x]))) {

          ints_x[k] = NA_REAL;
          ints_y[k] = NA_REAL;
          ints_d[k] = NA_REAL;
        }
      }

      // if((k == 0) &
      //    (ints_d[0] == ints_d[(p_segs + 1)])) {
      //    // (ints_x[k] == ints_x[ints_x.size() - 1]) &
      //    // (ints_y[k] == ints_y[ints_x.size() - 1])) {
      //
      //   ints_x[p_segs + 1] = NA_REAL;
      //   ints_y[p_segs + 1] = NA_REAL;
      //   ints_d[p_segs + 1] = NA_REAL;
      //   ints_l[p_segs + 1] = NA_REAL;
      // }

      // if((k > 0) &
      //    (approxEqual(ints_d[k], ints_d[k-1]))) {
      //   // (ints_x[k] == ints_x[k-1]) &
      //   // (ints_y[k] == ints_y[k-1])) {
      //
      //   // Rcout << k << "^ samesies!\n";
      //
      //   ints_x[k] = NA_REAL;
      //   ints_y[k] = NA_REAL;
      //   ints_d[k] = NA_REAL;
      //   ints_l[k] = NA_REAL;
      // }
    }

    // Now drop any NAs
    ints_x = na_omit(ints_x);
    ints_y = na_omit(ints_y);
    ints_d = na_omit(ints_d);
    // ints_l = na_omit(ints_l);

    // If there's less than 2 valid intersections, nothing to record; move on to
    // the next line
    if(ints_x.size() < 2) continue;

    // Sort by distance
    IntegerVector idx = sorted_indices(ints_d);
    ints_x = ints_x[idx];
    ints_y = ints_y[idx];
    ints_d = ints_d[idx];

    // Now label segment groups
    NumericVector ints_l(ints_x.size());
    NumericVector ints_g(ints_x.size());

    bool same_as_previous = approxEqual(ints_x[0], prev_x) &
      approxEqual(ints_y[0], prev_y);

    if(same_as_previous) --segment_id;

    for(int s = 0; s < ints_x.size(); ++s) {
      if(s % 2 == 0) ++segment_id;
      ints_g[s] = segment_id;
      ints_l[s] = i+1;

    }

    // Now check if first point is same as the last point of the previous
    // segment.
    // if(!approx_equal(ints_x[0], prev_x) |
    //    !approx_equal(ints_y[0], prev_y)) {
    //   ++segment_id;
    // }
    //
    prev_x = ints_x[ints_x.size()-1];
    prev_y = ints_y[ints_y.size()-1];

    // And only include a result when there's more than 1 point
    // Might need a check for an even number of points too?
    // if((ints_x.size() > 0)) { //  & (ints_x.size() % 2 == 0)
    res_x[i] = DataFrame::create(_["x"] = ints_x,
                                 _["y"] = ints_y,
                                 _["d"] = ints_d,
                                 _["line"] = ints_l,
                                 _["seg_id"] = ints_g);
    // }

  }

  // Remove nulls from list here
  res_x = rm_null(res_x);
  return res_x;

}



// Unexported helper functions

double dist(NumericVector P1, NumericVector P2) {

  double dx = P1[0] - P2[0];
  double dy = P1[1] - P2[1];
  // return sqrt(dx * dx + dy * dy);
  return dx * dx + dy * dy;

}


List rm_null(List x) {
  int n = x.size();
  LogicalVector to_keep(n);
  for (int i = 0; i < n; i++) {
    to_keep[i] = !Rf_isNull(x[i]);
  }
  return x[to_keep];
}


bool approxEqual(double a, double b, double e) {
  return abs(a - b) < e;
}


IntegerVector sorted_indices(NumericVector x) {
  IntegerVector idx = seq_along(x) - 1;
  std::sort(idx.begin(), idx.end(), [&](int i, int j){return x[i] < x[j];});
  return idx;
}

