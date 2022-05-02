#' @title Hatch fill a polygon
#'
#' @param df A data.frame containing x and y coordinates for a single polygon
#' @param spacing the spacing between lines (if n is not specified)
#' @param angle In radians
#' @param keep_outline Logical: keep or drop the original outline of the polygon
#' @param single_line Logical: Arrange the hatch lines into a single unbroken
#'   line (good for pen plotter). Overrides \code{keep_outline}
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' circle() %>% hatch() %>% show()
hatch <- function(df, spacing = .1, angle = 0, keep_outline = TRUE, single_line = FALSE) {

  # First create hatch segments in a bounding box with width and height equal to
  # the diagonal of the shape
  hatch_paths <- hatch_overlay(df, spacing, angle)

  # Now we take each hatch path and check for intersections with each segment of
  # the polygon
  h_segs <- nrow(hatch_paths)
  p_segs <- nrow(df) - 1

  res <- vector(mode = "list", length = h_segs/2 * p_segs)
  index <- 0

  for(i in seq(from = 1, to = h_segs, by= 2)) {

    P1 <- c(hatch_segments$x[i]   , hatch_segments$y[i])
    P2 <- c(hatch_segments$x[i+1]   , hatch_segments$y[i+1])

    # Inelegant fix here for discontinuous lines being drawn outside of the
    # shape (e.g. between points of a star). The solution is to start by
    # checking the last side, rather than the first one. But it doesn't seem
    # like this would generalize to all irregular shapes...
    for(j in c(p_segs, 1:(p_segs-1))) {
    # for(j in 1:p_segs) {

      P3 <- c(df$x[j]  , df$y[j])
      P4 <- c(df$x[j+1], df$y[j+1])

      index <- index + 1
      res[[index]] <- line.line.intersection(P1, P2, P3, P4)

    }

  }

  hatch_points <- res %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(group = rep(1:(nrow(.)/2), each = 2))

  if(keep_outline) {
    df %>%
      dplyr::mutate(group = 0) %>%
      dplyr::bind_rows(hatch_points)
  } else {
    return(hatch_points)
  }

}

hatch_overlay <- function(df, spacing, angle) {

  width <- max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)
  diagonal <- sqrt(width^2 + height^2)
  # rotated hatch line length==diagonal of bounding box

  x_center <- min(df$x) + width/2
  y_center <- min(df$y) + height/2

  xmin <- x_center - diagonal/2
  xmax <- xmin + diagonal

  y <- seq(from = y_center - diagonal/2,
           to =   y_center + diagonal/2,
           by = spacing)

  paths <- data.frame(y = rep(y, each = 2),
                      x = c(xmin, xmax))

  rotate(paths, angle, x_center, y_center)

  # segments <- data.frame(x    = rep(xmin, length(y)),
  #                        xend = rep(xmax, length(y)),
  #                        y    = y,
  #                        yend = y)
  #
  # segments
  # segments_to_paths(segments)

}

# hatch_overlay <- function(df, spacing) {
#
#   width <- max(df$x) - min(df$x)
#   height <- max(df$y) - min(df$y)
#   diagonal <- sqrt(width^2 + height^2)
#   # rotated hatch line length==diagonal of bounding box
#
#   x_center <- min(df$x) + width/2
#   y_center <- min(df$y) + height/2
#
#   xmin <- min(df$x) - width * .1
#   xmax <- max(df$x) + width * .1
#   y <- seq(from = min(df$y) - height * .1,
#            to =   max(df$y) + height * .1,
#            by = spacing)
#
#   segments <- data.frame(x    = rep(xmin, length(y)),
#                          xend = rep(xmax, length(y)),
#                          y    = y,
#                          yend = y)
#
#   segments
#   # segments_to_paths(segments)
#
# }


##' Determine the intersection of two lines L1 and L2 in two dimensions,
##' using the formula described by Weisstein.
##' @title Determine intersection between two lines
##' @param P1 vector containing x,y coordinates of one end of L1
##' @param P2 vector containing x,y coordinates of other end of L1
##' @param P3 vector containing x,y coordinates of one end of L2
##' @param P4 vector containing x,y coordinates of other end of L2
##' @param interior.only boolean flag indicating whether only
##' intersections inside L1 and L2 should be returned.
##' @return Vector containing x,y coordinates of intersection of L1
##' and L2.  If L1 and L2 are parallel, this is infinite-valued.  If
##' \code{interior.only} is \code{TRUE}, then when the intersection
##' does not occur between P1 and P2 and P3 and P4, a vector
##' containing \code{NA}s is returned.
##' @source Weisstein, Eric W. "Line-Line Intersection."
##' From MathWorld--A Wolfram Web Resource.
##' \url{http://mathworld.wolfram.com/Line-LineIntersection.html}
##' @author David Sterratt
##' @export
##' @examples
##' ## Intersection of two intersecting lines
##' line.line.intersection(c(0, 0), c(1, 1), c(0, 1), c(1, 0))
##'
##' ## Two lines that don't intersect
##' line.line.intersection(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
line.line.intersection <- function(P1, P2, P3, P4, interior.only=TRUE) {
  P1 <- as.vector(P1)
  P2 <- as.vector(P2)
  P3 <- as.vector(P3)
  P4 <- as.vector(P4)

  dx1 <- P1[1] - P2[1]
  dx2 <- P3[1] - P4[1]
  dy1 <- P1[2] - P2[2]
  dy2 <- P3[2] - P4[2]

  D <- det(rbind(c(dx1, dy1),
                 c(dx2, dy2)))
  if (D==0) {
    return(c(Inf, Inf))
  }
  D1 <- det(rbind(P1, P2))
  D2 <- det(rbind(P3, P4))

  X <- det(rbind(c(D1, dx1),
                 c(D2, dx2)))/D
  Y <- det(rbind(c(D1, dy1),
                 c(D2, dy2)))/D

  if (interior.only) {
    ## Compute the fractions of L1 and L2 at which the intersection
    ## occurs
    lambda1 <- -((X-P1[1])*dx1 + (Y-P1[2])*dy1)/(dx1^2 + dy1^2)
    lambda2 <- -((X-P3[1])*dx2 + (Y-P3[2])*dy2)/(dx2^2 + dy2^2)
    if (!((lambda1>0) & (lambda1<1) &
          (lambda2>0) & (lambda2<1))) {
      return(data.frame(x = NA, y = NA))
    }
  }
  return(data.frame(x = X, y = Y))
}


rotate <- function(df, angle = pi/2, x_center, y_center) {

  # w <- (max(df$x) - min(df$x))
  # h <-( max(df$y) - min(df$y))
  # x_center <- min(df$x) + (max(df$x) - min(df$x))/2
  # y_center <- min(df$y) + (max(df$y) - min(df$y))/2

  dplyr::mutate(df,
                x = x - x_center,
                y = y - y_center,
                x0 = x * cos(angle) - y * sin(angle) + x_center,
                y0 = y * cos(angle) + x * sin(angle) + y_center) %>%
    dplyr::select(x = x0, y = y0)
}



rotatex <- function(df, angle = pi/2) {

  w <- (max(df$x) - min(df$x))
  h <-( max(df$y) - min(df$y))
  x_center <- min(df$x) + (max(df$x) - min(df$x))/2
  y_center <- min(df$y) + (max(df$y) - min(df$y))/2

  dplyr::mutate(df,
                x = x - x_center,
                y = y - y_center,
                x0 = x * cos(angle) - y * sin(angle),
                y0 = y * cos(angle) + x * sin(angle)) %>%
    dplyr::select(x = x0, y = y0)
}

odds <- function(x) {
  i <- 1:length(x)
  x[i%%2!=0]
}

evens <- function(x) {
  i <- 1:length(x)
  x[i%%2==0]
}


intersect <- function(poly_df, x = x, xend = xend, y = y, yend = yend, correction = FALSE, interior.only = TRUE) {

  out_x <- numeric()
  out_y <- numeric()
  rev <- FALSE

  for(i in 1:(nrow(poly_df)-1)) {
    j <- i+1
    line1 <- c(x,y)
    line2 <- c(xend,yend)
    poly1 <- c(poly_df$x[i], poly_df$y[i])
    poly2 <- c(poly_df$x[j], poly_df$y[j])

    int_coords <- line.line.intersection(P1 = line1,
                                         P2 = line2,
                                         P3 = poly1,
                                         P4 = poly2,
                                         interior.only = TRUE)

    ix <- int_coords[1]
    iy <- int_coords[2]

    if(!is.na(ix) & !is.infinite(ix)) {
      out_x[length(out_x)+1] <- ix
      out_y[length(out_y)+1] <- iy

      if(i==1) rev = TRUE
    }

  }

  # out <- unlist(out)
  # out <- out[!is.na(out) & !is.infinite(out)]

  # the function works weirdly, where sometimes it'll give NA for
  # a line that hits the exact end of a ploygon edge. This is a hacky
  # workaround and I'm not even sure why it works...
  if(correction & length(out_x)==2) {
    out <- intersect(dplyr::mutate(poly_df, x = x+.1, y = y), x = x, xend = xend, y = y, yend = yend)
  }

  if(length(out_x)<2) return(NULL)
  if(length(out_y)<2) return(NULL)

  # if(rev) {
  # l <- length(out_x)
  # out_x <- c(out_x[2:l], out_x[1])
  # out_y <- c(out_y[2:l], out_y[1])
  # }
  #

  out <- data.frame(x = out_x, y = out_y) %>%
    # group_by(y) %>%
    dplyr::arrange(x)

  out_x <- out$x
  out_y <- out$y


  len_out <- length(out_x)
  # len_out <- len_out - (len_out%%2)

  out <- data.frame(ix = odds(out_x)[1:len_out],
                ixend = evens(out_x)[1:len_out],
                iy = odds(out_y)[1:len_out],
                iyend = evens(out_y)[1:len_out])

  return(out)

}


# for this version, rotate the whole polygon instead of lines
# then can sort intersection coords left to right (or right to left)?

hatch2 <- function(poly_df, angle = 0, spacing = 1, correction = FALSE) {

  # first, rotate the polygon df
  width <- (max(poly_df$x) - min(poly_df$x))
  height <- (max(poly_df$y) - min(poly_df$y))
  x_center <- min(poly_df$x) + (max(poly_df$x) - min(poly_df$x))/2
  y_center <- min(poly_df$y) + (max(poly_df$y) - min(poly_df$y))/2

  poly_df <- poly_df %>%
    rotate_poly(angle)
  # }
  # now create the hatching df
  # w <- (max(poly_df$x) - min(poly_df$x))
  # h <- (max(poly_df$y) - min(poly_df$y))
  # x_center <- min(poly_df$x) + (max(poly_df$x) - min(poly_df$x))/2
  # y_center <- min(poly_df$y) + ( max(poly_df$y) - min(poly_df$y))/2

  h <- tibble::tibble(y = seq(min(poly_df$y)*1.1,max(poly_df$y)*1.1,by=spacing),
                      yend = y,
                      x = rep(c(min(poly_df$x)*1.1, max(poly_df$x)*1.1), length.out = length(y)),
                      xend = rep(c(max(poly_df$x)*1.1,min(poly_df$x)*1.1), length.out = length(y)))

  # now figure out where hatch lines intersect the polygon
  h <- h %>%
    dplyr::rowwise() %>%
    dplyr::mutate(inter = list(intersect(poly_df, x, xend, y, yend, correction = correction))) %>%
    tidyr::unnest_wider(inter) %>%
    tidyr::unnest(cols = c(ix, ixend, iy, iyend)) %>%
    dplyr::filter(!is.na(ix)) %>%
    dplyr::select(x = ix, y = iy, xend = ixend, yend = iyend)

  # if(angle != 0){
  angle = -angle
  # width <- (max(poly_df$x) - min(poly_df$x))
  # height <- (max(poly_df$y) - min(poly_df$y))
  # x_center <- min(poly_df$x) + (max(poly_df$x) - min(poly_df$x))/2
  # y_center <- min(poly_df$y) + (max(poly_df$y) - min(poly_df$y))/2
  h <- h %>%
    # rowwise() %>%
    dplyr::mutate(x0 = x * cos(angle) - y * sin(angle) + x_center,
           xend0 = xend * cos(angle) - yend * sin(angle) + x_center,
           y0 = y * cos(angle) + x * sin(angle) + y_center,
           yend0 = yend * cos(angle) + xend * sin(angle) + y_center) %>%
    dplyr::select(x = x0, y = y0, xend = xend0, yend = yend0)
  # }
  return(h)

}

paths_to_segments <- function(df) {

}

segments_to_paths <- function(df) {
  df %>%
    dplyr::filter(!is.na(x) & !is.na(xend)) %>%
    # dplyr::mutate(group = 1:dplyr::n()) %>%
    tibble::rowid_to_column("row") %>%
    dplyr::mutate(x_ =    ifelse(row %% 2 != 0, x,    xend),
                  xend_ = ifelse(row %% 2 != 0, xend, x),
                  y_ =    ifelse(row %% 2 != 0, y,    yend),
                  yend_ = ifelse(row %% 2 != 0, yend, y)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x = list(c(x_, xend_)), y = list(c(y_,yend_))) %>%
    tidyr::unnest(cols = c(x, y)) %>%
    dplyr::select(x, y, group = row)
}


