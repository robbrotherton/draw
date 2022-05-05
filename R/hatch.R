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

  if(!"group" %in% names(df)) {
    df$group <- 0
  }

  width <-  max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)

  center_x <- min(df$x) + width/2
  center_y <- min(df$y) + height/2

  # First, create hatch segments in a bounding box with width and height equal
  # to the diagonal of the shapex, then rotate that df of hatch lines to the
  # desired angle
  hatch_paths <- df %>%
    hatch_overlay(spacing) %>%
    rotate(angle, around = c(center_x, center_y))

  # Now we take the endpoints of each hatch path and check for intersections
  # with each segment of the polygon
  h_segs <- nrow(hatch_paths)
  p_segs <- nrow(df) - 1

  # We'll have a data.frame for each line, then bind_rows() at the end
  res <- vector(mode = "list", length = h_segs/2)
  index <- 0

  for(i in seq(from = 1, to = h_segs, by = 2)) {

    P1 <- c(hatch_paths$x[i]   , hatch_paths$y[i])
    P2 <- c(hatch_paths$x[i+1] , hatch_paths$y[i+1])

    # The solution is to save all intersections for a single line together, then
    # arrange them by distance from the origin of the hatch line. This should
    # generalize to any shape and makes the order of the polygon segments
    # irrelevant.

    line_intersections_df <- data.frame(x = vector("numeric", length = p_segs),
                                        y = vector("numeric", length = p_segs))

    for(j in 1:p_segs) {

      if(df$group[j+1] != df$group[j]) {
        # Need to skip rows where the next row is from a different polygon
        line_intersections_df[j,] <- data.frame(x = NA, y = NA)
        next
      }

      P3 <- c(df$x[j]  , df$y[j])
      P4 <- c(df$x[j+1], df$y[j+1])

      line_intersections_df[j,] <- line.line.intersection(P1, P2, P3, P4)

    }

    index <- index + 1
    res[[index]] <- line_intersections_df %>%
      # tidyr::drop_na() %>%
      dplyr::mutate(d = (x - P1[1])^2 + (y - P1[2]^2)^2) %>%
      dplyr::arrange(d)

  }

  # Last, clean and organize the output data
  hatch_points <- res %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(group = rep(1:(nrow(.)/2), each = 2) + max(df$group))

  if(keep_outline) {
    df %>%
      # dplyr::mutate(group = 0) %>%
      dplyr::bind_rows(hatch_points)
  } else {
    return(hatch_points)
  }

}



hatch_overlay <- function(df, spacing) {

  width <- max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)
  diagonal <- sqrt(width^2 + height^2)
  # rotated hatch line length==diagonal of bounding box

  # Need something in here to make sure hatch lines are drawn on the spacing
  # intervals. I.e the starting number should be divisible by the spacing.
  # s <- .1
  # seq(floor(1.53*(1/s))/(1/s), ceiling(2.17*(1/s))/(1/s), s)


  x_center <- min(df$x) + width/2
  y_center <- min(df$y) + height/2

  xmin <- x_center - diagonal/2
  xmax <- xmin + diagonal

  y <- seq(from = y_center - diagonal/2,
           to =   y_center + diagonal/2,
           by = spacing)

  data.frame(y = rep(y, each = 2),
             x = c(xmin, xmax))

}


line_to_wave <- function(P1, P2, points = 50, frequency = .1, amplitude = .1) {

  points_x <- seq(from = P1[1], to = P2[1], length.out = points)
  points_y <- seq(from = P1[2], to = P2[2], length.out = points)

  tibble::tibble(x = points_x,
                 y = points_y + cos(x/frequency) * amplitude)


}

lines_to_waves <- function(hatch_df, points = 50, frequency = .1, amplitude = .1) {

  out <- vector(mode = "list", length = nrow(hatch_df)/2)
  index <- 0

  for (l in seq(1, nrow(hatch_df), by = 2)) {

    index <- index + 1

    P1 <- c(hatch_df$x[l], hatch_df$y[l])
    P2 <- c(hatch_df$x[l+1], hatch_df$y[l+1])

    out[[index]] <- line_to_wave(P1, P2, points, frequency, amplitude)

  }

  dplyr::bind_rows(out, .id = "group")

}

# d <- square() %>%
#   hatch_overlay(spacing = .05) %>%
#   rotate(pi*.25) %>%
#   lines_to_waves() %>%
#   dplyr::mutate(inside = points_in_polygons(data.frame(x, y),
#                                             dplyr::mutate(square(), group = 0)))
# #
# # d$inside <- points_in_polygons(d, dplyr::mutate(square(), group = 0))
# #
# # # could do this within the pipe...
# #   # dplyr::mutate(inside = points_in_polygons(data.frame(x, y),
# #                                             # dplyr::mutate(square(), group = 0))) %>%
# #   # rotate(pi*.25) %>%
# #   # dplyr::mutate(group = 1) %>%
# ggplot2::ggplot() +
#   ggplot2::geom_path(data = dplyr::filter(d, inside), ggplot2::aes(x, y, group = group, color = inside)) +
#   ggplot2::geom_path(data = square(), ggplot2::aes(x, y, group = NULL)) +
#   ggplot2::coord_fixed()


hatch_wave <- function(df, spacing = .1,
                       angle = 0,
                       frequency = .1,
                       amplitude = .1,
                       keep_outline = TRUE,
                       single_line = FALSE) {

  # First create hatch segments in a bounding box with width and height equal to
  # the diagonal of the shape
  hatch_paths <- hatch_overlay(df, spacing) %>%
    lines_to_waves(frequency, amplitude) %>%
    rotate(angle, center_x, center_y)

  # Now instead of taking the endpoints of each hatch path and checking for
  # intersections with each segment of the polygon, we need to take each point
  # along the line and check if it's inside or outside of the polygon

  # hatch_paths$inside <- points_in_polygons(hatch_paths, df)

  # # Want this to return a data.frame
  #
  # hatch_points <- res %>%
  #   dplyr::bind_rows() %>%
  #   tidyr::drop_na() %>%
  #   dplyr::mutate(group = rep(1:(nrow(.)/2), each = 2))
  #
  # if(keep_outline) {
  #   df %>%
  #     dplyr::mutate(group = 0) %>%
  #     dplyr::bind_rows(hatch_points)
  # } else {
  #   return(hatch_points)
  # }

  hatch_paths

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




rotate <- function(df, angle = pi/2, around = c(0, 0)) {

  # w <- (max(df$x) - min(df$x))
  # h <-( max(df$y) - min(df$y))
  # x_center <- min(df$x) + (max(df$x) - min(df$x))/2
  # y_center <- min(df$y) + (max(df$y) - min(df$y))/2

  dplyr::mutate(df,
                x0 = x - around[1],
                y0 = y - around[2],
                x = x0 * cos(angle) - y0 * sin(angle) + around[1],
                y = y0 * cos(angle) + x0 * sin(angle) + around[2]) %>%
    dplyr::select(-x0, -y0)
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


