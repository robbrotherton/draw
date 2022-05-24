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
#' circle() |> hatch() |> show()
hatch <- function(df, spacing = .1, angle = 0, keep_outline = TRUE, single_line = FALSE) {

  if(!"group" %in% names(df)) {
    df$group <- 0
  }

  width <-  max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)

  center_x <- min(df$x) + width/2
  center_y <- min(df$y) + height/2

  # First, create hatch segments in a bounding box with width and height equal
  # to the diagonal of the shape, then rotate that df of hatch lines to the
  # desired angle
  hatch_paths <- df |>
    hatch_overlay(spacing) |>
    rotate(angle, around = c(center_x, center_y)) |>
    clip_hatch_lines(df)

  # Clean and organize the output data
  hatch_points <- hatch_paths |>
    dplyr::bind_rows() |>
    tidyr::drop_na() |>
    dplyr::mutate(group = rep(1:(dplyr::n()/2), each = 2) + max(df$group))

  if(keep_outline) {
    dplyr::bind_rows(df, hatch_points)
  } else {
    hatch_points
  }

}

# important tests to add:
# star() |> hatch(angle = pi*.5, keep_outline = TRUE) |> show()
# make sure no points outside star, middle line bisecting star is present

# star() |> hatch(spacing = .01, angle = pi/5) |> show()
# make sure no points outside star, middle line bisecting star is present

# rectangle() |> hatch(spacing = .1, angle = pi/2) |> show()

# letters("MABEL") |> hatch(spacing = .01, angle = pi*.5) |> show()

# hatch_multi
# purrr::map_df(c(pi*.4, pi*.6), ~square() |>
# hatch(angle = .x, keep_outline = FALSE), .id = "unit") |>
#   dplyr::group_by(unit, group) |>
#   dplyr::mutate(group = dplyr::cur_group_id()) |>
#   show()

hatch_overlay <- function(df, spacing) {

  width <- max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)
  diagonal <- sqrt(width^2 + height^2)
  # rotated hatch line length==diagonal of bounding box

  half_diagonal <- ceiling_spacing(diagonal/2, spacing)

  # Need something in here to make sure hatch lines are drawn on the spacing
  # intervals. I.e the starting number should be divisible by the spacing.
  # s <- .1
  # seq(floor(1.53*(1/s))/(1/s), ceiling(2.17*(1/s))/(1/s), s)

  x_center <- min(df$x) + width/2
  y_center <- min(df$y) + height/2

  xmin <- x_center - half_diagonal
  xmax <- x_center + half_diagonal


  ymin <- y_center - half_diagonal
  ymax <- y_center + half_diagonal
#
#   y <- seq(from = ymin,
#            to =   ymax,
#            by = spacing)

  y <- seq(from = ymin, #diagonal/2,
           to =   ymax, #diagonal/2,
           by = spacing)

  data.frame(y = rep(y, each = 2),
             x = c(xmin, xmax)) # + 1e-3

}

clip_hatch_lines <- function(hatch_df, polygon_df) {

  # Check each hatch segment  for intersections with each segment of the polygon
  h_segs <- nrow(hatch_df)
  p_segs <- nrow(polygon_df) - 1

  # We'll have a data.frame for each line, then bind_rows() at the end
  res <- vector(mode = "list", length = h_segs/2)
  index <- 0

  for(i in seq(from = 1, to = h_segs, by = 2)) {

    P1 <- c(hatch_df$x[i]   , hatch_df$y[i])
    P2 <- c(hatch_df$x[i+1] , hatch_df$y[i+1])

    # The solution is to save all intersections for a single line together, then
    # arrange them by distance from the origin of the hatch line. This should
    # generalize to any shape and makes the order of the polygon segments
    # irrelevant.

    line_intersections_df <- data.frame(x = vector("numeric", length = p_segs),
                                        y = vector("numeric", length = p_segs))

    for(j in 1:p_segs) {

      if(polygon_df$group[j+1] != polygon_df$group[j]) {
        # Need to skip rows where the next row is from a different polygon
        line_intersections_df[j,] <- data.frame(x = NA, y = NA)
        next
      }

      P3 <- c(polygon_df$x[j]  , polygon_df$y[j])
      P4 <- c(polygon_df$x[j+1], polygon_df$y[j+1])

      line_intersections_df[j,] <- lineLineIntersection(P1, P2, P3, P4)

    }

    line_intersections_df <- line_intersections_df |>
      dplyr::distinct() |>
      tidyr::drop_na()

    index <- index + 1

    # Need some checks here before adding a result to the list.
    # Is there just a single point? Can happen if a hatch line clips one corner
    # Or 3 points? Or any odd number?
    # Does the hatch segment duplicate a segment of the polygon?

    if(nrow(line_intersections_df) %% 2 == 0) {

      res[[index]] <- line_intersections_df |>
        dplyr::mutate(d = (x - P1[1])^2 + (y - P1[2]^2)^2)

      if(index %% 2 == 0) {
        res[[index]] <- res[[index]] |>
          dplyr::arrange(-d)
      } else {
        res[[index]] <- res[[index]] |>
          dplyr::arrange(d)
      }

    }

  }

  res

}
# sig.
# waldo::compare(.0001, 0, tolerance = .0001)
# expect_equal(1e-1, 0, tolerance = e-1)
# 1e-324==0

# lineLineIntersection(c(-.5, -1), c(-.5, 1),
#                      c(-.5, -.5), c(-.5, .5))




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

  dplyr::bind_rows(out, .id = "group") |>
    dplyr::mutate(group = as.numeric(group))

}

# d <- square() |>
#   hatch_overlay(spacing = .05) |>
#   rotate(pi*.25) |>
#   lines_to_waves() |>
#   dplyr::mutate(inside = pointsInPolygons(data.frame(x, y),
#                                             dplyr::mutate(square(), group = 1)))
# #
# d$inside <- points_in_polygons(d, dplyr::mutate(square(), group = 1))
# #
# # # could do this within the pipe...
# #   # dplyr::mutate(inside = points_in_polygons(data.frame(x, y),
# #                                             # dplyr::mutate(square(), group = 0))) |>
# #   # rotate(pi*.25) |>
# #   # dplyr::mutate(group = 1) |>
# ggplot2::ggplot() +
#   ggplot2::geom_path(data = d, ggplot2::aes(x, y, group = group, color = inside)) +
#   ggplot2::geom_path(data = square(), ggplot2::aes(x, y, group = NULL)) +
#   ggplot2::coord_fixed()

# square() |>
#   hatch_overlay(.1) |>
#   lines_to_waves(f = .1, a = .1) |>
#   rotate(0) |>
#   show()


hatch_wave <- function(df, spacing = .1,
                       angle = 0,
                       frequency = .1,
                       amplitude = .1,
                       neat_edges = FALSE,
                       keep_outline = TRUE,
                       single_line = FALSE) {

  if(!"group" %in% names(df)) {
    df$group <- 1
  }

  # First create hatch segments in a bounding box with width and height equal to
  # the diagonal of the shape
  hatch_paths <- df |>
    hatch_overlay(spacing) |>
    lines_to_waves(frequency = frequency, amplitude = amplitude) |>
    rotate(angle)

  # Now instead of taking the endpoints of each hatch path and checking for
  # intersections with each segment of the polygon, we need to take each point
  # along the line and check if it's inside or outside of the polygon

  hatch_paths$inside <- pointsInPolygons(hatch_paths, df)

  # Need to update group ids here, since a line might pass out of the polygon
  # and then come back in, resulting in two separate sections
  hatch_paths <- hatch_paths |>
    dplyr::rename(line = group) |>
    dplyr::group_by(line) |>
    dplyr::mutate(subsection = cumsum(inside!=dplyr::lag(inside, default = 1))) |>
    dplyr::group_by(line, subsection) |>
    dplyr::mutate(group = dplyr::cur_group_id())
  # return(hatch_paths)

  if(neat_edges) {
    hatch_paths <- tidy_edges(hatch_paths, df) |>
      dplyr::group_by(line, subsection) |>
      dplyr::mutate(group = dplyr::cur_group_id())
  }

  # Need to do two things here:
  # 1: clean intersections between waves and poly boundary
  # 2: revise line groups, increment group when a line passes out and back into polygon

  hatch_paths <- dplyr::filter(hatch_paths, inside)

  if(keep_outline) {
    df |>
      # dplyr::mutate(group = 0) |>
      dplyr::bind_rows(hatch_paths)
  } else {
    return(hatch_paths)
  }

}




tidy_edges <- function(hatch_df, poly_df) {

  z <- hatch_df |>
    tibble::rowid_to_column("point_id")

  # Drop any lines/sections that are entierly outside of the polygon
  z2 <- z |>
    dplyr::group_by(subsection) |>
    dplyr::filter(sum(inside) > 0)

  # Need to tag points as being first of last of their subsection
  first_points <- z2 |>
    dplyr::group_by(line, subsection) |>
    dplyr::slice(1)

  last_points <- z2 |>
    dplyr::group_by(line, subsection) |>
    dplyr::slice(dplyr::n())

  # Now can compute new points which intersect with edges of polygon

  # deal with first points----
  for(i in seq_len(nrow(first_points))) {
    this_point_id <- first_points$point_id[i]
    this_point_subsection <- first_points$subsection[i]
    this_point_x <- first_points$x[i]
    this_point_y <- first_points$y[i]

    prev_point_id <- z$point_id[z$point_id==(this_point_id - 1)]
    prev_point_x <- z$x[z$point_id==prev_point_id]
    prev_point_y <- z$y[z$point_id==prev_point_id]

    P1 <- c(this_point_x, this_point_y)
    P2 <- c(prev_point_x, prev_point_y)

    for(j in seq_len(nrow(poly_df) - 1)) {
      P3 <- c(poly_df$x[j],   poly_df$y[j])
      P4 <- c(poly_df$x[j+1], poly_df$y[j+1])

      intersection <- lineLineIntersection(P1, P2, P3, P4)

      # IF there is an intersection, stop checking
      if(!any(is.na(intersection))) break

    }

    # intersections[[i]] <- intersection
    z$x[z$point_id==prev_point_id] <- intersection$x[1]
    z$y[z$point_id==prev_point_id] <- intersection$y[1]
    z$subsection[z$point_id==prev_point_id] <- this_point_subsection
    z$inside[z$point_id==prev_point_id] <- TRUE

  }

  # deal with last points ----
  for(i in seq_len(nrow(last_points))) {
    this_point_id         <- last_points$point_id[i]
    this_point_subsection <- last_points$subsection[i]
    this_point_x          <- last_points$x[i]
    this_point_y          <- last_points$y[i]

    prev_point_id <- z$point_id[z$point_id==(this_point_id + 1)]
    prev_point_x <- z$x[z$point_id==prev_point_id]
    prev_point_y <- z$y[z$point_id==prev_point_id]

    P1 <- c(this_point_x, this_point_y)
    P2 <- c(prev_point_x, prev_point_y)

    for(j in seq_len(nrow(poly_df) - 1)) {
      P3 <- c(poly_df$x[j],   poly_df$y[j])
      P4 <- c(poly_df$x[j+1], poly_df$y[j+1])

      intersection <- lineLineIntersection(P1, P2, P3, P4)

      # IF there is an intersection, stop checking
      if(!any(is.na(intersection))) break

    }

    # intersections[[i]] <- intersection
    z$x[z$point_id==prev_point_id] <- intersection$x[1]
    z$y[z$point_id==prev_point_id] <- intersection$y[1]
    z$subsection[z$point_id==prev_point_id] <- this_point_subsection
    z$inside[z$point_id==prev_point_id] <- TRUE

  }

  z

}


# hatch_wave(dplyr::mutate(square(), group = 1)) |>
#   ggplot2::ggplot(ggplot2::aes(x, y, color = group, group = group)) +
#   ggplot2::geom_path() +
#   ggplot2::coord_fixed()


# paths_to_segments <- function(df) {
#
# }




fill_inset <- function(df, spacing = .1, single_line = TRUE) {

  data <- purrr::map_df(.x = seq(1, to = 0+spacing, by = -spacing),
                        .f = ~df * .x,
                        .id = "group") |>
    dplyr::mutate(group = as.numeric(group))

  if (single_line) {

    # Need to edit the last point of each group

    # Tried this as a 'proportion of line' problem but couldn't get it to work
    # reliably. Still seems like there should be a viable solution that way, but
    # I'm reframing as a line intersection problem instead: Where does the first
    # line of the n+1th polygon intersect the last line of the nth polygon?

    n <- nrow(df)
    groups <- unique(data$group)

    for (i in 1:(length(groups)-1)) {

      P1 <- c(data$x[n*i-1], data$y[n*i-1])
      P2 <- c(data$x[n*i]  , data$y[n*i])
      P3 <- c(data$x[n*i+2], data$y[n*i+2])
      P4 <- c(data$x[n*i+1], data$y[n*i+1])
      # print(c(P1, P2, P3, P4))
      #
      int <- line.line.intersection(P1, P2, P3, P4, interior.only = FALSE)

      # print(int)

      data$x[n*i] <- int$x
      data$y[n*i] <- int$y

      data$group <- 1

    }


  }

  # drop the last row, since it completes the innermost shape
  data[1:(nrow(data)-1),]

}
