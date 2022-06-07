#' @title Hatch fill a polygon
#'
#' @param df A data.frame containing x and y coordinates for a single polygon
#' @param spacing the spacing between lines (if n is not specified)
#' @param angle In radians
#' @param keep_outline Logical: keep or drop the original outline of the polygon
#' @param single_line Logical: Arrange the interior hatch lines into a single unbroken
#'   line (good for pen plotter). Overrides \code{keep_outline}
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' circle() |> hatch() |> show()
fill_hatch <- function(df, spacing = .1, angle = NULL, keep_outline = TRUE, single_line = FALSE) {

  if(!"group" %in% names(df)) {
    df$group <- 1
  }

  width <-  max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)

  center_x <- min(df$x) + width/2
  center_y <- min(df$y) + height/2

  hatch_paths <- df |>
    hatch_overlay(spacing)

  if(!is.null(angle)) {
    hatch_paths <- purrr::map_df(angle, ~rotate(hatch_paths, .x, around = c(center_x, center_y)))
  }

  hatch_paths <- hatch_paths |>
    hatch_to_segments() |>
    # clip_paths(df) |>
    clip_paths_complex(df) |>
    dplyr::bind_rows() |>
    dplyr::select(x, y, group = seg_id)

  if(keep_outline) {
    hatch_paths <- dplyr::mutate(hatch_paths, group = group + max(df$group))
    dplyr::bind_rows(df, hatch_paths)
  } else {
    hatch_paths
  }

}


#' Fill a polygon with sine waves
#'
#' @param df
#' @param spacing
#' @param angle
#' @param frequency
#' @param amplitude
#' @param neat_edges
#' @param keep_outline
#' @param single_line
#'
#' @return
#' @export
#'
#' @examples
fill_wave <- function(df,
                      spacing = .1,
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
    rotate(angle) |>
    paths_to_segments() |>
    clip_paths_complex(df) |>
    dplyr::bind_rows() |>
    dplyr::select(x, y, group = seg_id)

  if(keep_outline) {
    df |>
      dplyr::mutate(group = 0) |>
      dplyr::bind_rows(hatch_paths)
  } else {
    return(hatch_paths)
  }

}


#' Fill a polygon with a zigzag pattern
#'
#' @param df
#' @param spacing
#' @param angle
#' @param frequency
#' @param amplitude
#' @param neat_edges
#' @param keep_outline
#' @param single_line
#'
#' @return
#' @export
#'
#' @examples
fill_zigzag <- function(df,
                        spacing = .1,
                        angle = 0,
                        frequency = .1,
                        amplitude = .1,
                        neat_edges = TRUE,
                        keep_outline = TRUE,
                        single_line = FALSE) {

  if(!"group" %in% names(df)) {
    df$group <- 1
  }

  # First create hatch segments in a bounding box with width and height equal to
  # the diagonal of the shape
  hatch_paths <- df |>
    hatch_overlay(spacing) |>
    lines_to_zigzag(frequency = frequency, amplitude = amplitude) |>
    rotate(angle) |>
    paths_to_segments() |>
    clip_paths_complex(df) |>
    dplyr::bind_rows() |>
    dplyr::select(x, y, group = seg_id)


  if(keep_outline) {
    df |>
      dplyr::mutate(group = 0) |>
      dplyr::bind_rows(hatch_paths)
  } else {
    return(hatch_paths)
  }

}


#' Fill a polygon with smaller versions of itself
#'
#' @param df
#' @param spacing
#' @param single_line
#'
#' @return
#' @export
#'
#' @examples
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

      # Edit cpp function to handle this case
      int <- line.line.intersection(P1, P2, P3, P4, interior.only = FALSE)

      data$x[n*i] <- int$x
      data$y[n*i] <- int$y

      data$group <- 1

    }


  }

  # drop the last row, since it completes the innermost shape
  data[1:(nrow(data)-1),]

}


# Unexported helpers ------------------------------------------------------


hatch_overlay <- function(df, spacing) {

  width <- max(df$x) - min(df$x)
  height <- max(df$y) - min(df$y)
  diagonal <- sqrt(width^2 + height^2)
  # rotated hatch line length==diagonal of bounding box

  half_diagonal <- ceiling_spacing(diagonal/2, spacing)

  x_center <- min(df$x) + width/2
  y_center <- min(df$y) + height/2

  xmin <- x_center - half_diagonal
  xmax <- x_center + half_diagonal


  ymin <- y_center - half_diagonal
  ymax <- y_center + half_diagonal

  y <- seq(from = ymin, #diagonal/2,
           to =   ymax, #diagonal/2,
           by = spacing)

  # Random spacing...
  # y <- runif(length(y), min = ymin, max = ymax) |>
  #   sort()

  data.frame(y = rep(y, each = 2),
             x = c(xmin, xmax)) # + 1e-3

}

clip_hatch_lines <- function(hatch_df, polygon_df) {

  # Check each hatch segment for intersections with each segment of the polygon
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

      line_intersections_df[j,] <- line_intersection(P1, P2, P3, P4)

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

line_to_zigzag <- function(P1, P2, frequency = .1, amplitude = .1) {

  distance <- distance(P1, P2)
  points <- ceiling(distance/frequency + 1)

  x <- seq(from = P1[1], to = P2[1], length.out = points)
  y <- seq(from = P1[2], to = P2[2], length.out = points)

  n <- 1:points

  # y_offset <- rep(c(amplitude/2, -amplitude/2), points/2)

  # print(c(y, y_offset))

  tibble::tibble(x = x,
                 y = y + ifelse(n %% 2 == 0, amplitude/2, -amplitude/2))

}


lines_to_zigzag <- function(hatch_df, frequency = .1, amplitude = .1) {

  out <- vector(mode = "list", length = nrow(hatch_df)/2)
  index <- 0

  for (l in seq(1, nrow(hatch_df), by = 2)) {

    index <- index + 1

    P1 <- c(hatch_df$x[l], hatch_df$y[l])
    P2 <- c(hatch_df$x[l+1], hatch_df$y[l+1])

    out[[index]] <- line_to_zigzag(P1, P2, frequency, amplitude)

  }

  dplyr::bind_rows(out, .id = "group") |>
    dplyr::mutate(group = as.numeric(group))

}


tidy_edges <- function(hatch_df, poly_df) {

  z <- hatch_df |>
    tibble::rowid_to_column("point_id")

  # Drop any lines/sections that are entirely outside of the polygon
  z2 <- z |>
    # dplyr::group_by(subsection) |>
    dplyr::filter(inside)

  # Need to tag points as being first of last of their subsection
  first_points <- z2 |>
    dplyr::group_by(line, subsection) |>
    dplyr::slice(1) |>
    dplyr::mutate(x2 = z$x[z$point_id==point_id - 1],
                  y2 = z$y[z$point_id==point_id - 1],
                  int = list(find_intersection(P1 = c(x, y),
                                               P2 = c(x2, y2),
                                               poly = poly_df)))

  last_points <- z2 |>
    dplyr::group_by(line, subsection) |>
    dplyr::slice(dplyr::n()) |>
    dplyr::mutate(x2 = z$x[z$point_id==point_id + 1],
                  y2 = z$y[z$point_id==point_id + 1],
                  int = list(find_intersection(P1 = c(x, y),
                                               P2 = c(x2, y2),
                                               poly = poly_df)))

  # Now can compute new points which intersect with edges of polygon

  z3 <- z2 |>
    dplyr::group_by(line, subsection) |>
    dplyr::group_split() |>
    purrr::map2(.y = first_points$int,
                .f = ~dplyr::add_row(.x,
                                     x = .y$x,
                                     y = .y$y,
                                     inside = TRUE,
                                     line = .x$line[1],
                                     subsection = .x$subsection[1],
                                     .before = 1)) |>
    purrr::map2(.y = last_points$int,
                .f = ~dplyr::add_row(.x,
                                     x = .y$x,
                                     y = .y$y,
                                     inside = TRUE,
                                     line = .x$line[1],
                                     subsection = .x$subsection[1],
                                     .after = nrow(.x)))


  z4 <- z3 |>
    dplyr::bind_rows() |>
    dplyr::group_by(line, subsection) |>
    dplyr::mutate(group = dplyr::cur_group_id()) |>
    dplyr::filter(!is.na(x))

  # This is leaving out some segments for which both endpoints are outside the
  # polygon. Maybe this is an inefficient fix (and a baroque approach in general), but
  # applying clip_hatch_lines picks up those segments

  x <- hatch_df |>
    # dplyr::filter(!inside) |>
    dplyr::group_by(line) |>
    dplyr::group_split() |>
    purrr::map_df(~clip_hatch_lines(.x, poly_df))

  if(nrow(x) > 1) {
    x |>
      dplyr::mutate(group = rep(1:(dplyr::n()/2), each = 2) + max(z4$group),
                    inside = TRUE) |>
      dplyr::bind_rows(z4)
  } else {
    z4
  }

}

find_intersection <- function(P1, P2, poly) {

  # P1 is the point in question
  # P2 is the neighboring point (before or after)

  for(i in seq_len(nrow(poly)-1)) {
    P3 <- c(poly$x[i], poly$y[i])
    P4 <- c(poly$x[i+1], poly$y[i+1])

    # first check if the point (P1) is on a polygon segment
    # if(distance(P3, P1) + distance(P4, P1) == distance(P3, P4)) next

    res <- lineLineIntersection(P1, P2, P3, P4, include_lineend = TRUE)

    if(!is.na(res$x[1])) return(res)

    # if(!is.na(res$x[1])) {
    #   if(res$x[1]==P1[1] & res$y[1]==P1[2]) next
    # } else {
    #   return(res)
    # }
  }
  return(data.frame(x = NA, y = NA))
}
