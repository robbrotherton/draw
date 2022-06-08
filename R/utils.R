#' Title
#'
#' @param df
#' @param nrow
#' @param ncol
#' @param spacing
#'
#' @return
#' @export
#'
#' @examples
arrange_grid <- function(df, nrow = NULL, ncol = NULL, spacing = 1) {

  groups <- unique(df$group)
  n_groups <- length(groups)

  offsets <- data.frame(group = groups,
                        x_offset = rep(seq(1, length.out = nrow, by = spacing), ncol),
                        y_offset = rep(seq(1, length.out = ncol, by = spacing), each = nrow))

  dplyr::left_join(df, offsets, by = "group") |>
    dplyr::mutate(x = x + x_offset,
                  y = y + y_offset)

}

#
# shapes <- dplyr::bind_rows(circle(),
#                            square(),
#                            star(),
#                            polygon(sides = 6),
#                            rectangle(width = 1.5, height = 1),
#                            heart(),
#                            .id = "group")

# purrr::map_df(1:16, ~square(), .id = "group") |>
#   arrange_grid(4, 4, 1.1) |>
#   dplyr::mutate(group = as.numeric(group)) |>
#   hatch(angle = pi/4) |>
#   show()
# as.logical(as.integer(runif(1)+.5))
#
# purrr::map_df(1:16, ~square(), .id = "group") |>
#   arrange_grid(4, 4, 1) |>
#   dplyr::mutate(group = as.numeric(group)) |>
#   dplyr::group_split(group) |>
#   purrr::map2_df(.y = runif(16, max = pi), ~if(as.logical(as.integer(runif(1)+.5))) hatch(.x, angle = .y) else .x, .id = "unit") |>
#   dplyr::group_by(unit, group) |>
#   dplyr::mutate(group = dplyr::cur_group_id()) |>
#   show(void = TRUE)
# #
# square() |>
#   hatch(angle = pi/4) |>
#   show()

# circle() |>
#   hatch(angle = pi/2) |>
#   show()

# polygon(radius = .5) |>
#   rotate(.1) |>
#   hatch(spacing = .01, angle = .1, keep = FALSE) |>
#   dplyr::bind_rows(polygon()) |>
#   show()


decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


distance <- function(P1, P2) {
  sqrt((P1[1] - P2[1])^2 + (P1[2] - P2[2])^2)
}

# polygon_to_segments <- function(df) {
#
#   starts <- df |>
#     dplyr::slice(seq(1, dplyr::n(), 2))
#
#   ends <- df |>
#     dplyr::slice(seq(2, dplyr::n(), 2)) |>
#     dplyr::rename(xend = x, yend = y)
#
#   dplyr::bind_cols(starts, ends)
#
# }

hatch_to_segments <- function(df) {

  starts <- df |>
    dplyr::slice(seq(1, dplyr::n(), 2))

  ends <- df |>
    dplyr::slice(seq(2, dplyr::n(), 2)) |>
    dplyr::rename(xend = x, yend = y)

  dplyr::bind_cols(starts, ends)

}

paths_to_segments <- function(df) {

  # dplyr::group_by(df, group) |>
    dplyr::mutate(df,
                  xend = ifelse(group==dplyr::lead(group),
                                dplyr::lead(x), NA),
                  yend = ifelse(group==dplyr::lead(group),
                                dplyr::lead(y), NA)) |>
    tidyr::drop_na()

  # starts <- df |>
  #   dplyr::slice(seq(1, dplyr::n(), 2))
  #
  # ends <- df |>
  #   dplyr::slice(seq(2, dplyr::n(), 2)) |>
  #   dplyr::rename(xend = x, yend = y)
  #
  # dplyr::bind_cols(starts, ends)

}


segments_to_paths <- function(df) {
  df |>
    dplyr::filter(!is.na(x) & !is.na(xend)) |>
    # dplyr::mutate(group = 1:dplyr::n()) |>
    tibble::rowid_to_column("row") |>
    dplyr::mutate(x_ =    ifelse(row %% 2 != 0, x,    xend),
                  xend_ = ifelse(row %% 2 != 0, xend, x),
                  y_ =    ifelse(row %% 2 != 0, y,    yend),
                  yend_ = ifelse(row %% 2 != 0, yend, y)) |>
    dplyr::rowwise() |>
    dplyr::mutate(x = list(c(x_, xend_)), y = list(c(y_,yend_))) |>
    tidyr::unnest(cols = c(x, y))
  # |>
  #   dplyr::select(x, y, group = row)
}


rotate <- function(df, angle, around = c(0, 0)) {

  # w <- (max(df$x) - min(df$x))
  # h <-( max(df$y) - min(df$y))
  # x_center <- min(df$x) + (max(df$x) - min(df$x))/2
  # y_center <- min(df$y) + (max(df$y) - min(df$y))/2

  dplyr::mutate(df,
                x0 = x - around[1],
                y0 = y - around[2],
                x = x0 * cos(angle) - y0 * sin(angle) + around[1],
                y = y0 * cos(angle) + x0 * sin(angle) + around[2]) |>
    dplyr::select(-x0, -y0) |>
    dplyr::mutate(x = round(x, 3),
                  y = round(y, 3))
}

floor_spacing <- function(x, spacing) {
  floor(x*(1/spacing))/(1/spacing)
}

ceiling_spacing <- function(x, spacing) {
  ceiling(x*(1/spacing))/(1/spacing)
}

degrees_to_radians <- function(d) {
  d * pi/180
}
