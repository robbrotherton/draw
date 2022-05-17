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
#
# purrr::map_df(1:16, ~square(), .id = "group") |>
#   arrange_grid(4, 4, 1.1) |>
#   dplyr::mutate(group = as.numeric(group)) |>
#   dplyr::group_split(group) |>
#   purrr::map2_df(.y = runif(16, max = pi), ~hatch(.x, angle = .y), .id = "unit") |>
#   dplyr::group_by(unit, group) |>
#   dplyr::mutate(group = dplyr::cur_group_id()) |>
#   show()
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

# polygon(5) |> hatch(spacing = .1, angle = 0, keep = TRUE, single_line = TRUE)
