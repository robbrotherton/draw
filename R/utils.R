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

# purrr::map_df(1:16, ~square(), .id = "group") |>
#   arrange_grid(4, 4, 1.1) |>
#   dplyr::mutate(group = as.numeric(group)) |>
#   dplyr::group_split(group) |>
#   purrr::map2_df(.y = runif(16, max = pi), ~hatch(.x, angle = .y), .id = "unit") |>
#   dplyr::group_by(unit, group) |>
#   dplyr::mutate(group = dplyr::cur_group_id()) |>
#   show()
#
# square() |>
#   hatch(angle = pi/4) |>
#   show()
