
# geom_cloud <- function(df) {
#
#   groups <- unique(as.numeric(df$group))
#   clouds <- sample(groups, length(groups))
#
#   geom_list <- vector(mode = "list", length = length(groups)*2)
#
#   for (i in seq_along(clouds)) {
#     j <- clouds[i]
#     data <- dplyr::filter(df, group == j)
#     geom_list[[i*2]] <- ggplot2::geom_polygon(data = data,
#                                               ggplot2::aes(x, y), fill = hsv(.6, .5, 1))
#     geom_list[[i*2+1]] <- ggplot2::geom_path(data = data,
#                                              ggplot2::aes(x, y),
#                                              color = 'white', size = 2)
#   }
#
#   geom_list
#
# }



cloud <- function(n_circles) {

  spirals <- vector(mode = "list", length = n_circles)
  spirals_rm <- vector(mode = "list", length = n_circles)
  polygons <- vector(mode = "list", length = n_circles)

  prev_r <- .5
  prev_x <- 0
  prev_y <- 0

  for (i in seq_len(n_circles)) {

    this_r <- prev_r # + rnorm(1, mean = 0, sd = .05)
    this_x <- prev_x + rnorm(1, mean = .5, sd = .1)
    this_y <- prev_y + rnorm(1, mean = 0, sd = .2)

    spirals[[i]] <- dplyr::mutate(spiral(coils = 5,
                                         points = 30,
                                         radius = this_r,
                                         inner_radius = 0),
                                  x = x + this_x,
                                  y = y + this_y)

    polygons[[i]] <- spirals[[i]][c(121:150, 121),]

    # if (i > 1) {
    #   spirals[[i]] <- dplyr::mutate(spirals[[i]],
    #                                 inside = points_in_polygon(spirals[[i]],
    #                                                            dplyr::bind_rows(circles[1:(i-1)], .id = "group") |>
    #                                                              dplyr::mutate(group = as.numeric(group))))
    # } else {
    #   spirals[[i]]$inside <- FALSE
    # }

    if (i > 1) {

      spirals[[i]] <- paths_to_segments(spirals[[i]] |> dplyr::mutate(group = 1)) |>
        clip_paths_complex_outside(dplyr::bind_rows(polygons[1:(i-1)], .id = "group") |>
                                     dplyr::mutate(group = as.numeric(group))) |>
        dplyr::bind_rows() |>
        dplyr::select(x, y, group = seg_id)

      # spirals[[i]] <- dplyr::anti_join(spirals[[i]], dplyr::select(spirals_rm[[i]], x, y))
    }

    prev_r <- this_r
    prev_x <- this_x
    prev_y <- this_y


  }

  # dplyr::bind_rows(spirals, .id = "group")
  dplyr::bind_rows(spirals, .id = "unit") |>
    dplyr::group_by(unit, group) |>
    dplyr::mutate(group = dplyr::cur_group_id())

}


# c <- cloud(3)
#
# show(c, void = TRUE)
#
#
#
# ggplot2::ggplot() +
#   geom_cloud(c) +
#   ggplot2::coord_fixed() +
#   ggplot2::theme_void() +
#   ggplot2::theme(panel.background = ggplot2::element_rect(fill = hsv(.6, .5, 1)),
#                  legend.position = 'none')

