#' Arrange shapes on a grid
#'
#' @param shape_fun A shape function used to create the shape to be tiled
#' @param dim The dimensions (width and height) of the grid. One (or both) of
#'   dim or n must be specified.
#' @param n The number of shapes to be placed on a grid. If dim is not
#'   specified, the dimensions will be automatically determined from n.
#' @param spacing The spacing between the centers of shapes on the grid.
#'
#' @return A dataframe of x and y coordinates for the paths of each shape, with a group column to identify unique shapes.
#' @export
#'
#' @examples shape_grid(circle(), n = 9) |> show()
shape_grid <- function(shape_fun, dim = NULL, n = NULL, spacing = 1) {

  if (is.null(dim)) {
    dim <- rep(ceiling(sqrt(n)), 2)
  }

  if (is.null(n)) {
    n <- prod(dim)
  }

  offsets <- data.frame(group = as.character(1:n),
                        x_offset = rep(0:(dim[1] - 1), length.out = n) * spacing,
                        y_offset = rep((dim[2] - 1):0, each = dim[1])[1:n] * spacing)

  shapes <- purrr::map_df(1:n, ~shape_fun, .id = "group")

  dplyr::left_join(shapes, offsets) |>
    dplyr::transmute(x = x + x_offset, y = y + y_offset,
                     group = as.numeric(group))

}


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
