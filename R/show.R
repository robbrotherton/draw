#' Title
#'
#' @param df
#' @param geom
#'
#' @return
#' @export
#'
#' @examples
show <- function(df, geom = c("path", "point", "polygon", "line"), group = NULL, fixed = TRUE, ...) {

  geom <- match.arg(geom)

  geom <- switch(geom,
                 point   = ggplot2::geom_point(...),
                 path    = ggplot2::geom_path(...),
                 polygon = ggplot2::geom_polygon(...),
                 line    = ggplot2::geom_line(...))

  ggplot2::ggplot(df, ggplot2::aes(x, y, group = {{ group }})) +
    geom +
    ggplot2::labs(x = NULL, y = NULL) +
    if (fixed) ggplot2::coord_fixed()
    else NULL

}


# Need to add functionality for groups
