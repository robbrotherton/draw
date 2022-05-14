#' Title
#'
#' @param df
#' @param geom
#'
#' @return
#' @export
#'
#' @examples
#' square() |> show(group = NULL)
show <- function(df, geom = c("path", "point", "polygon", "line"), group = group, fixed = TRUE, void = FALSE, ...) {

  geom <- match.arg(geom)

  geom <- switch(geom,
                 point   = ggplot2::geom_point(...),
                 path    = ggplot2::geom_path(...),
                 polygon = ggplot2::geom_polygon(...),
                 line    = ggplot2::geom_line(...))

  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, group = {{ group }})) +
    geom +
    ggplot2::labs(x = NULL, y = NULL)

  if (fixed) plot <- plot + ggplot2::coord_fixed()
  if (void)  plot <- plot + ggplot2::theme_void()

  plot

}

# Need to add functionality for groups
