
points_in_polygons <- function(points, polygons) {

  res <- vector(mode = "logical", length = nrow(points))

  for (p in 1:nrow(points)) {

    x <- points$x[p]
    y <- points$y[p]

    # this is a problem when there's no group col; if it doesn't exist create it
    for (i in unique(polygons$group)) {

      poly <- dplyr::filter(polygons, group == i)

      max_x <- max(polygons$x) + 1
      intersections <- 0

      for (j in 1:(nrow(poly)-1)) {

        # Want a version of line.line.intersection that returns TRUE if there is an
        # intersection, FALSE if not. Then count the number of TRUEs. If it's
        # even, the point is outside the polygon; if it's odd, the point is
        # inside.

        intersections <- intersections + line.line.intersection2(c(x, y), c(max_x, y),
                                                                 c(poly$x[j], poly$y[j]), c(poly$x[j+1], poly$y[j+1]))

      }

      if(intersections %% 2 != 0) {
        res[[p]] <- TRUE
        break
      }

    }

    # res[p] <- FALSE

  }

  res

}
