#' Title
#'
#' @param points
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
circle <- function(points = 64, radius = 1) {

  theta <- seq(0,2*pi, length.out = points)

  data.frame(x = cos(theta) * radius,
             y = sin(theta) * radius)

}


#' Title
#'
#' @param side
#'
#' @return
#' @export
#'
#' @examples
square <- function(side = 1) {

  lim <- side/2

  data.frame(x = c(lim, -lim, -lim, lim, lim),
             y = c(lim, lim, -lim, -lim, lim))

}

#' Title
#'
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
rectangle <- function(width = 1, height = 1.5) {

  xlim <- width/2
  ylim <- height/2

  data.frame(x = c(xlim, -xlim, -xlim, xlim, xlim),
             y = c(ylim, ylim, -ylim, -ylim, ylim))

}

#' Make an n-sided regular polygon
#'
#' @param sides
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
polygon <- function(sides = 4, radius = 1) {

  theta <- seq(0,2*pi, length.out = sides + 1)

  rotation <- pi * 1/sides

  data.frame(x = cos(theta + rotation) * radius,
             y = sin(theta + rotation) * radius)

}


#' Make a Heart
#'
#' @param points
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
#'
#'
heart <- function(points = 64, radius = 1) {

  t <- seq(0,2*pi, length.out = points)+pi*1.5
  rad <- numeric(points)

  for(i in 1:points) {
    rad[i] <- sin(t[i])*sqrt(abs(cos(t[i]))) / (sin(t[i]) + 7/5) - 2*sin(t[i]) + 2
  }

  data.frame(x = cos(t) * rad * radius,
             y = sin(t) * rad * radius)

}


# see https://math.stackexchange.com/questions/4293250/how-to-write-a-polar-equation-for-a-five-pointed-star
star <- function(points = 5, radius = 1, m = 3, k = 1) {

  t <-  seq(0, 2 * pi, length.out = 2 * points + 1)
  nom <-  cos((2 * asin(k) + pi * m) / (2 * points))
  denom <-  cos((2 * asin(k * cos(points * t)) + pi * m) / (2 * points))
  r <-  (nom / denom)

  data.frame(x = cos(t) * r * radius,
             y = sin(t) * r * radius)

}
