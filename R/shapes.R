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

letter <- function(letter, nseg = 4, family = "sans", face = "regular") {

  # devtools::install_github("https://github.com/yixuan/fontr")
  # sysfonts::font_families() gf <-
  # sysfonts::font_families_google()

  fontr::glyph_polygon(letter, nseg = 5, family = family, face = face) %>%
    dplyr::mutate(group = ifelse(is.na(x), 1, 0),
                  group = cumsum(group)) %>%
    tidyr::drop_na()

  # some letters (like "a") have an inset polygon to make the hole
  # some letters (like "B") even have two inset polygons
  # some letters (like "i") have two separate polygons which should both be filled

  # This all works fine with the hatch() function; inset polys are excluded,
  # because a hatch segment must cross into the letter, out into the inset, back
  # into the letter, then out again.

}

letters <- function(string, nseg = 4, kerning = 0, family = "sans", face = "regular") {

  letters <- strsplit(string, "")[[1]]
  letter_dfs <- vector(mode = "list", length = length(letters))
  prev_max_x <- 0

  for(i in seq_along(letters)) {

    if(letters[i]==" ") {
      prev_max_x = prev_max_x + .2
    } else {
      new_letter <- fontr::glyph_polygon(letters[i], family = family, face = face, nseg = nseg) %>%
        dplyr::mutate(x = x + prev_max_x + kerning,
                      y = y)

      # need the group thing in mutate

      prev_max_x <- max(new_letter$x, na.rm = T)

      letter_dfs[[i]] <- new_letter
    }

  }

  dplyr::bind_rows(letter_dfs, .id = "letter") %>%
    dplyr::mutate(new_group = cumsum(is.na(x))) %>%
    dplyr::group_by(letter, new_group) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(group = dplyr::cur_group_id())
  # %>%
    # tidyr::drop_na()

}

# l <- letters("hatch", nseg = 5)
