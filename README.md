
<!-- README.md is generated from README.Rmd. Please edit that file -->

# draw

<!-- badges: start -->
<!-- badges: end -->

A package to generate gcode for a pen plotter. It can be used to create
data.frames of x and y coordinates for shapes and lines, and to convert
the data into gcode.

## Installation

You can install the development version of draw from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robbrotherton/draw")
```

## Make shapes

``` r
library(draw)
#> 
#> Attaching package: 'draw'
#> The following object is masked from 'package:graphics':
#> 
#>     polygon
#> The following object is masked from 'package:methods':
#> 
#>     show

offsets <- data.frame(group = as.character(1:6),
                      x_offset = rep(seq(1, length.out = 3, by = 1.5), 2),
                      y_offset = rep(c(1, 2.5), each = 3))

shapes <- dplyr::bind_rows(circle(),
                           square(),
                           star(),
                           polygon(sides = 6),
                           rectangle(width = 1.5, height = 1), 
                           heart(),
                           .id = "group") |> 
  dplyr::left_join(offsets) |>  
  dplyr::mutate(x = x + x_offset, 
                y = y + y_offset)
#> Joining, by = "group"

show(shapes, void = TRUE)
```

<img src="man/figures/README-shapes-1.png" width="100%" />

## Fill a shape

``` r
square() |> fill_hatch() |> show()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Fill many shapes

``` r
set.seed(1)

n <- 15

input <- data.frame(n = 1:n, 
                    r = rnorm(n, mean = 10, sd = 2),
                    a = runif(n, min = 0, max = 2*pi),
                    ox = runif(n, min = 0, max = 120),
                    oy = runif(n, min = 0, max = 200))

stars <- purrr::pmap(input, ~star(radius = ..2, angle = ..3) |> 
                             dplyr::mutate(x = x + ..4, y = y + ..5)) |>
  purrr::map2(.y = input$a, .f = ~fill_hatch(.x, spacing = 1, angle = .y)) |> 
  dplyr::bind_rows(.id = "star") |> 
  dplyr::group_by(star, group) |> 
  dplyr::mutate(group = dplyr::cur_group_id())

show(stars, void = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
