
<!-- README.md is generated from README.Rmd. Please edit that file -->

# draw

<!-- badges: start -->
<!-- badges: end -->

A package to generate cartesian coordinates for various polygons, and to
fill polygons with lines.

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

circle() |> show()
```

<img src="man/figures/README-shapes-1.png" width="50%" />

``` r
square() |> show()
```

<img src="man/figures/README-shapes-2.png" width="50%" />

``` r
star() |> show()
```

<img src="man/figures/README-shapes-3.png" width="50%" />

``` r
polygon(sides = 6) |> show()
```

<img src="man/figures/README-shapes-4.png" width="50%" />

``` r
rectangle(width = 1.5, height = 1) |> show()
```

<img src="man/figures/README-shapes-5.png" width="50%" />

``` r
heart() |> show()
```

<img src="man/figures/README-shapes-6.png" width="50%" />

## Fill shapes

``` r
circle() |> fill_hatch() |> show()
```

<img src="man/figures/README-fills-1.png" width="50%" />

``` r
square() |> fill_hatch(angle = c(pi*.25, pi*.75)) |> show()
```

<img src="man/figures/README-fills-2.png" width="50%" />

``` r
polygon(8) |> fill_wave() |> show()
```

<img src="man/figures/README-fills-3.png" width="50%" />

``` r
heart() |> fill_zigzag() |> show()
```

<img src="man/figures/README-fills-4.png" width="50%" />

``` r
star() |> fill_inset() |> show()
```

<img src="man/figures/README-fills-5.png" width="50%" />

## Fill many shapes

``` r
set.seed(1)

n <- 15

input <- data.frame(n = 1:n, 
                    radius = rnorm(n, mean = 10, sd = 2),
                    angle = runif(n, min = 0, max = 2*pi),
                    offset_x = runif(n, min = 0, max = 120),
                    offset_y = runif(n, min = 0, max = 200))

stars <- purrr::pmap(input, ~star(radius = ..2, angle = ..3) |> 
                             dplyr::mutate(x = x + ..4, y = y + ..5)) |>
  purrr::map2(.y = input$angle, .f = ~fill_hatch(.x, spacing = 1, angle = .y)) |> 
  dplyr::bind_rows(.id = "star") |> 
  dplyr::group_by(star, group) |> 
  dplyr::mutate(group = dplyr::cur_group_id())

show(stars, void = TRUE)
```

<img src="man/figures/README-stars-1.png" width="100%" />

``` r
dim <- c(12, 17)
n <- prod(dim)
angles <- sample(c(0, pi*.5, pi*.25, pi*.75), n, replace = TRUE)

square_grid <- shape_grid(square(), dim) |> 
  dplyr::group_by(group) |> 
  dplyr::group_split() |> 
  purrr::map2_df(.y = angles, .f = ~fill_hatch(.x, angle = .y), .id = "shape") |> 
  dplyr::group_by(shape, group) |> 
  dplyr::mutate(group = dplyr::cur_group_id())
#> Joining, by = "group"

show(square_grid, void = TRUE)
```

<img src="man/figures/README-square-grid-1.png" width="100%" />

``` r

n <- 9^2
angles <- runif(n, 0, pi)

circle_grid <- shape_grid(circle(), n = n) |> 
  dplyr::group_by(group) |> 
  dplyr::group_split() |> 
  purrr::map2_df(.y = angles, .f = ~fill_hatch(.x, angle = .y), .id = "shape") |> 
  dplyr::group_by(shape, group) |> 
  dplyr::mutate(group = dplyr::cur_group_id())
#> Joining, by = "group"

show(circle_grid, void = TRUE)
```

<img src="man/figures/README-circle-grid-1.png" width="100%" />
