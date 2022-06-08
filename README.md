
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

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
