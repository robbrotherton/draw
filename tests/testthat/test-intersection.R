test_that("Rcpp line intersection function returns expected intersection or NA", {


  expect_equal(line_intersection(c(0, 0), c(10, 10),
                                 c(5, 0), c(5, 10)),
               data.frame(x = 5, y = 5))


  # parallel, same y

  P1 <- c(0.0, -0.653)
  P2 <- c(0.0, 0.747)

  P3 <- c(-0.294, -0.405) # point 7 of the star
  P4 <- c(0.000, -0.191) # point 8 of the star

  expect_equal(line_intersection(P1, P2, P3, P4),
               data.frame(x = 0, y = -.191))

  # hatch segment
  P1 <- c(-1, .5)
  P2 <- c( 1, .5)

  # polygon segment
  P3 <- c(-.5, .5)
  P4 <- c( .5, .5)

  expect_equal(line_intersection(P1, P2, P3, P4),
               data.frame(x = NA_real_, y = NA_real_))

  # intersection at a single point

  # polygon segment
  P3 <- c(-.5, .5)
  P4 <- c(-.5,-.5)

  expect_equal(line_intersection(P1, P2, P3, P4, include_lineend = FALSE),
               data.frame(x = NA_real_, y = NA_real_))

  P3 <- c(.5, .5)
  P4 <- c(.5,-.5)

  expect_equal(line_intersection(P1, P2, P3, P4, include_lineend = FALSE),
               data.frame(x = NA_real_, y = NA_real_))

  })
