test_that("Rcpp line intersection function returns expected intersection,
           or NA if lines don't intersect", {


  expect_equal(lineLineIntersection(c(0, 0), c(10, 10),
                                    c(5, 0), c(5, 10)),
               data.frame(x = 5, y = 5))

  # parallel, same y

  # hatch segment
  P1 <- c(-1, .5)
  P2 <- c( 1, .5)

  # polygon segment
  P3 <- c(-.5, .5)
  P4 <- c( .5, .5)

  expect_equal(lineLineIntersection(P1, P2, P3, P4),
               data.frame(x = NA_real_, y = NA_real_))

  # intersection at a single point

  # polygon segment
  P3 <- c(-.5, .5)
  P4 <- c(-.5,-.5)

  expect_equal(lineLineIntersection(P1, P2, P3, P4),
               data.frame(x = NA_real_, y = NA_real_))

  P3 <- c(.5, .5)
  P4 <- c(.5,-.5)

  expect_equal(lineLineIntersection(P1, P2, P3, P4),
               data.frame(x = NA_real_, y = NA_real_))

  })
