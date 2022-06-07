test_that("fill_hatch function works for a square", {

  # no rotation
  test_df <- fill_hatch(square(side = 1), spacing = .1, keep_outline = FALSE)

  expect_equal(test_df$x,
               rep(c(-.5, .5), 11))
  expect_equal(test_df$y,
               rep(seq(-.5, .5, .1), each = 2))
  expect_equal(test_df$group,
               rep(1:11, each = 2))

  # rotated 90 degrees
  test_df <- fill_hatch(square(side = 1), spacing = .1, angle = pi*.5, keep_outline = FALSE)

  expect_equal(test_df$x,
               rep(seq(.5, -.5, -.1), each = 2))
  expect_equal(test_df$y,
               rep(c(-.5, .5), 11))
  expect_equal(test_df$group,
               rep(1:11, each = 2))

  # rotated 45 degrees
  test_df <- fill_hatch(square(side = 1), spacing = .1, angle = pi*.25, keep_outline = FALSE)

  expect_equal(tolerance = .01,
               test_df$x,
               c(0.49, 0.5, 0.349, 0.5, 0.207, 0.5, 0.066, 0.5, -0.076, 0.5, -0.217, 0.5, -0.359, 0.5, -0.5, 0.5, -0.5, 0.359, -0.5, 0.217, -0.5, 0.076, -0.5, -0.066, -0.5, -0.207, -0.5, -0.349, -0.5, -0.49))
  expect_equal(tolerance = .01,
               test_df$y,
               c(-0.5, -0.49, -0.5, -0.349, -0.5, -0.207, -0.5, -0.066, -0.5, 0.076, -0.5, 0.217, -0.5, 0.359, -0.5, 0.5, -0.359, 0.5, -0.217, 0.5, -0.076, 0.5, 0.066, 0.5, 0.207, 0.5, 0.349, 0.5, 0.49, 0.5))
  expect_equal(test_df$group,
               rep(1:15, each = 2))


  test_df <- fill_hatch(square(side = 1), angle = c(0, pi*.5), spacing = .1, keep_outline = FALSE)
  expect_equal(test_df$group,
               rep(1:22, each = 2))

  # star
  test_df <- fill_hatch(star(radius = .5), spacing = .1, keep_outline = FALSE)
  expect_equal(tolerance = .01,
               test_df$x,
               c(-0.2770058, -0.2218738, 0.2218738, 0.2770058, -0.2446358, -0.08449065, 0.08449065, 0.2446358, -0.2122659, 0.2122659, -0.1909299, 0.1909299, -0.3283131, 0.3283131, -0.4656963, 0.4656963, -0.08197101, 0.08197101, -0.04950725, 0.04950725, -0.01704348, 0.01704348))
  expect_equal(tolerance = .01,
               test_df$y,
               c(-0.3525, -0.3525, -0.3525, -0.3525, -0.2525, -0.2525, -0.2525, -0.2525, -0.1525, -0.1525, -0.0525, -0.0525, 0.0475, 0.0475, 0.1475, 0.1475, 0.2475, 0.2475, 0.3475, 0.3475, 0.4475, 0.4475))
  expect_equal(test_df$group,
               rep(1:11, each = 2))
})
