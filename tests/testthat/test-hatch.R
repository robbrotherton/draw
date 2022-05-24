test_that("hatch function works for a square", {

  # no rotation
  test_df <- fill_hatch(square(side = 1), spacing = .1, keep_outline = FALSE)

  expect_equal(test_df$x,
               c(rep(c(-.5, .5, .5, -.5), 4), -.5, .5))
  expect_equal(test_df$y,
               rep(seq(-.4, .4, .1), each = 2))
  expect_equal(test_df$group,
               rep(1:9, each = 2))

  # rotated 90 degrees
  test_df <- fill_hatch(square(side = 1), spacing = .1, angle = pi*.5, keep_outline = FALSE)

  expect_equal(test_df$x,
               rep(seq(.4, -.4, -.1), each = 2))
  expect_equal(test_df$y,
               c(rep(c(.5, -.5, -.5, .5), 4), .5, -.5))
  expect_equal(test_df$group,
               rep(1:9, each = 2))

  # rotated 45 degrees
  test_df <- fill_hatch(square(side = 1), spacing = .1, angle = pi*.25, keep_outline = FALSE)

  expect_equal(tolerance = .01,
               test_df$x,
               c(0.49, 0.5, 0.5, 0.35, 0.21, 0.5, 0.5, 0.07, -0.08, 0.5, 0.5, -0.22, 0.5, -0.36, -0.5, 0.5, 0.36, -0.5, -0.5, 0.22, 0.08, -0.5, -0.5, -0.07, -0.21, -0.5, -0.5, -0.35, -0.49, -0.5))
  expect_equal(tolerance = .01,
               test_df$y,
               c(-0.5, -0.49, -0.35, -0.5, -0.5, -0.21, -0.07, -0.5, -0.5, 0.08, 0.22, -0.5, 0.36, -0.5, -0.5, 0.5, 0.5, -0.36, -0.22, 0.5, 0.5, -0.08, 0.07, 0.5, 0.5, 0.21, 0.35, 0.5, 0.5, 0.49))
  expect_equal(test_df$group,
               rep(1:15, each = 2))
})
