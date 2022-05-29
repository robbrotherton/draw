
test_that("points are correctly labeled as being inside or outside polygons", {

  # Points outside
  expect_false(point_in_polygon(1, 1, square(1)))
  expect_false(point_in_polygon(-.3, -.4, star(5, .5)))

  # Points inside
  expect_true(point_in_polygon(0, 0, square(1)))
  expect_true(point_in_polygon(.3, .3, square()))
  expect_true(point_in_polygon(0, 0, star(5, .5)))

  # Point on a side
  expect_true(point_in_polygon(.5, 0, square()))

  # Point on a corner
  expect_true(point_in_polygon(.707, 0, square() |> rotate(pi/4)))

  # For some reason this was causing a problem at one point...
  expect_false(point_in_polygon(-0.6910000000, -0.308000000, star(9)))



  test_points <- data.frame(x = 1:5, y = 1:5)
  test_polygons <- dplyr::bind_rows(.id = "group",
                                    square() + 2,
                                    square() + 4)

  expect_identical(points_in_polygons(test_points, test_polygons),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE))


  # what about points exactly on an edge? Or on a corner--so two edges??

})

