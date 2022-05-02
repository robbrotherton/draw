
test_that("points are correctly labeled as being inside or outside polygons", {

  test_points <- dplyr::tribble(
    ~x, ~y,
     1,  1,
     2,  2,
     3,  3,
     4,  4,
     5,  5
  )

  test_polygons <- dplyr::tribble(
    ~x,   ~y, ~group,
    2.5, 2.5, 1,
    1.5, 2.5, 1,
    1.5, 1.5, 1,
    2.5, 1.5, 1,
    2.5, 2.5, 1,
    4.5, 4.5, 2,
    3.5, 4.5, 2,
    3.5, 3.5, 2,
    4.5, 3.5, 2,
    4.5, 4.5, 2
  )

  # test_polygons <- dplyr::bind_rows(.id = "group",
  #                                   square() + 2,
  #                                   square() + 4)

  expect_identical(points_in_polygons(test_points, test_polygons),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE))

  # ggplot() +
  #   geom_path(data = test_polygons, aes(x, y, group = group)) +
  #   geom_point(dat = test_points, aes(x, y, color = inside)) +
  #   coord_fixed()

  # what about points exactly on an edge? Or on a corner--so two edges??

})

