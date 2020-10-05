context('Defining marker colours')

test_that("standard colours", {
  expect_equal(get_colours("CYTOSOL"), "#E41A1C")
})

test_that("Undefined marker colour", {
  expect_error(get_colours("new_marker_class_1"))
})

test_that("User-defined colour scheme", {
  udc <- list('a'='black', 'b'='white')
  expect_equal(get_colours("b", class_2_colour=udc), "white")
})

