context("test-space-bucket.R")


test_that("space bucket works", {
  expect_silent(sb <- polymer(B, C))
  sb %>% expect_s3_class("polymer")
  expect_equal(sort(names(sb)), sort(c("input", "primitives", "geometry_map", "index")))
  expect_equal(length(sb[["input"]]), 2L)
  expect_s3_class(sb$primitives, "triangulation") ## RTriangle native
  expect_s3_class(sb$input[[1]], "sf")
  expect_output(print(sb))
  expect_silent(plot(sb))
  expect_silent(sb_intersection(sb))
})
