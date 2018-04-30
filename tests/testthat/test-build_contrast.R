context("test-build_contrast.R")

test_that("returns double", {
  vector <- c("a", "b", "a", "a")

  contrasts <- build_contrast(vector, "a", "b")

  expect_type(contrasts, "double")
})

test_that("returns two codes only", {
  vector <- c("a", "b", "a", "a")

  contrasts <- build_contrast(vector, "a", "b")
  expect_length(unique(contrasts), 2L)
})
