context("test-mdt_within_w.R")

test_that("mdt_within_w does not throw an error when used as intended", {
  expect_silent(
    mdt_within_w(dohle_siegrist_w,
                willingness_c,
                willingness_s,
                hazardousness_c,
                hazardousness_s)
  )
})
