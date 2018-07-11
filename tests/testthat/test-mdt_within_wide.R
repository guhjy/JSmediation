context("test-mdt_within_wide.R")

test_that("mdt_within_wide does not throw an error when used as intended", {
  expect_silent(
    mdt_within_wide(dohle_siegrist_wide,
                    willingness_c,
                    willingness_s,
                    hazardousness_c,
                    hazardousness_s)
  )
})
