context("test-mdt_within2.R")

test_that("mdt_within2 does not throw an error when used as intended", {
  expect_silent(
    mdt_within2(dohle_siegrist2,
                willingness_c,
                willingness_s,
                hazardousness_c,
                hazardousness_s)
  )
})
