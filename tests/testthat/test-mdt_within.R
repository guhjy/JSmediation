context("test-mdt_within.R")

test_that("mdt_within does not throw error", {
  expect_silent(mdt_within(dohle_siegrist,
                           name,
                           willingness,
                           hazardousness,
                           participant))
})
