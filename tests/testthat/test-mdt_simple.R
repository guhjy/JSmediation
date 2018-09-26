context("test-mdt_simple")

test_that("does not throw an error", {
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  expect_silent(
    mdt_simple(data = ho_et_al,
               IV   = condition_c,
               DV   = hypodescent,
               M    = linkedfate)
  )
})
