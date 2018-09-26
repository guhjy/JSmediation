context("test-apastylr")

test_that("Does not throw an error", {
  data(ho_et_al)
  test <- lm(hypodescent ~ linkedfate, ho_et_al)
  expect_silent(apastylr(test, "linkedfate"))
})

test_that("Correctly reports p < .001", {
  data(ho_et_al)
  test <- lm(hypodescent ~ linkedfate, ho_et_al)
  expect_output(print(apastylr(test, "linkedfate")), "< .001")
})
