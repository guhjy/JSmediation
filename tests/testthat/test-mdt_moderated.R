context("test-mdt_moderated.R")

test_that("used models are correctly specified", {
  library(tidyverse)

  data <-
    ho_et_al %>%
    mutate(condition_c =
             build_contrast(condition,
                            "High discrimination",
                            "Low discrimination"))

  js_model <-
    data %>%
    mdt_moderated(condition_c, hypodescent, linkedfate, sdo)

  # models --------------------------------------------------------------------
  models <-
    list(formula(hypodescent ~ condition_c * sdo),
         formula(linkedfate ~ condition_c * sdo),
         formula(hypodescent ~ (condition_c + linkedfate) * sdo)) %>%
    map(~lm(.x, data))

  models_js <- pluck(js_model, "js_models")

  expect_equivalent(models_js, models)
})
