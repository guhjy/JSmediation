#' Add confidence interval for the indirect effect to a fitted mediation model
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#'   method.
#' @param ...   Further argument to pass to the different methods.
#' @param iter  Number of iteration for the Monte Carlo CI method.
#' @param alpha Alpha threshold to use when computing Monte Carlo CI.
#'
#' @return An object of the same class as \code{mediation_model}, but with
#'   confidence interval for the indirect effect estimation added for later
#'   use.
#'
#' @examples
#' \dontrun{
#' mediation_model <- add_ci(mediation_model)
#' }
#' @export
add_ci <- function(mediation_model, ...) {
  UseMethod("add_ci")
}

#' @export
add_ci.mediation_model <- function(mediation_model, iter = 5000, alpha = .05) {

  model_type <- mediation_model$type

  supported_models <-
    c("simple mediation")

  if(!(model_type %in% supported_models))
    warning(glue::glue("Error:\n {model_type} model is not supported."))

  if(model_type == "simple mediation")
  {
    models_summary <- mediation_model$js_models_summary

    model1 <- models_summary[["X -> M"]]
    model2 <- models_summary[["X + M -> Y"]]

    IV <- mediation_model$model$IV
    M  <- mediation_model$model$M

    a   <- model1[model1$term == IV, "estimate"]
    sea <- model1[model1$term == IV, "std.error"]
    b   <- model2[model2$term == M,   "estimate"]
    seb <- model2[model2$term == M,  "std.error"]

    ab_sampling <-
      MASS::mvrnorm(n  = iter,
                    mu = c(a, b),
                    Sigma =
                      matrix(
                        c(sea^2, 0,
                          0, seb^2),
                        nrow = 2
                      ))

    indirect_sampling <- ab_sampling[ , 1] * ab_sampling[ , 2]
    CI <- stats::quantile(indirect_sampling, c(alpha / 2, 1 - alpha / 2))
    contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)

    mediation_model$CI <- TRUE
    mediation_model$CI_infos <-
      as_indirect_CI(list(method = "Monte Carlo",
                          CI     = CI,
                          contains_zero     =
                            contains_zero,
                          indirect_sampling =
                            indirect_sampling))

  }

  mediation_model
}
