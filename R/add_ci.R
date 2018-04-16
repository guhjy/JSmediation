#' Add index for the indirect effect to a fitted mediation model
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#'   method.
#' @param ...   Further argument to pass to the different methods.
#' @param iter  Number of iteration for the Monte Carlo CI method.
#' @param alpha Alpha threshold to use when computing Monte Carlo CI.
#' @param stage Stage on which you want do c
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
add_index <- function(mediation_model, ...) {
  UseMethod("add_index")
}

#' @export
add_index.mediation_model <- function(mediation_model, iter = 5000, alpha = .05, stage = NULL) {

  model_type <- mediation_model$type

  supported_models <-
    c("simple mediation", "moderated mediation")

  if(!(model_type %in% supported_models))
    stop(glue::glue("Error:\n {model_type} model is not supported."))

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

    indirect_index_infos <-
      list(type          = "Indirect effect",
           method        = "Monte Carlo",
           estimate      = a * b,
           CI            = CI,
           alpha         = alpha,
           iterations    = iter,
           contains_zero = contains_zero,
           sampling      = indirect_sampling)

    mediation_model$indirect_index <- TRUE
    mediation_model$indirect_index_infos <-
      as_indirect_index(indirect_index_infos)

  }

  if(model_type == "moderated mediation")
  {
    if(is.null(stage))
      stop(
        "Warning:\n You have to explicite the stage on which you want to compute the moderated mediation index with the stage argument."
        )

    models_summary <- mediation_model$js_models_summary

    model1 <- models_summary[["X * Mod -> M"]]
    model2 <- models_summary[["(X + M) * Mod -> Y"]]

    IV    <- mediation_model$model$IV
    M     <- mediation_model$model$M
    Mod   <- mediation_model$model$Mod
    IVMod <- glue::glue("{IV}:{Mod}")
    MMod  <- glue::glue("{M}:{Mod}")


    b_51_e  <- model1[model1$term == IV, "estimate"]
    b_51_se <- model1[model1$term == IV, "std.error"]
    b_53_e  <- model1[model1$term == IVMod, "estimate"]
    b_53_se <- model1[model1$term == IVMod, "std.error"]

    b_64_e  <- model2[model2$term == M, "estimate"]
    b_64_se <- model2[model2$term == M, "std.error"]
    b_65_e  <- model2[model2$term == MMod, "estimate"]
    b_65_se <- model2[model2$term == MMod, "std.error"]

    if(stage == 1) {
      a   <- b_53_e
      sea <- b_53_se
      b   <- b_64_e
      seb <- b_64_se
    } else if(stage == 2) {
      a   <- b_51_e
      sea <- b_51_se
      b   <- b_65_e
      seb <- b_65_se
    } else {
      stop("Warning:\nagument stage must be either 1 or 2.")
    }

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

    indirect_index_infos <-
      list(method            = "Monte Carlo",
           CI                = CI,
           alpha             = alpha,
           stage             = stage,
           iterations        = iter,
           contains_zero     = contains_zero,
           indirect_sampling = indirect_sampling)

    mediation_model$indirect_inex <- TRUE
    mediation_model$indirect_index_infos <-
      as_indirect_index(indirect_index_infos)

  }

  mediation_model
}
