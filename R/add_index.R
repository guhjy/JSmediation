#' @title Add mediation index to a fitted mediation model
#'
#' @description Add mediation index to a fitted mediation model.
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#' method.
#' @param iter number of iteration for the Monte Carlo CI method.
#' @param alpha Alpha threshold to use when computing Monte Carlo CI.
#' @param stage Stage on which you want to compute conditional index for
#' moderated mediation models.
#'
#' @return An object of the same class as \code{mediation_model}, but with
#' confidence interval for the indirect effect estimation added for later
#' use.
#'
#' @examples
#' \dontrun{
#' my_model <- add_ci(my_model)
#' }
#' @export
add_index <- function(mediation_model, iter = 5000, alpha = .05, stage = NULL) {
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
    a   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
    sea <- purrr::pluck(mediation_model, "paths", "a", "se")
    b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
    seb <- purrr::pluck(mediation_model, "paths", "b", "se")

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

    if(stage == 1) {
      a   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
      sea <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
      b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
      seb <- purrr::pluck(mediation_model, "paths", "b", "se")
    } else if(stage == 2) {
      a   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
      sea <- purrr::pluck(mediation_model, "paths", "a", "se")
      b   <- purrr::pluck(mediation_model, "paths", "b * Mod", "point_estimate")
      seb <- purrr::pluck(mediation_model, "paths", "b * Mod", "se")
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
      list(type          = glue::glue("Conditional index (Stage {stage})"),
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

  mediation_model
}
