#' @export
add_index.moderated_mediation <- function(mediation_model, iter = 5000, alpha = .05, stage = NULL, ...) {

  if(is.null(stage))
    stop(
      "Warning:\n You have to explicite the stage on which you want to compute the moderated mediation index with the stage argument."
    )

  stage <- as.character(stage)


  if(stage %in% c("1", "first", "2", "second")) {
    if(stage %in% c("1", "first")) {
      a   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
      sea <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
      b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
      seb <- purrr::pluck(mediation_model, "paths", "b", "se")

      type <- "Mediated moderation index (First stage)"
    } else if(stage %in% c("2", "second")) {
      a   <- purrr::pluck(mediaction_model, "paths", "a", "point_estimate")
      sea <- purrr::pluck(mediation_model,  "paths", "a", "se")
      b   <- purrr::pluck(mediation_model,  "paths", "b * Mod", "point_estimate")
      seb <- purrr::pluck(mediation_model,  "paths", "b * Mod", "se")

      type <- "Mediated moderation index (Second stage)"
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
  }
  else if(stage %in% c("both")) {

    a1   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
    sea1 <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
    b1   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
    seb1 <- purrr::pluck(mediation_model, "paths", "b", "se")

    a2   <- purrr::pluck(mediaction_model, "paths", "a", "point_estimate")
    sea2 <- purrr::pluck(mediation_model,  "paths", "a", "se")
    b2   <- purrr::pluck(mediation_model,  "paths", "b * Mod", "point_estimate")
    seb2 <- purrr::pluck(mediation_model,  "paths", "b * Mod", "se")

    type <- "Mediated moderation index (Both stages)"

    ab_sampling <-
      MASS::mvrnorm(n  = iter,
                    mu = c(a1, b1, a2, b2),
                    Sigma =
                      matrix(
                        c(sea1^2,      0,      0,      0,
                          0, seb1^2,      0,      0,
                          0,      0, sea2^2,      0,
                          0,      0,      0, seb2^2),
                        nrow = 2
                      ))

    indirect_sampling <- ab_sampling[ , 1] * ab_sampling[ , 2]
    CI <- stats::quantile(indirect_sampling, c(alpha / 2, 1 - alpha / 2))
    contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)
  }

  indirect_index_infos <-
    list(type          = type,
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

  mediation_model
}
