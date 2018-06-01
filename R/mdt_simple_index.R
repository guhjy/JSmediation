#' @export
add_index.simple_mediation <- function(mediation_model, iter = 5000, alpha = .05, ...) {

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
    structure(
      list(
        type          = "Indirect effect",
        method        = "Monte Carlo",
        estimate      = a * b,
        CI            = CI,
        alpha         = alpha,
        iterations    = iter,
        contains_zero = contains_zero,
        sampling      = indirect_sampling
      ),
      class = "indirect_index"
    )

  mediation_model$indirect_index <- TRUE

  mediation_model
}
