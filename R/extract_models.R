#' Helper to extract models from a mediation object
#'
#' With joint significant approach, different models are fitted to the data.
#' This function helps you access the data that have been fit when using
#' \code{mediation_model()}.
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#'   method.
#'
#' @export

extract_models <- function(mediation_model) {
  UseMethod("extract_models")
}

#' @export

extract_models.mediation_model <- function(mediation_model) {
  mediation_model$js_models
}

#' Helper to extract tidy models from a mediation object
#'
#' With joint significant approach, different models are fitted to the data.
#' This function helps you access the models that have been fit when using
#' \code{mediation_model()}.
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#'   method.
#'
#' @return A data.frame with models information.
#'
#' @export

extract_tidy_models <- function(mediation_model) {
  UseMethod("extract_tidy_models")
}

#' @export

extract_tidy_models.mediation_model <- function(mediation_model) {
  mediation_model$js_models %>%
    purrr::map_df(~broom::tidy(.x), .id = "model")

}
