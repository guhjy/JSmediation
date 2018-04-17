#' @title Extract models from a mediation object
#'
#' @description When conducting a joint significant test, different models are
#' fitted to the data. This function helps you access the models that have been
#' used in an object of class \code{mediation_model}.
#'
#' @seealso \code{\link{extract_models}} to access to models used for joint
#' significant test as a data frame.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return A list of \code{lm} objects.
#'
#' @examples
#' \dontrun{
#' extract_models(my_model)
#' }
#'
#' @export

extract_models <- function(mediation_model) {
  UseMethod("extract_models")
}

#' @export

extract_models.mediation_model <- function(mediation_model) {
  purrr::pluck(mediation_model, "js_models")
}

#' @title Extract models from a mediation object as a data frame
#'
#' @description When conducting a joint significant test, different models are
#' fitted to the data. This function helps you access the models that have been
#' used in an object of class \code{mediation_model}.
#'
#' @seealso \code{\link{extract_models}} to access to models used for joint
#' significant test as a list of object of class \code{lm}.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return A data frame with models information.
#'
#' @export

extract_tidy_models <- function(mediation_model) {
  UseMethod("extract_tidy_models")
}

#' @export

extract_tidy_models.mediation_model <- function(mediation_model) {
  purrr::pluck(mediation_model, "js_models") %>%
    purrr::map_df(~broom::tidy(.x), .id = "model")

}
