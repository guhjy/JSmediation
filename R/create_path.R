# Create an object of class "mediation_model" and performs type check so
# that every subcomponent is the class it is suppose to be.
# Args:
#   type:
#   params: List of params involved in the model
#   paths: Paths involved in the model (list)
#   indirect_index: Has the indirect index been compute ? (boolean)
# Returns:
#   An object of class "mediation_model".

create_path <- function(model_list, model_name, term_name) {
  list(
    point_estimate =
      model_list %>%
        purrr::pluck(model_name) %>%
        broom::tidy() %>%
        dplyr::filter(term == term_name) %>%
        dplyr::pull(estimate),
    se =
      model_list %>%
        purrr::pluck(model_name) %>%
        broom::tidy() %>%
        dplyr::filter(term == term_name) %>%
        dplyr::pull(std.error),
    APA =
      model_list %>%
        purrr::pluck(model_name) %>%
        apastylr(term_name)
  )
}
