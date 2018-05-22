is_contrast <- function(x) {
  length(unique(x)) == 2L & sum(unique(x)) == 0
}

is_centered <- function(x) {
  # implementation to deal with floating number rounding error
  isTRUE(all.equal(mean(x), 0))
}

create_path <- function(model_list, model_name, term_name) {
  list(point_estimate =
         model_list %>%
         purrr::pluck(model_name) %>%
         broom::tidy() %>%
         dplyr::filter(term == term_name) %>%
         dplyr::pull(estimate),
       se  =
         model_list %>%
         purrr::pluck(model_name) %>%
         broom::tidy() %>%
         dplyr::filter(term == term_name) %>%
         dplyr::pull(std.error),
       APA =
         model_list %>%
         purrr::pluck(model_name) %>%
         apastylr(term_name))
}

access_data <- function(mediation_model, variable) {
  variable_q <- enquo(variable)

  purrr::pluck(mediation_model, "data") %>%
    dplyr::pull( !! variable_q )
}
