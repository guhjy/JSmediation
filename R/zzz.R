is_contrast <- function(x) {
  length(unique(x)) == 2L & sum(unique(x)) == 0
}

is_centered <- function(x) {
  # implementation to deal for floating number rounding error
  isTRUE(all.equal(mean(x), 0))
}

create_path <- function(model_list, model_name, term_name) {
  list(point_estimate =
         model_list %>%
         purrr::pluck(model_name) %>%
         broom::tidy() %>%
         dplyr::filter(term == term_name) %>%
         pull(estimate),
       se  =
         model_list %>%
         purrr::pluck(model_name) %>%
         broom::tidy() %>%
         dplyr::filter(term == term_name) %>%
         pull(std.error),
       APA =
         model_list %>%
         purrr::pluck(model_name) %>%
         apastylr(term_name))
}
