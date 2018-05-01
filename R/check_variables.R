check_variables <- function(model) {
  UseMethod("check_variables")
}

check_variables.default <- function(model) {
  NULL
}

check_variables.moderated_mediation <- function(model) {
  IV_n  <- purrr::pluck(model, "params", "IV")
  M_n   <- purrr::pluck(model, "params", "M")
  Mod_n <- purrr::pluck(model, "params", "Mod")

  Var_n <-
    list(IV_n,
       M_n,
       Mod_n)

  Var_check <-
    Var_n %>%
    purrr::map(~access_data(model, .x)) %>%
    purrr::map_lgl(~(is_centered(.x) | is_contrast(.x)))

  Var_n_check <-
    Var_n[!Var_check]

  if(length(Var_n_check) != 0) {
    message("\nMessage:")
    message(
      "It appears that the following variable are not
contrast-coded or centred, please make sure it
is intended:"
    )
    for(var in Var_n_check) {
      message(glue::glue("- {var}"))
    }
  }
}
