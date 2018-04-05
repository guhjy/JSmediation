#' Fit a simple mediation model
#'
#' Given a dataframe, an IV, a DV and a mediator, fit a mediation model
#'
#' @param data a dataframe containing the variables in the model.
#' @param IV an unquoted variable in the data frame which will be used
#'             as independant variable.
#' @param M an unquoted variable in the data frame which will be used
#'             as mediator.
#' @param DV an unquoted variable in the data frame which will be used
#'             as dependant variable.
#'
#' @export

mdt_simple <- function(...) {
  UseMethod("mdt_simple")
}

#' @export
mdt_simple.data.frame <- function(data,
                                  IV,
                                  DV,
                                  M) {

  IV_var <- enquo(IV)
  DV_var <- enquo(DV)
  M_var  <- enquo(M)

  model1 <-
    stats::as.formula(glue::glue("{DV} ~ {IV}",
                                 IV = rlang::f_text(IV_var),
                                 DV = rlang::f_text(DV_var)))

  model2 <-
    stats::as.formula(glue::glue("{M} ~ {IV}",
                                 IV = rlang::f_text(IV_var),
                                 M  = rlang::f_text(M_var)))

  model3 <-
    stats::as.formula(glue::glue("{DV} ~ {IV} + {M}",
                                 DV = rlang::f_text(DV_var),
                                 IV = rlang::f_text(IV_var),
                                 M  = rlang::f_text(M_var)))

  mediation_model <-
    tibble::lst(
      type      = "simple mediation",
      method    = "Joint significant",
      model     = list("IV" = rlang::f_text(IV_var),
                       "DV" = rlang::f_text(DV_var),
                       "M"  = rlang::f_text(M_var)),
      CI        = FALSE,
      js_models =
        list("X -> Y"     = model1,
             "X -> M"     = model2,
             "X + M -> Y" = model3) %>%
        purrr::map(~lm(.x, data)),
      js_models_summary =
        purrr::map(js_models, ~broom::tidy(.x)),
      data =
        data
    )

  as_mediation_model(mediation_model)
}
