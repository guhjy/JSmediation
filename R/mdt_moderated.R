#' Fit a moderated mediation model
#'
#' Given a dataframe, an IV, a DV, a mediator and a moderator, fit a moderated
#'   mediation model.
#'
#' @details This function is an helper to fit linear models needed to test a
#'   moderated mediation model. The Joint Significant formal approach has been
#'   described in Muller, Judd, & Yzerbyt (2005).
#'
#'   Three models will be fit:
#'
#'   \deqn{\hat{Y_i} = \Beta_0 + \Beta_1 X_i + \Beta_2 Mod_i + \Beta_3
#'      X_i Mod_i}{Yi = B0 + B1X + B2Mod + B3XMod}
#'   \deqn{\hat{M_i} = \Beta_0 + \Beta_1 X_i + \Beta_2 Mod_i + \Beta_3
#'      X_i Mod_i}{Yi = B0 + B1X + B2Mod + B3XMod}
#'
#'   \deqn{\hat{M_i} = \Beta_0 + \Beta_1 X_i + \Beta_2 Mod_i + \Beta_3
#'      X_i Mod_i}{Yi = B0 + B1X + B2Mod + B3XMod}
#'
#' @param data a dataframe containing the variables in the model.
#' @param IV an unquoted variable in the data frame which will be used
#'             as independant variable.
#' @param M an unquoted variable in the data frame which will be used
#'             as mediator.
#' @param DV an unquoted variable in the data frame which will be used
#'             as dependant variable.
#' @param Moderator an unquoted variable in the data frame which will be used
#'                    as moderator.
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. \emph{Journal of Personality and
#'   Social Psychology}, 89(6), 852‑863. doi: 10.1037/0022-3514.89.6.852
#'
#' @export

mdt_moderated <- function(...) {
  UseMethod("mdt_moderated")
}

#' @export
mdt_moderated.data.frame <- function(data,
                                     IV,
                                     DV,
                                     M,
                                     Moderator) {

  # create object needed because of NSE
  IV_var  <- enquo(IV)
  DV_var  <- enquo(DV)
  M_var   <- enquo(M)
  Mod_var <- enquo(Moderator)

  IV_name  <- rlang::quo_name(IV_var)
  DV_name  <- rlang::quo_name(DV_var)
  M_name   <- rlang::quo_name(M_var)
  Mod_name <- rlang::quo_name(Mod_var)

  IV_data  <- data %>% dplyr::pull( !! IV_var )  %>% as.numeric()
  Mod_data <- data %>% dplyr::pull( !! Mod_var ) %>% as.numeric()
  M_data   <- data %>% dplyr::pull( !! M_var )   %>% as.numeric()

  if(!(is_centered(IV_data) | is_contrast(IV_data)))
    stop(glue::glue("Warning:\n To apply JS method, {IV_name} should be either centred or contrast-coded."))

  if(!(is_centered(Mod_data) | is_contrast(Mod_data)))
    stop(glue::glue("Warning:\n To apply JS method, {Mod_name} should be either centred or contrast-coded."))

  if(!is_centered(M_data))
    stop(glue::glue("Warning:\n To apply JS method, {M_name} should be centred."))

  # write models' formula
  model1 <-
    stats::as.formula(glue::glue("{DV} ~ {IV} * {Mod}",
                                 IV  = IV_name,
                                 DV  = M_name,
                                 Mod = Mod_name))

  model2 <-
    stats::as.formula(glue::glue("{M} ~ {IV} * {Mod}",
                                 IV  = IV_name,
                                 M   = M_name,
                                 Mod = Mod_name))

  model3 <-
    stats::as.formula(glue::glue("{DV} ~ ({IV} + {M}) * {Mod}",
                                 DV  = DV_name,
                                 IV  = IV_name,
                                 M   = M_name,
                                 Mod = Mod_name))

  mediation_model <-
    tibble::lst(
      type      = "moderated mediation",
      method    = "Joint significant",
      model     = list("IV"  = IV_name,
                       "DV"  = DV_name,
                       "M"   = M_name,
                       "Mod" = Mod_name),
      CI        = FALSE,
      js_models =
        list("X * Mod -> Y"       = model1,
             "X * Mod -> M"       = model2,
             "(X + M) * Mod -> Y" = model3) %>%
        purrr::map(~lm(.x, data)),
      js_models_summary =
        purrr::map(js_models, ~broom::tidy(.x)),
      data =
        data
    )

  as_mediation_model(mediation_model)
}
