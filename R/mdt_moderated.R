#' @title Fit a moderated mediation model
#'
#' @description Given a dataframe, an IV, a DV, a mediator and a moderator, fit
#'   a moderated mediation model.
#'
#' @details This function is an helper to fit linear models needed to test a
#'   moderated mediation model. The Joint Significant formal approach has been
#'   described in Muller, Judd, & Yzerbyt (2005).
#'
#'   Three models will be fit:
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. \emph{Journal of Personality and
#'   Social Psychology}, 89(6), 852‑863. doi: 10.1037/0022-3514.89.6.852
#'
#' @param data A dataframe containing the variables in the model.
#' @param IV An unquoted variable in the data frame which will be used
#'   as independant variable.
#' @param M An unquoted variable in the data frame which will be used
#'   as mediator.
#' @param DV An unquoted variable in the data frame which will be used
#'   as dependant variable.
#' @param Mod An unquoted variable in the data frame which will be used
#'   as moderator.
#'
#' @family mediation models
#'
#' @export

mdt_moderated <- function(data, IV, DV, M, Mod) {
  UseMethod("mdt_moderated")
}

#' @export
mdt_moderated.data.frame <- function(data, IV, DV, M, Mod) {

  # nse -----------------------------------------------------------------------
  IV_var  <- enquo(IV)
  DV_var  <- enquo(DV)
  M_var   <- enquo(M)
  Mod_var <- enquo(Mod)

  IV_name    <- rlang::quo_name(IV_var)
  DV_name    <- rlang::quo_name(DV_var)
  M_name     <- rlang::quo_name(M_var)
  Mod_name   <- rlang::quo_name(Mod_var)
  IVMod_name <- glue::glue("{IV_name}:{Mod_name}")
  MMod_name  <- glue::glue("{M_name}:{Mod_name}")

  IV_data  <- data %>% dplyr::pull( !! IV_var )  %>% as.numeric()
  M_data   <- data %>% dplyr::pull( !! M_var )   %>% as.numeric()
  DV_data  <- data %>% dplyr::pull( !! DV_var )  %>% as.numeric()
  Mod_data <- data %>% dplyr::pull( !! Mod_var ) %>% as.numeric()


  # type check ----------------------------------------------------------------
  if(!is.numeric(IV_data))
    stop(glue::glue("Warning:
                    IV ({IV_name}) must be numeric (see build_contrast() to
                    convert a character vector to a contrast code)."))

  if(!is.numeric(M_data))
    stop(glue::glue("Warning:
                    Mediator ({M_name}) must be numeric."))

  if(!is.numeric(DV_data))
    stop(glue::glue("Warning:
                    DV ({DV_name}) must be numeric."))

  if(!is.numeric(DV_data))
    stop(glue::glue("Warning:
                    Moderator ({DV_name}) must be numeric."))

  # building models -----------------------------------------------------------
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

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("X * Mod -> Y"       = model1,
         "X * Mod -> M"       = model2,
         "(X + M) * Mod -> Y" = model3) %>%
    purrr::map(~lm(.x, data))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a"       = create_path(js_models, "X * Mod -> M", IV_name),
         "a * Mod" = create_path(js_models, "X * Mod -> M", IVMod_name),
         "b"       = create_path(js_models, "(X + M) * Mod -> Y", M_name),
         "b * Mod" = create_path(js_models, "(X + M) * Mod -> Y", M_name),
         "c"       = create_path(js_models, "X * Mod -> Y", IV_name),
         "c * Mod" = create_path(js_models, "X * Mod -> Y", IVMod_name),
         "c'"      = create_path(js_models, "(X + M) * Mod -> Y", IV_name),
         "c' * Mod"= create_path(js_models, "(X + M) * Mod -> Y", IVMod_name))


  # bulding mediation model object --------------------------------------------
  mediation_model <-
    list(type           = "moderated mediation",
         method         = "Joint significant",
         params         = list("IV"  = IV_name,
                               "DV"  = DV_name,
                               "M"   = M_name,
                               "Mod" = Mod_name),
         paths          = paths,
         indirect_index = FALSE,
         js_models      = js_models,
         data           = data)

  as_mediation_model(mediation_model)
}
