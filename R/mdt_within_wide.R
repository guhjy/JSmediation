#' Fit a within-participant mediation model (wide-format input)
#'
#' @param data a data frame containing the variables in the model.
#' @param DV_A a
#' @param DV_B a
#' @param M_A a
#' @param M_B a
#'
#' @export
mdt_within_wide <- function(data, DV_A, DV_B, M_A, M_B) {
  UseMethod("mdt_within_wide")
}

#' @export
mdt_within_wide.data.frame <- function(data, DV_A, DV_B, M_A, M_B) {

  # nse -----------------------------------------------------------------------
  DV_A_var       <- enquo(DV_A)
  DV_B_var       <- enquo(DV_B)
  M_A_var        <- enquo(M_A)
  M_B_var        <- enquo(M_B)


  DV_A_name       <- rlang::quo_name(DV_A_var)
  DV_B_name       <- rlang::quo_name(DV_B_var)
  M_A_name        <- rlang::quo_name(M_A_var)
  M_B_name        <- rlang::quo_name(M_B_var)

  DV_A_data       <- data %>% dplyr::pull( !! DV_A_var )
  DV_B_data       <- data %>% dplyr::pull( !! DV_B_var )
  M_A_data        <- data %>% dplyr::pull( !! M_A_var )
  M_B_data        <- data %>% dplyr::pull( !! M_B_var )

  # type check ----------------------------------------------------------------

  if(!is.numeric(DV_A_data))
    stop(glue::glue("Warning:
                    Mediator ({DV_A_name}) must be numeric."))

  if(!is.numeric(DV_B_data))
    stop(glue::glue("Warning:
                    Mediator ({DV_B_name}) must be numeric."))

  if(!is.numeric(M_A_data))
    stop(glue::glue("Warning:
                    Mediator ({M_A_name}) must be numeric."))

  if(!is.numeric(M_B_data))
    stop(glue::glue("Warning:
                    Mediator ({M_B_name}) must be numeric."))

  # data wrangling ------------------------------------------------------------
  dataset <-
    data %>%
    dplyr::mutate(M_diff  = !!M_A_var - !!M_B_var ,
                  M_mean  = (!!M_A_var + !!M_B_var) / 2,
                  M_mean_center = scale((!!M_A_var + !!M_B_var) / 2, scale = FALSE),
                  DV_diff = !!DV_A_var - !!DV_B_var)

  # bulding models ------------------------------------------------------------
  model1 <-
    stats::as.formula(DV_diff ~ 1)

  model2 <-
    stats::as.formula(M_diff ~ 1)

  model3 <-
    stats::as.formula(DV_diff ~ 1 + M_diff + M_mean_center)

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("1 -> DV_diff"                   = model1,
         "1 -> M_diff"                    = model2,
         "1 + M_diff + M_mean -> DV_diff" = model3) %>%
    purrr::map(~lm(.x, dataset))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a"  = create_path(js_models, "1 -> M_diff", "(Intercept)"),
         "b"  = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "M_diff"),
         "c"  = create_path(js_models, "1 -> DV_diff", "(Intercept)"),
         "c'" = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "(Intercept)"))

  # bulding mediation model object --------------------------------------------
  mediation_model(
    type      = "within-participant mediation",
    params    = list("DV difference" = glue::glue("{DV_A_name} - {DV_B_name}"),
                     "M difference"  = glue::glue("{M_A_name} - {M_B_name}")),
    paths     = paths,
    js_models = js_models,
    data      = data,
    subclass  = "within_participant_mediation"
  )
}
