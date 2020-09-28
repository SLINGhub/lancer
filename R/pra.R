calculate_concavity <- function(dilution_data, conc_var, signal_var) {

  #Create the formula
  quad_formula <- stats::as.formula(paste(signal_var, "~",
                                   paste(conc_var, "+",
                                         paste0("I(",conc_var, " * ",
                                                conc_var,")")
                                   )
  )
  )

  #Create the quadratic model on dilution data
  quad_model <- stats::lm(quad_formula, data = dilution_data)

  #Get concanvity Value for (x^2) of Quadratic model
  quad_tidy <- broom::tidy(quad_model)
  concavity <- quad_tidy %>%
    dplyr::filter(.data$term == "I(Dilution_Percent * Dilution_Percent)") %>%
    dplyr::pull(.data$estimate)

  return(concavity)

}


calculate_mandel <- function(dilution_data, conc_var, signal_var) {

  #Create the formula
  linear_formula <- stats::as.formula(paste(signal_var, "~",
                                     paste(conc_var, collapse = " + ")
  )
  )
  quad_formula <- stats::as.formula(paste(signal_var, "~",
                                   paste(conc_var, "+",
                                         paste0("I(",conc_var, " * ",
                                                conc_var,")")
                                   )
  )
  )

  #Create the linear model on dilution data
  linear_model <- stats::lm(linear_formula, data = dilution_data)

  #Create the quadratic model on dilution data
  quad_model <- stats::lm(quad_formula, data = dilution_data)

  #Get some statistics for each point in the linear model
  linear_stat <- broom::augment(linear_model)
  quad_stat <- broom::augment(quad_model)

  # Mandel Test
  mandel_numerator <- sum((linear_stat$.resid)^2, na.rm = TRUE) -
    sum((quad_stat$.resid)^2, na.rm = TRUE)
  mandel_denominator <- sum((quad_stat$.resid)^2, na.rm = TRUE) /
    (nrow(quad_stat) - 3)
  mandel_stats <- mandel_numerator / mandel_denominator
  mandel_p_val <- stats::pf(q = mandel_stats,
                            df1 = 1,
                            df2 = nrow(quad_stat) - 3,
                            lower.tail = FALSE)

  mandel_result <- tibble::tibble(mandel_stats = mandel_stats,
                                  mandel_p_val = mandel_p_val)

  return(mandel_result)

}


calculate_pra <- function(dilution_data, conc_var, signal_var) {

  #Create the formula
  #linear_formuala <- syms_reduce(c(signal_var, conc_var), op = "~")
  linear_formula <- stats::as.formula(paste(signal_var, "~",
                                      paste(conc_var, collapse = " + ")
  )
  )

  #Create the linear model on Dilution Data
  linear_model <- stats::lm(linear_formula, data = dilution_data)

  #Get Intercept Value and Slope of Linear model
  linear_tidy <- broom::tidy(linear_model)
  intercept <- linear_tidy %>%
    dplyr::filter(.data$term == "(Intercept)") %>%
    dplyr::pull(.data$estimate)
  slope <- linear_tidy %>%
    dplyr::filter(.data$term == conc_var) %>%
    dplyr::pull(.data$estimate)

  #Get some statistics for each point in the linear model
  linear_stat <- broom::augment(linear_model)

  fit_aug <- linear_stat %>%
    dplyr::mutate(
      .xfitted = (.data[[signal_var]] - intercept) / (slope),
      .xerror = (.data[[conc_var]] - .data$.xfitted) / .data[[conc_var]]
    )

  # Get GOF summary for linear model
  valid_xerror_data <- fit_aug$.xerror[is.finite(fit_aug$.xerror)]

  valid_dilution_point_amount <- sum(!is.na(valid_xerror_data), na.rm = TRUE)
  percent_ra <- 100 *
    sum((1 - abs(valid_xerror_data)), na.rm = TRUE) /
    valid_dilution_point_amount

  return(percent_ra)
}

# validate_dilution_data <- function(dilution_data, conc_var, signal_var) {
#
#   validator <- data.validator::create_validator()
#
#   #Check if conc_var and signal_var is are valid columns in dilution_data
#   #Drop rows whose value of signal_var is NA
#   #assertr::verify(assertr::has_all_names(conc_var, signal_var)) %>%
#   dilution_data <- dilution_data %>%
#     assertr::verify(description = "Column has correct names",
#                     assertr::has_all_names("Dilution_Percen"),
#                     success_fun = assertr::success_continue,
#                     error_fun = assertr::error_append
#                     ) %>%
#     assertr::verify(description = "Column has correct names",
#                     assertr::has_all_names("Area"),
#                     success_fun = assertr::success_continue,
#                     error_fun = assertr::error_append
#     ) %>%
#     data.validator::add_results(validator)
#
#   print(dilution_data)
#
# }

get_dilution_summary <- function(dilution_data, conc_var, signal_var) {

  #Try to add validation checks

  #Create an empty summary table
  res <- tibble::tibble(r_corr = NA,
                        r2_linear = NA,
                        r2_quad = NA,
                        r2_adj_linear = NA,
                        r2_adj_quad = NA,
                        concavity = NA,
                        mandel_stats = NA,
                        mandel_p_val = NA,
                        percent_ra_linear = NA
                        )

  #Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  pra <- NA

  if (nrow(dilution_data) > 3) {

    pra <- calculate_pra(dilution_data, conc_var, signal_var)

  }
  return(pra)
}
