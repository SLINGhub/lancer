#' @title Create Dilution Linear Model
#' @description A wrapper to create a linear model for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return A linear model object from `stats:lm()` with formula
#' `signal_var ~ conc_var` from data `diltuion_data`
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' linear_model <- create_dil_linear_model(dilution_data,
#'                                         "Dilution_Percent",
#'                                         "Area")
#' @rdname create_dil_linear_model
#' @export
create_dil_linear_model <- function(dilution_data, conc_var, signal_var) {

  #Create the formula
  linear_formula <- stats::as.formula(paste(signal_var, "~",
                                            paste(conc_var, collapse = " + ")
  )
  )

  #Create the linear model on dilution data
  linear_model <- stats::lm(linear_formula, data = dilution_data)

  return(linear_model)

}

#' @title Create Dilution Quadratic Model
#' @description A wrapper to create a quadratic model for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return A linear model object from `stats:lm()` with formula
#' `signal_var ~ conc_var + I(conc_var * conc_var)`
#' from data `diltuion_data`
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' linear_model <- create_dil_quad_model(dilution_data,
#'                                         "Dilution_Percent",
#'                                         "Area")
#' @rdname create_dil_quad_model
#' @export
create_dil_quad_model <- function(dilution_data, conc_var, signal_var) {

  #Create the formula
  quad_formula <- stats::as.formula(paste(signal_var, "~",
                                          paste(conc_var, "+",
                                                paste0("I(", conc_var, " * ",
                                                       conc_var, ")")
                                          )
  )
  )

  #Create the quadratic model on dilution data
  quad_model <- stats::lm(quad_formula, data = dilution_data)

  return(quad_model)

}

#' @title Calculate concavity
#' @description Calculate the concavity of the Dilution Quadratic Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return The concavity of the Dilution Quadratic Model
#' @details The function will return NA if the number of dilution points
#' is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' concavity_value <- calculate_concavity(dilution_data,
#'                                        "Dilution_Percent", "Area")
#' @rdname calculate_concavity
#' @export
calculate_concavity <- function(dilution_data, conc_var, signal_var) {

  concavity <- NA

  if (is.null(nrow(dilution_data))) {
    return(concavity)
  }

  #Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  if (nrow(dilution_data) <= 3) {
    return(concavity)
  }

  #Create the quadratic model on dilution data
  quad_model <- create_dil_quad_model(dilution_data, conc_var, signal_var)

  #Get concanvity Value for (x^2) of Quadratic model
  quad_tidy <- broom::tidy(quad_model)
  concavity <- quad_tidy %>%
    dplyr::slice(3) %>%
    dplyr::pull(.data$estimate)

  return(concavity)

}

#' @title Calculate Goodness of Linear Fit
#' @description Calculate the Goodness of Fit of the Dilution Linear Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return A tibble containing the Goodness of Fit measures of the linear model
#' The Goodness of Fit measures are the Pearson correlation coefficient (R),
#' the R^2, the adjusted R^2, the Bayesian Information Criterion (BIC) and the
#' Pearson correlation p value
#' @details The function will return a tibble with NA values
#' if the number of dilution points is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
#'                                            "Dilution_Percent", "Area")
#' @rdname calculate_dil_linear_gof
#' @export
calculate_dil_linear_gof <- function(dilution_data, conc_var, signal_var) {

  dil_linear_gof <- tibble::tibble(r_corr = NA,
                                   r2_linear = NA,
                                   r2_adj_linear = NA,
                                   bic_linear = NA,
                                   corr_p_val = NA)

  if (is.null(nrow(dilution_data))) {
    return(dil_linear_gof)
  }

  #Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  if (nrow(dilution_data) <= 3) {
    return(dil_linear_gof)
  }

  #Get the correlation results
  cor_result <- broom::tidy(stats::cor.test(dilution_data[[signal_var]],
                                            dilution_data[[conc_var]],
                                            method = "pearson"))
  r_corr <- round(cor_result$estimate, digits = 6)
  corr_p_val <- cor_result$p.value

  #Create the linear model on dilution data
  linear_model <- create_dil_linear_model(dilution_data, conc_var, signal_var)

  # Get GOF for each model
  linear_gof <- broom::glance(linear_model)

  # Get R2
  r2_linear <- round(linear_gof$r.squared, digits = 6)

  # Get Adjusted R2
  r2_adj_linear <- round(linear_gof$adj.r.squared, digits = 6)

  # Get BIC
  bic_linear <- round(linear_gof$BIC, digits = 6)

  dil_linear_gof <- tibble::tibble(r_corr = r_corr,
                                   r2_linear = r2_linear,
                                   r2_adj_linear = r2_adj_linear,
                                   bic_linear = bic_linear,
                                   corr_p_val = corr_p_val)

  return(dil_linear_gof)

}


#' @title Calculate the Mandel fitting test
#' @description Calculate the Mandel fitting test for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return A tibble containing Mandel test statistics. The values are
#' the Mandel test value and its corresponding p value. More information
#' of the test can be found in
#' \url{https://link.springer.com/article/10.1007/s00769-002-0487-6}
#' @details The function will return a tibble with NA values
#' if the number of dilution points is less than or equal to three.
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
#' @rdname calculate_mandel
#' @export
calculate_mandel <- function(dilution_data, conc_var, signal_var) {

  mandel_result <- tibble::tibble(mandel_stats = NA,
                                  mandel_p_val = NA)

  if (is.null(nrow(dilution_data))) {
    return(mandel_result)
  }

  #Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  if (nrow(dilution_data) <= 3) {
    return(mandel_result)
  }

  #Create the linear model on dilution data
  linear_model <- create_dil_linear_model(dilution_data, conc_var, signal_var)

  #Create the quadratic model on dilution data
  quad_model <- create_dil_quad_model(dilution_data, conc_var, signal_var)

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

#' @title Linear Model's Percent Residual Accuracy (PRA)
#' @description Calculate the Percent Residual Accuracy (PRA) of the
#' Dilution Linear Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return The Percent Residual Accuracy (PRA) of the
#' Dilution Linear Model. More information of this value can be found in
#' \url{https://www.sciencedirect.com/science/article/abs/pii/S0039914018307549}
#' @details The function will return NA if the number of dilution points
#' is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' pra_value <- calculate_pra_linear(dilution_data, "Dilution_Percent", "Area")
#' @rdname calculate_pra_linear
#' @export
calculate_pra_linear <- function(dilution_data, conc_var, signal_var) {

  pra_linear <- NA

  if (is.null(nrow(dilution_data))) {
    return(pra_linear)
  }

  #Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  if (nrow(dilution_data) <= 3) {
    return(pra_linear)
    }

  #Create the linear model on dilution data
  linear_model <- create_dil_linear_model(dilution_data, conc_var, signal_var)

  #Get Intercept Value and Slope of Linear model
  linear_tidy <- broom::tidy(linear_model)
  intercept <- linear_tidy %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$estimate)
  slope <- linear_tidy %>%
    dplyr::slice(2) %>%
    dplyr::pull(.data$estimate)

  fit_aug <- tibble::tibble(
    .xfitted = (dilution_data[[signal_var]] - intercept) / (slope),
    .xerror = (dilution_data[[conc_var]] - .data$.xfitted) /
      dilution_data[[conc_var]]
    )

  # Get GOF summary for linear model
  valid_xerror_data <- fit_aug$.xerror[is.finite(fit_aug$.xerror)]

  valid_dilution_point_amount <- sum(!is.na(valid_xerror_data), na.rm = TRUE)
  pra_linear <- 100 *
    sum((1 - abs(valid_xerror_data)), na.rm = TRUE) /
    valid_dilution_point_amount

  return(pra_linear)
}

# validate_dilution_data2 <- function(dilution_data, conc_var, signal_var,
#                                    validator) {
#
#   #Check if conc_var and signal_var is are present in dilution_data
#   #Drop rows whose value of signal_var is NA
#   dilution_data %>%
#     assertr::chain_start() %>%
#     assertr::verify(description = paste0("Column \"", conc_var,
#                                          "\" is absent in data"),
#                     assertr::has_all_names(conc_var),
#                     success_fun = assertr::success_continue,
#                     error_fun = assertr::error_append
#                     ) %>%
#     assertr::verify(description = paste0("Column \"", signal_var,
#                                          "\" is absent in data"),
#                     assertr::has_all_names(signal_var),
#                     success_fun = assertr::success_continue,
#                     error_fun = assertr::error_append
#     ) %>%
#     assertr::chain_end(error_fun = assertr::error_append) %>%
#     data.validator::add_results(validator)
#
#   return(validator)
#
# }

#' @title Validate Dilution Data
#' @description Validate Dilution Data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return An error if the column name is not found in the Dilution Data
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' validate_dilution_data(dilution_data,
#'                        "Dilution_Percent", "Area")
#' @rdname validate_dilution_data
#' @export
validate_dilution_data <- function(dilution_data, conc_var, signal_var) {

  #Check if conc_var and signal_var is are present in dilution_data
  assertable::assert_colnames(dilution_data, conc_var,
                              only_colnames = FALSE, quiet = TRUE)
  assertable::assert_colnames(dilution_data, signal_var,
                              only_colnames = FALSE, quiet = TRUE)


}

#' @title Get dilution data summary statistic
#' @description Get the summary statistics of the dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `diltuion_data` to indicate concentration
#' @param signal_var Column name in `diltuion_data` to indicate signal
#' @return A tibble containing the Goodness of Fit measures of the linear model
#' The Goodness of Fit measures are the Pearson correlation coefficient (R),
#' R^2, adjusted R^2, Bayesian Information Criterion (BIC), Pearson correlation
#' p value, Mandel test statistics and p value, Percent Residual Accuracy and
#' Concavity.
#' @details The function will return a tibble with NA values
#' if the number of dilution points is less than or equal to three
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' dilution_summary <- get_dilution_summary(dilution_data,
#'                                          "Dilution_Percent", "Area")
#' @rdname get_dilution_summary
#' @export
get_dilution_summary <- function(dilution_data, conc_var, signal_var) {

  #Try to add validation checks

  mandel_result <- calculate_mandel(dilution_data, conc_var, signal_var)
  dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
                                             conc_var, signal_var)
  one_value_tibble <- tibble::tibble(
    pra_linear = calculate_pra_linear(dilution_data, conc_var, signal_var),
    concavity = calculate_concavity(dilution_data, conc_var, signal_var)
  )

  dilution_summary <- dplyr::bind_cols(dil_linear_gof,
                                       mandel_result,
                                       one_value_tibble
                                       )

  #arrange them in the right order

  return(dilution_summary)
}
