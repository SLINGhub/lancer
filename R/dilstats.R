#' @title Create Linear Model
#' @description A wrapper to create a linear model from dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return A linear model object from `stats:lm()` with formula
#' `signal_var ~ conc_var` from data `diltuion_data`
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' linear_model <- create_linear_model(
#'   dilution_data,
#'   "Dilution_Percent",
#'   "Area"
#' )
#' linear_model
#' @rdname create_linear_model
#' @export
create_linear_model <- function(dilution_data, conc_var, signal_var) {
  conc_var <- paste0("`", conc_var, "`")
  signal_var <- paste0("`", signal_var, "`")

  # Create the formula
  linear_formula <- stats::as.formula(paste(
    signal_var, "~",
    paste(conc_var, collapse = " + ")
  ))

  # Create the linear model on dilution data
  linear_model <- stats::lm(linear_formula, data = dilution_data)

  return(linear_model)
}

#' @title Create Quadratic Model
#' @description A wrapper to create a quadratic model for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return A linear model object from `stats:lm()` with formula
#' `signal_var ~ conc_var + I(conc_var * conc_var)`
#' from data `diltuion_data`
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' quad_model <- create_quad_model(
#'   dilution_data,
#'   "Dilution_Percent",
#'   "Area"
#' )
#' quad_model
#' @rdname create_quad_model
#' @export
create_quad_model <- function(dilution_data, conc_var, signal_var) {
  conc_var <- paste0("`", conc_var, "`")
  signal_var <- paste0("`", signal_var, "`")

  # Create the formula
  quad_formula <- stats::as.formula(paste(
    signal_var, "~",
    paste(
      conc_var, "+",
      paste0(
        "I(", conc_var, " * ",
        conc_var, ")"
      )
    )
  ))

  # Create the quadratic model on dilution data
  quad_model <- stats::lm(quad_formula, data = dilution_data)

  return(quad_model)
}

#' @title Create Cubic Model
#' @description A wrapper to create a cubic model for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return A linear model object from `stats:lm()` with formula
#' `signal_var ~ conc_var + I(conc_var * conc_var * conc_var)`
#' from data `diltuion_data`
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' cubic_model <- create_cubic_model(
#'   dilution_data,
#'   "Dilution_Percent",
#'   "Area"
#' )
#' cubic_model
#' @rdname create_cubic_model
#' @export
create_cubic_model <- function(dilution_data, conc_var, signal_var) {
  conc_var <- paste0("`", conc_var, "`")
  signal_var <- paste0("`", signal_var, "`")

  # Create the formula
  cubic_formula <- stats::as.formula(paste(
    signal_var, "~",
    paste(
      conc_var,
      "+",
      paste0(
        "I(",
        conc_var, " * ",
        conc_var, ")"
      ),
      "+",
      paste0(
        "I(",
        conc_var, " * ",
        conc_var, " * ",
        conc_var, ")"
      )
    )
  ))

  # Create the cubic model on dilution data
  cubic_model <- stats::lm(cubic_formula, data = dilution_data)

  return(cubic_model)
}

#' @title Calculate Kroll's Linearity Test
#' @description Calculate a statistical test for linearity
#' from Kroll et. al. (2000)
#' \doi{10.5858/2000-124-1331-EOTEON}
#' using average deviation from linearity
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return A data frame of statistical results from Kroll et. al. (2000)
#' \doi{10.5858/2000-124-1331-EOTEON}
#' \itemize{
#'   \item \code{adl_kroll}:
#'   Average deviation from linearity as defined in Kroll et. al. (2000)
#'   \doi{10.5858/2000-124-1331-EOTEON}
#'   \item \code{precision_on_percent_scale}:
#'   Estimate of precision on percent scale.
#'   It is the ratio of the estimated regression standard error from
#'   the best-fit polynomial and the estimated solution mean.
#'   \item \code{uncorrected_critical_value}:
#'   Critical value (Uncorrected) as defined in Kroll et. al. (2000)
#'   \doi{10.5858/2000-124-1331-EOTEON}
#'   \item \code{corrected_critical_value}:
#'   Critical value (Corrected) as defined in Hsieh et. al. (2008)
#'   \doi{10.1080/10543400802071378}
#'   \item \code{uncorrected_kroll_results}:
#'   Indicate if \code{adl_kroll} is less than
#'   \code{uncorrected_critical_value}
#'   If \code{FALSE}, there is sufficient evidence to reject the hypothesis that
#'   the points are linear.
#'   \item \code{corrected_kroll_results}:
#'   Indicate if \code{adl_kroll} is less than
#'   \code{corrected_critical_value}
#'   If \code{FALSE}, there is sufficient evidence to reject the hypothesis that
#'   the points are linear.
#'   \item \code{best_model}
#'   Gives values linear, quadratic or cubic to indicate the model
#'   that "best fit" the points. The function
#'   \code{\link[performance]{compare_performance}} is used
#'   to determine the best fit.
#' }
#' @details The function will return NA if the number of dilution points
#' is less than or equal to three.
#' @examples
#' # Data from Kroll's 2000 paper
#' solution_number <- c(
#'   1, 1, 2, 2, 3, 3, 4, 4,
#'   5, 5, 6, 6, 7, 7
#' )
#' result <- c(
#'   352, 348, 1009, 991, 1603, 1584, 3100, 3200,
#'   4482, 4390, 5101, 5046, 5669, 5516
#' )
#'
#' dilution_data <- data.frame(
#'   Solution_Number = solution_number,
#'   Result = result
#' )
#'
#' adl_result <- calculate_adl_kroll_test(
#'   dilution_data,
#'   "Solution_Number",
#'   "Result"
#' )
#' adl_result
#' @rdname calculate_adl_kroll_test
#' @export
#' @references
#' Kroll, M. H., Praestgaard, J., Michaliszyn, E.,
#' & Styer, P. E. (2000).
#' Evaluation of the extent of nonlinearity
#' in reportable range studies.
#' \emph{Archives of pathology & laboratory medicine},
#' \emph{124}(9), 1331–1338.
#' \doi{10.5858/2000-124-1331-EOTEON}
#' \cr\cr
#' Hsieh E., & Liu J. P. (2008).
#' On Statistical Evaluation of the
#' Linearity in Assay Validation
#' \emph{Journal of Biopharmaceutical Statistics},
#' \emph{18}(4), 677–690.
#' \doi{10.1080/10543400802071378}
calculate_adl_kroll_test <- function(dilution_data, conc_var, signal_var) {
  adl_result <- tibble::tibble(
    adl_kroll = NA,
    best_model = NA
  )

  if (is.null(nrow(dilution_data))) {
    return(adl_result)
  }

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(adl_result)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(adl_result)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(adl_result)
  }

  # Create the models on dilution data
  linear_model <- create_linear_model(dilution_data, conc_var, signal_var)
  quad_model <- create_quad_model(dilution_data, conc_var, signal_var)
  cubic_model <- create_cubic_model(dilution_data, conc_var, signal_var)

  g <- performance::compare_performance(linear_model,
    quad_model,
    cubic_model,
    rank = TRUE,
    metrics = c("all")
  )

  best_model <- g$Name[1]

  mean_of_y <- mean(dilution_data[[signal_var]], na.rm = TRUE)
  s_in_paper <- length(unique(dilution_data[[conc_var]]))
  r_in_paper <- dilution_data %>%
    dplyr::group_by(.data[[conc_var]]) %>%
    dplyr::summarise(no_replicates = length(.data[[conc_var]])) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data[["no_replicates"]]) %>%
    max(na.rm = FALSE)

  new_data <- tibble::tibble(!!conc_var := unique(dilution_data[[conc_var]]))

  # Case 1 - linear model is best fitting
  if (best_model == "linear_model") {
    adl_result <- tibble::tibble(
      adl_kroll = NA,
      precision_on_percent_scale = NA,
      uncorrected_critical_value = NA,
      corrected_critical_value = NA,
      uncorrected_kroll_results = NA,
      corrected_kroll_results = NA,
      best_model = "linear"
    )
    return(adl_result)
  }

  # Case 2 - quad model is best fitting
  if (best_model == "quad_model") {
    linear_predict <- stats::predict(linear_model,
      newdata = new_data
    )
    quad_predict <- stats::predict(quad_model,
      newdata = new_data
    )
    adl_kroll <-
      100 * sqrt(sum((quad_predict - linear_predict)^2)
      / (s_in_paper)) / mean_of_y

    sigma <- broom::glance(quad_model)$sigma
    precision_on_percent_scale <- (sigma / mean_of_y) * 100

    non_central_parameter <-
      (5^2 * s_in_paper * r_in_paper) / (precision_on_percent_scale)^2

    q_value <- stats::qchisq(p = .95, ncp = non_central_parameter, df = 2)
    uncorrected_critical_value <-
      precision_on_percent_scale * sqrt((q_value / (s_in_paper * r_in_paper)))

    q_value <- stats::qchisq(p = .05, ncp = non_central_parameter, df = 2)
    corrected_critical_value <-
      precision_on_percent_scale * sqrt((q_value / (s_in_paper * r_in_paper)))

    adl_result <- tibble::tibble(
      adl_kroll = adl_kroll,
      precision_on_percent_scale = precision_on_percent_scale,
      uncorrected_critical_value = uncorrected_critical_value,
      corrected_critical_value = corrected_critical_value,
      uncorrected_kroll_results =
        as.character(adl_kroll < uncorrected_critical_value),
      corrected_kroll_results =
        as.character(adl_kroll < corrected_critical_value),
      best_model = "quadratic"
    )

    return(adl_result)
  }

  # Case 3 - cubic model is best fitting
  if (best_model == "cubic_model") {
    linear_predict <- stats::predict(linear_model,
      newdata = new_data
    )
    cubic_predict <- stats::predict(cubic_model,
      newdata = new_data
    )
    adl_kroll <- 100 *
      sqrt(sum((cubic_predict - linear_predict)^2) /
        (s_in_paper)) / mean_of_y

    sigma <- broom::glance(cubic_model)$sigma
    precision_on_percent_scale <- (sigma / mean_of_y) * 100

    non_central_parameter <-
      (5^2 * s_in_paper * r_in_paper) / (precision_on_percent_scale)^2

    q_value <- stats::qchisq(p = .95, ncp = non_central_parameter, df = 2)
    uncorrected_critical_value <-
      precision_on_percent_scale * sqrt((q_value / (s_in_paper * r_in_paper)))

    q_value <- stats::qchisq(p = .05, ncp = non_central_parameter, df = 2)
    corrected_critical_value <-
      precision_on_percent_scale * sqrt((q_value / (s_in_paper * r_in_paper)))

    adl_result <- tibble::tibble(
      adl_kroll = adl_kroll,
      precision_on_percent_scale = precision_on_percent_scale,
      uncorrected_critical_value = uncorrected_critical_value,
      corrected_critical_value = corrected_critical_value,
      uncorrected_kroll_results =
        as.character(adl_kroll < uncorrected_critical_value),
      corrected_kroll_results =
        as.character(adl_kroll < corrected_critical_value),
      best_model = "cubic"
    )
    return(adl_result)
  }

  return(adl_result)
}

#' @title Calculate Average Deviation From Linearity
#' @description Calculate the average deviation from linearity as
#' defined in Kroll et. al. (2000)
#' \doi{10.5858/2000-124-1331-EOTEON}
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return The average deviation from linearity
#' @details The function will return NA if the number of dilution points
#' is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' adl_value <- calculate_adl(
#'   dilution_data,
#'   "Dilution_Percent", "Area"
#' )
#' adl_value
#' @rdname calculate_adl
#' @export
#' @references
#' Kroll, M. H., Praestgaard, J., Michaliszyn, E.,
#' & Styer, P. E. (2000).
#' Evaluation of the extent of nonlinearity
#' in reportable range studies.
#' \emph{Archives of pathology & laboratory medicine},
#' \emph{124}(9), 1331–1338.
#' \doi{10.5858/2000-124-1331-EOTEON}
calculate_adl <- function(dilution_data, conc_var, signal_var) {
  adl <- NA

  if (is.null(nrow(dilution_data))) {
    return(adl)
  }

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(adl)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(adl)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(adl)
  }

  # Create the models on dilution data
  linear_model <- create_linear_model(dilution_data, conc_var, signal_var)
  quad_model <- create_quad_model(dilution_data, conc_var, signal_var)
  cubic_model <- create_cubic_model(dilution_data, conc_var, signal_var)

  # Get the p values
  linear_pval <- broom::glance(linear_model)$p.value
  quad_pval <- broom::glance(quad_model)$p.value
  cubic_pval <- broom::glance(cubic_model)$p.value

  best_model <- min(linear_pval, quad_pval, cubic_pval, na.rm = TRUE)

  # Case 1 - linear model is best fitting
  if (best_model == linear_pval) {
    return(adl)
  }

  # Case 2 - quad model is best fitting
  if (best_model == quad_pval) {
    linear_predict <- stats::predict(linear_model)
    quad_predict <- stats::predict(quad_model)
    adl <- mean(abs((linear_predict - quad_predict) / linear_predict),
      na.rm = TRUE
    ) * 100
    return(adl)
  }

  # Case 3 - cubic model is best fitting
  if (best_model == cubic_pval) {
    linear_predict <- stats::predict(linear_model)
    cubic_predict <- stats::predict(cubic_model)
    adl <- mean(abs((linear_predict - cubic_predict) / linear_predict),
      na.rm = TRUE
    ) * 100
    return(adl)
  }

  return(adl)
}

#' @title Calculate Concavity
#' @description Calculate the concavity of the Dilution Quadratic Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return The concavity of the Dilution Quadratic Model
#' @details The function will return NA if the number of dilution points
#' is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' concavity_value <- calculate_concavity(
#'   dilution_data,
#'   "Dilution_Percent", "Area"
#' )
#' concavity_value
#' @rdname calculate_concavity
#' @export
calculate_concavity <- function(dilution_data, conc_var, signal_var) {
  concavity <- NA

  if (is.null(nrow(dilution_data))) {
    return(concavity)
  }

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(concavity)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(concavity)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(concavity)
  }

  # Create the quadratic model on dilution data
  quad_model <- create_quad_model(dilution_data, conc_var, signal_var)

  # Get concanvity Value for (x^2) of Quadratic model
  quad_tidy <- broom::tidy(quad_model)
  concavity <- quad_tidy %>%
    dplyr::slice(3) %>%
    dplyr::pull(.data$estimate)

  return(concavity)
}

#' @title Calculate Linear Model's Goodness Of Fit
#' @description Calculate the Goodness of Fit of the Dilution Linear Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return A tibble containing the Goodness of Fit measures of the linear model
#' The Goodness of Fit measures are the Pearson correlation coefficient (R),
#' the R^2 and the adjusted R^2
#' @details The function will return a tibble with NA values
#' if the number of dilution points is less than or equal to three
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' dil_linear_gof <- calculate_gof_linear(
#'   dilution_data,
#'   "Dilution_Percent", "Area"
#' )
#' dil_linear_gof
#' @rdname calculate_gof_linear
#' @export
calculate_gof_linear <- function(dilution_data, conc_var, signal_var) {
  dil_linear_gof <- tibble::tibble(
    r_corr = NA,
    r2_linear = NA,
    r2_adj_linear = NA
  )

  if (is.null(nrow(dilution_data))) {
    return(dil_linear_gof)
  }

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(dil_linear_gof)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(dil_linear_gof)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(dil_linear_gof)
  }

  # Get the correlation results
  cor_result <- broom::tidy(stats::cor.test(dilution_data[[signal_var]],
    dilution_data[[conc_var]],
    method = "pearson"
  ))
  r_corr <- round(cor_result$estimate, digits = 6)

  # Create the linear model on dilution data
  linear_model <- create_linear_model(dilution_data, conc_var, signal_var)

  # Get GOF for each model
  linear_gof <- broom::glance(linear_model)

  # Get R2
  r2_linear <- round(linear_gof$r.squared, digits = 6)

  # Get Adjusted R2
  r2_adj_linear <- round(linear_gof$adj.r.squared, digits = 6)


  dil_linear_gof <- tibble::tibble(
    r_corr = r_corr,
    r2_linear = r2_linear,
    r2_adj_linear = r2_adj_linear
  )

  return(dil_linear_gof)
}


#' @title Calculate Mandel Fitting Test
#' @description Calculate the Mandel fitting test for dilution data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
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
#' mandel_result
#' @rdname calculate_mandel
#' @export
calculate_mandel <- function(dilution_data, conc_var, signal_var) {
  mandel_result <- tibble::tibble(
    mandel_stats = NA,
    mandel_p_val = NA
  )

  if (is.null(nrow(dilution_data))) {
    return(mandel_result)
  }

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(mandel_result)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(mandel_result)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(mandel_result)
  }

  # Create the linear model on dilution data
  linear_model <- create_linear_model(dilution_data, conc_var, signal_var)

  # Create the quadratic model on dilution data
  quad_model <- create_quad_model(dilution_data, conc_var, signal_var)

  # Get some statistics for each point in the linear model
  linear_stat <- broom::augment(linear_model)
  quad_stat <- broom::augment(quad_model)

  # Mandel Test
  mandel_numerator <- sum((linear_stat$.resid)^2, na.rm = TRUE) -
    sum((quad_stat$.resid)^2, na.rm = TRUE)
  mandel_denominator <- sum((quad_stat$.resid)^2, na.rm = TRUE) /
    (nrow(quad_stat) - 3)
  mandel_stats <- mandel_numerator / mandel_denominator
  mandel_p_val <- stats::pf(
    q = mandel_stats,
    df1 = 1,
    df2 = nrow(quad_stat) - 3,
    lower.tail = FALSE
  )

  mandel_result <- tibble::tibble(
    mandel_stats = mandel_stats,
    mandel_p_val = mandel_p_val
  )

  return(mandel_result)
}

#' @title Calculate Linear Model's Percent Residual Accuracy (PRA)
#' @description Calculate the Percent Residual Accuracy (PRA) of the
#' Dilution Linear Model
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
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
#' pra_value
#' @rdname calculate_pra_linear
#' @export
calculate_pra_linear <- function(dilution_data, conc_var, signal_var) {
  pra_linear <- NA

  if (is.null(nrow(dilution_data))) {
    return(pra_linear)
  }

  # Drop rows whose value of signal_var is NA
  # Drop rows whose value of conc_var is 0 or less
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]]) %>%
    dplyr::filter(.data[[conc_var]] > 0)

  # Return NA for too little points
  # Horizontal, Vertical line or single point
  if (nrow(dilution_data) <= 3) {
    return(pra_linear)
  }
  if (stats::sd(dilution_data[[conc_var]]) == 0) {
    return(pra_linear)
  }
  if (stats::sd(dilution_data[[signal_var]]) == 0) {
    return(pra_linear)
  }

  # Create the linear model on dilution data
  linear_model <- create_linear_model(dilution_data, conc_var, signal_var)

  # Get Intercept Value and Slope of Linear model
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

#' @title Validate Dilution Data
#' @description Validate Dilution Data
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @return An error if the column name is not found in the Dilution Data
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100)
#' area <- c(22561, 31178, 39981, 48390, 52171, 53410)
#' dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
#' validate_dilution_data(
#'   dilution_data,
#'   "Dilution_Percent", "Area"
#' )
#' @rdname validate_dilution_data
#' @export
validate_dilution_data <- function(dilution_data, conc_var, signal_var) {

  # Check if conc_var and signal_var is are present in dilution_data
  assertable::assert_colnames(dilution_data, conc_var,
    only_colnames = FALSE, quiet = TRUE
  )
  assertable::assert_colnames(dilution_data, signal_var,
    only_colnames = FALSE, quiet = TRUE
  )
}

#' @title Summarise Dilution Curve Data For One Group
#' @description Get the summary statistics of the dilution data
#' for one group or batch
#' @param dilution_data A data frame or tibble containing dilution data
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @param details If set to `TRUE`, will include more columns in the dilution
#' summary but there are mainly for development and testing.
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
#' dilution_summary <- summarise_dilution_data(dilution_data,
#'                                             "Dilution_Percent", "Area")
#' print(dilution_summary, width = 100)
#' @rdname summarise_dilution_data
#' @export
summarise_dilution_data <- function(dilution_data, conc_var, signal_var,
                                    details = FALSE) {
  mandel_result <- calculate_mandel(dilution_data, conc_var, signal_var)
  dil_linear_gof <- calculate_gof_linear(
    dilution_data,
    conc_var, signal_var
  )

  if (isTRUE(details)) {
    one_value_tibble <- tibble::tibble(
      pra_linear = calculate_pra_linear(dilution_data, conc_var, signal_var),
      concavity = calculate_concavity(dilution_data, conc_var, signal_var),
      adl_value = calculate_adl(dilution_data, conc_var, signal_var)
    )

    kroll_tibble <- calculate_adl_kroll_test(
      dilution_data,
      conc_var,
      signal_var
    )

    dilution_summary <- dil_linear_gof %>%
      dplyr::bind_cols(
        mandel_result,
        one_value_tibble,
        kroll_tibble
      )
  } else {
    one_value_tibble <- tibble::tibble(
      pra_linear = calculate_pra_linear(dilution_data, conc_var, signal_var),
      concavity = calculate_concavity(dilution_data, conc_var, signal_var)
    )

    dilution_summary <- dil_linear_gof %>%
      dplyr::bind_cols(
        mandel_result,
        one_value_tibble
      )
  }

  return(dilution_summary)
}
