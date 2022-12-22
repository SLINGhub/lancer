#' @title Evaluate Linearity Based On Dilution Summary Data
#' @description Evaluate if a curve is linear based
#' on dilution summary statistics
#'
#' @param dilution_summary A data frame or tibble output from
#' the function [summarise_curve_data()]
#' @param grouping_variable A character vector of
#' column names in `dilution_summary`to indicate which columns should be placed
#' first before the evaluation results. Default: c()
#' @param corrcoef_column A column in `dilution_summary` that holds the
#' correlation coefficient, Default: 'r_corr'
#' @param corrcoef_min_threshold The minimum threshold value of the curve's
#' correlation coefficient to pass being potentially linear.
#' Equality to the threshold is considered a pass, Default: 0.8
#' @param pra_column A column in `dilution_summary` that holds the
#' percent residual accuracy, Default: 'pra_linear'
#' @param pra_min_threshold The minimum threshold value of the curve's
#' percent residual accuracy to pass being potentially linear.
#' Equality to the threshold is considered a pass, Default: 80
#' @param mandel_p_val_column A column in `dilution_summary` that holds the
#' p value results for the Mandel's fitting test, Default: 'mandel_p_val'
#' @param mandel_p_val_threshold The threshold value of the curve's
#' p value for the Mandel's fitting test to reject the hypothesis that
#' the quadratic model fits better than the linear model.
#' Default: 0.05
#' @param concavity_column A column in `dilution_summary` that holds the
#' concavity of the quadratic model, Default: 'concavity'
#' @return A data frame or tibble with evaluation results
#' @details Two work flows are given to evaluate linearity of dilution
#' curves. The results are highlighted as columns `wf1_group` and
#' `wf2_group` for now. Column names used to categorise the dilution
#' curves will be moved to the front allow with `wf1_group` and
#' `wf2_group`
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462, 0.970618, 0.969348)
#'
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473, 72.91220, 72.36528)
#'
#' mandel_p_val <- c(
#'   2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'   3.195779e-08, 6.366588e-08
#' )
#'
#' concavity <- c(
#'   -4133.501328, -4146.745747, -3.350942, -3.393617,
#'   0.3942824, 0.4012963
#' )
#'
#' curve_summary <- data.frame(
#'   r_corr = r_corr, pra_linear = pra_linear,
#'   mandel_p_val = mandel_p_val,
#'   concavity = concavity
#' )
#'
#' curve_summary <- evaluate_linearity(curve_summary)
#'
#' print(curve_summary, width = 100)
#'
#' @rdname evaluate_linearity
#' @export
evaluate_linearity <- function(dilution_summary,
                               grouping_variable = c(),
                               corrcoef_column = "r_corr",
                               corrcoef_min_threshold = 0.8,
                               pra_column = "pra_linear",
                               pra_min_threshold = 80,
                               mandel_p_val_column = "mandel_p_val",
                               mandel_p_val_threshold = 0.05,
                               concavity_column = "concavity") {

  # Check if these column name existed
  col_names <- c(corrcoef_column, pra_column)
  non_df_cols <- col_names[!col_names %in% colnames(dilution_summary)]

  if (length(non_df_cols) > 0) {
    # If no, return the summary data with a warning
    warning(paste0(
      "These column names need to be used
                    but not in your input dataframe: ",
      paste(non_df_cols, collapse = " ")
    ))
    return(dilution_summary)
  } else {
    # Else perform the evaluation
    dilution_summary <- dilution_summary %>%
      dplyr::mutate(
        wf1_group =
          dplyr::case_when(
            .data[[corrcoef_column]] < corrcoef_min_threshold
            ~ "Poor Linearity",
            .data[[corrcoef_column]] >= corrcoef_min_threshold &
              .data[[pra_column]] < pra_min_threshold
            ~ "Poor Linearity",
            .data[[corrcoef_column]] >= corrcoef_min_threshold &
              .data[[pra_column]] >= pra_min_threshold
            ~ "Good Linearity",
          )
      )
  }

  # Check if these column name existed
  col_names <- c(mandel_p_val_column, concavity_column)
  non_df_cols <- col_names[!col_names %in% colnames(dilution_summary)]

  if (length(non_df_cols) > 0) {
    # If no, return the summary data with a warning
    warning(paste0(
      "These column names need to be used
                    but not in your input dataframe: ",
      paste(non_df_cols, collapse = " ")
    ))

    # Rearrange the column based on the first evaluation
    dilution_summary <- dilution_summary %>%
      dplyr::relocate(
        dplyr::any_of(
          c(grouping_variable, "wf1_group",
            corrcoef_column, pra_column)
        )
      )

    return(dilution_summary)
  } else {
    # Else perform the evaluation
    dilution_summary <- dilution_summary %>%
      dplyr::mutate(
        wf2_group =
          dplyr::case_when(
            .data[["wf1_group"]] == "Poor Linearity" &
              .data[[corrcoef_column]] >= corrcoef_min_threshold &
              .data[[mandel_p_val_column]] <
                mandel_p_val_threshold &
              .data[[concavity_column]] >= 0 ~ "Noise Regime",
            .data[["wf1_group"]] == "Poor Linearity" &
              .data[[corrcoef_column]] >= corrcoef_min_threshold &
              .data[[mandel_p_val_column]] <
                mandel_p_val_threshold &
              .data[[concavity_column]] < 0 ~ "Saturation Regime",
            TRUE ~ wf1_group
          )
      )
  }

  # Rearrange the column based on the first evaluation
  dilution_summary <- dilution_summary %>%
    dplyr::relocate(
      dplyr::any_of(
        c(grouping_variable, "wf1_group", "wf2_group",
          corrcoef_column, pra_column,
          mandel_p_val_column, concavity_column)
      )
    )


  return(dilution_summary)
}
