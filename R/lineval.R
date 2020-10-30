#' @title Evaluate linearity
#' @description Evaluate if a curve is linear based
#' on dilution summary statistics
#' @param dilution_summary A data frame or tibble output
#' from the function `get_dilution_summary`
#' @return A data frame or tibble with evaluation results
#' @details Two work flows are given to evaluate linearity of dilution
#' curves. The results are highlighted as columns curve_group1 and
#' curve_group2 for now.
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462, 0.970618, 0.969348)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473, 72.91220, 72.36528)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963)
#' dilution_summary <- data.frame(r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' dilution_summary <- evaluate_linearity(dilution_summary)
#' @rdname evaluate_linearity
#' @export
evaluate_linearity <- function(dilution_summary) {

  #Check if these column name existed
  col_names <- c("r_corr", "pra_linear")
  non_df_cols <- col_names[!col_names %in% colnames(dilution_summary)]

  if (length(non_df_cols) > 0) {
    #If no, return the summary data with a warning
    warning(paste0("These columns exist in colnames
                    but not in your dataframe: ",
                paste(non_df_cols, collapse = " ")))
    return(dilution_summary)
  } else {
    #Else perform the evaluation
    dilution_summary <- dilution_summary %>%
      dplyr::mutate(curve_group1 =
                      dplyr::case_when(
                        .data[["r_corr"]] < 0.8 ~ "Poor Linearity",
                        .data[["r_corr"]] >= 0.8 &
                          .data[["pra_linear"]] < 80 ~ "Poor Linearity",
                        .data[["r_corr"]] >= 0.8 &
                          .data[["pra_linear"]] >= 80 ~ "Good Linearity",
                      )
      )
  }

  #Check if these column name existed
  col_names <- c("mandel_p_val", "concavity")
  non_df_cols <- col_names[!col_names %in% colnames(dilution_summary)]

  if (length(non_df_cols) > 0) {
    #If no, return the summary data with a warning
    warning(paste0("These columns exist in colnames
                    but not in your dataframe: ",
                   paste(non_df_cols, collapse = " ")))

    #Rearrange the column based on the first evaluation
    dilution_summary <- dilution_summary %>%
    dplyr::relocate(.data[["curve_group1"]], .data[["r_corr"]],
                    .data[["pra_linear"]])

    return(dilution_summary)
  } else {
    #Else perform the evaluation
    dilution_summary <- dilution_summary %>%
      dplyr::mutate(curve_group2 =
                      dplyr::case_when(
                        .data[["curve_group1"]] == "Poor Linearity" &
                          .data[["r_corr"]] >= 0.8 &
                          .data[["mandel_p_val"]] < 0.05 &
                          .data[["concavity"]] >= 0 ~ "LOD",
                        .data[["curve_group1"]] == "Poor Linearity" &
                          .data[["r_corr"]] >= 0.8 &
                          .data[["mandel_p_val"]] < 0.05 &
                          .data[["concavity"]] < 0 ~ "Saturation",
                        TRUE ~ curve_group1
                      )
                    )

  }

  #Rearrange the column based on the first evaluation
  dilution_summary <- dilution_summary %>%
    dplyr::relocate(.data[["curve_group1"]], .data[["curve_group2"]],
                    .data[["r_corr"]], .data[["pra_linear"]],
                    .data[["mandel_p_val"]], .data[["concavity"]]
                    )

  return(dilution_summary)

}
