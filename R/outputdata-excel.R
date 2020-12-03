#' @title Mark near zero columns
#' @description Mark numeric columns with near zero values from a dataset
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' but it can also be any generic data frame or tibble
#' @return A data frame or tibble with the class with numeric columns
#' with near zero values changed from numeric to scientific
#' @details We mark these columns as scientific so that `openxlsx` can
#' output these columns n scientific notations
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462,
#'             0.970618, 0.969348, 0.343838, 0.383552)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473,
#'                 72.91220, 72.36528, -233.05949, -172.13659)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963, -19.9469621, -22.6144875)
#' dilution_summary <- data.frame(r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' dilution_summary <- mark_near_zero_columns(dilution_summary)
#' @rdname mark_near_zero_columns
#' @export
mark_near_zero_columns <- function(dilution_summary) {


  # Check if we have numeric columns
  # If have none, return as it is
  remaining_cols <- dilution_summary %>%
    dplyr::select_if(function(col) is.numeric(col)) %>%
    colnames() %>%
    length()
  if (remaining_cols < 1) {
    return(dilution_summary)
  }

  # Take the absolute value for each numeric column
  # Collect the minimum value
  # If it is small enough, change the class to scientific
  near_zero_columns <- dilution_summary %>%
    dplyr::mutate_if(is.numeric, abs) %>%
    dplyr::summarise_if(is.numeric, min, na.rm = TRUE) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "summary_stats",
                        values_to = "min") %>%
    dplyr::filter(.data$min < 0.01) %>%
    dplyr::pull(.data$summary_stats)

  for (variables in near_zero_columns) {
    class(dilution_summary[[variables]]) <- "scientific"
  }

  return(dilution_summary)

}


#' @title Get column number style
#' @description Get column number style for excel
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' but it can also be any generic data frame or tibble
#' @param workbook A workbook object from `openxlsx`
#' @param sheet The name of the sheet to apply the numeric style on `workbook`
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462,
#'             0.970618, 0.969348, 0.343838, 0.383552)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473,
#'                 72.91220, 72.36528, -233.05949, -172.13659)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963, -19.9469621, -22.6144875)
#' dilution_summary <- data.frame(r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' dilution_summary <- mark_near_zero_columns(dilution_summary)
#' # Create a new workbook
#' my_workbook <- openxlsx::createWorkbook()
#' # Create a new worksheet
#' openxlsx::addWorksheet(wb = my_workbook, sheetName = "Dilution Summary")
#' get_column_number_style(dilution_summary,
#'                         workbook = my_workbook, sheet = "Dilution Summary")
#' @rdname get_column_number_style
#' @export
get_column_number_style <- function(dilution_summary, workbook, sheet) {

  # Get the class for each column
  classes <- dilution_summary %>%
    purrr::map_chr(class) %>%
    unname()

  #Format numeric style based on the class of column names
  if (length(which(classes == "numeric")) != 0) {
    s <- openxlsx::createStyle(numFmt = "0.00")
    openxlsx::addStyle(wb = workbook, sheet = sheet, style = s,
                       rows = 2:(nrow(dilution_summary) + 1),
                       cols = which(classes == "numeric"), gridExpand = TRUE)

  }

  if (length(which(classes == "scientific")) != 0) {
    s <- openxlsx::createStyle(numFmt = "0.00E+00")
    openxlsx::addStyle(wb = workbook, sheet = sheet, style = s,
                       rows = 2:(nrow(dilution_summary) + 1),
                       cols = which(classes == "scientific"), gridExpand = TRUE)

  }

}



#' @title Two colour word conditional formatting
#' @description Perform conditional formatting of two colours
#' based on if a given word on a given character column from `dilution summary`
#' @param workbook A workbook object from `openxlsx`
#' @param sheet The name of the sheet to apply the numeric style on `workbook`
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' @param conditional_column A string to indicate which column
#' in `dilution_summary` to use
#' @param pass_criteria_words A character vector to highlight
#' which words it must contain to
#' give a passing colour on the cell.
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462,
#'             0.970618, 0.969348, 0.343838, 0.383552)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473,
#'                 72.91220, 72.36528, -233.05949, -172.13659)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963, -19.9469621, -22.6144875)
#' dilution_summary <- data.frame(r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' dilution_summary <- mark_near_zero_columns(dilution_summary)
#' # Create a new workbook
#' my_workbook <- openxlsx::createWorkbook()
#'
#' # Create a new worksheet
#' openxlsx::addWorksheet(wb = my_workbook, sheetName = "Dilution Summary")
#'
#' # Write to worksheet as an Excel Table
#' openxlsx::writeDataTable(wb = my_workbook, sheet = "Dilution Summary",
#'                          x = dilution_summary,
#'                          withFilter = TRUE,
#'                          bandedRows = FALSE
#'                         )
#'
#' # Conditional formatting can only be done
#' # after data is written to excel sheet
#' two_col_word_cond_format(workbook = my_workbook,sheet = "Dilution Summary",
#'                          dilution_summary = dilution_summary,
#'                          conditional_column = "wf1_group",
#'                          pass_criteria_words = c("Good Linearity")
#'                          )
#' @rdname two_col_word_cond_format
#' @export
two_col_word_cond_format <- function(workbook, sheet,
                                     dilution_summary, conditional_column,
                                     pass_criteria_words) {

  col_index <- which(colnames(dilution_summary) %in% conditional_column)

  pos_style <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  neg_style <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")

  if (length(col_index) != 0) {

    for (words in pass_criteria_words) {
      openxlsx::conditionalFormatting(workbook, sheet, cols = col_index,
                                      rows = 2:(nrow(dilution_summary) + 1),
                                      rule = words, style = pos_style,
                                      type = "contains")
      openxlsx::conditionalFormatting(workbook, sheet, cols = col_index,
                                      rows = 2:(nrow(dilution_summary) + 1),
                                      rule = words, style = neg_style,
                                      type = "notContains")

    }
  }




}

#' @title Two colour number conditional formatting
#' @description Perform conditional formatting of
#' two colours based on a given threshold value
#' on a given numeric column from `dilution summary`
#' @param workbook A workbook object from `openxlsx`
#' @param sheet The name of the sheet to apply the numeric style on `workbook`
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' @param conditional_column A character vector to
#' indicate which column in `dilution_summary` to use
#' @param threshold_value The threshold value to indicate a pass or fail
#' @param pass_criteria To indicate pass if the value
#' is above or below threshold value,
#' Default: c("above", "below")
#' @param pass_equality To indicate if equality to the
#' threshold value is considered a pass or fail,
#' Default: TRUE
#' @examples
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462,
#'             0.970618, 0.969348, 0.343838, 0.383552)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473,
#'                 72.91220, 72.36528, -233.05949, -172.13659)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963, -19.9469621, -22.6144875)
#' dilution_summary <- data.frame(r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' dilution_summary <- mark_near_zero_columns(dilution_summary)
#' # Create a new workbook
#' my_workbook <- openxlsx::createWorkbook()
#'
#' # Create a new worksheet
#' openxlsx::addWorksheet(wb = my_workbook, sheetName = "Dilution Summary")
#'
#' # Write to worksheet as an Excel Table
#' openxlsx::writeDataTable(wb = my_workbook, sheet = "Dilution Summary",
#'                          x = dilution_summary,
#'                          withFilter = TRUE,
#'                          bandedRows = FALSE
#'                         )
#'
#' # Conditional formatting can only be done
#' # after data is written to excel sheet
#' two_col_num_cond_format(workbook = my_workbook,sheet = "Dilution Summary",
#'                         dilution_summary = dilution_summary,
#'                         conditional_column = "r_corr",
#'                         threshold_value = "0.8",
#'                         pass_criteria = "above"
#'                        )
#' @rdname two_col_num_cond_format
#' @export
two_col_num_cond_format <- function(workbook, sheet,
                                    dilution_summary, conditional_column,
                                    threshold_value,
                                    pass_criteria = c("above", "below"),
                                    pass_equality = TRUE) {


  col_index <- which(colnames(dilution_summary) %in% conditional_column)

  pos_style <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  neg_style <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")

  pass_criteria <- strex::match_arg(pass_criteria)

  if (pass_criteria == "above" && pass_equality) {
    pos_rule <- paste0(">=", threshold_value)
    neg_rule <- paste0("<", threshold_value)
  } else if (pass_criteria == "above" && !pass_equality) {
    pos_rule <- paste0(">", threshold_value)
    neg_rule <- paste0("<=", threshold_value)
  } else if (pass_criteria == "below" && pass_equality) {
    pos_rule <- paste0("<=", threshold_value)
    neg_rule <- paste0(">", threshold_value)
  } else if (pass_criteria == "below" && !pass_equality) {
    pos_rule <- paste0("<", threshold_value)
    neg_rule <- paste0(">=", threshold_value)
  }

  if (length(col_index) != 0) {
    openxlsx::conditionalFormatting(workbook, sheet, cols = col_index,
                                    rows = 2:(nrow(dilution_summary) + 1),
                                    rule = pos_rule, style = pos_style,
                                    type = "expression")
    openxlsx::conditionalFormatting(workbook, sheet, cols = col_index,
                                    rows = 2:(nrow(dilution_summary) + 1),
                                    rule = neg_rule, style = neg_style,
                                    type = "expression")

  }


}

#' @title Create Excel Report
#'
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' @param file_name Name of the excel file
#' @param sheet_name Sheet name to output the results
#' in Excel, Default: 'Dilution Summary'
#' @param corrcoef_column A column in `dilution_summary` that holds the
#' correlation coefficient, Default: 'r_corr'
#' @param corrcoef_min_threshold The minimum threshold value of the curve's
#' correlation coefficient to pass being potentially linear.
#' A pass will colour the excel cell green and red otherwise.
#' Equality to the threshold is considered a pass, Default: 0.8
#' @param pra_column A column in `dilution_summary` that holds the
#' percent residual accuracy, Default: 'pra_linear'
#' @param pra_min_threshold The minimum threshold value of the curve's
#' percent residual accuracy to pass being potentially linear.
#' A pass will colour the excel cell green and red otherwise.
#' Equality to the threshold is considered a pass, Default: 80
#' @param mandel_p_val_column A column in `dilution_summary` that holds the
#' p value results for the Mandel's fitting test.
#' Default: 'mandel_p_val'
#' @param mandel_p_val_threshold The threshold value of the curve's
#' p value for the Mandel's fitting test to reject the hypothesis that
#' the quadratic model fits better than the linear model.
#' If the value is less than this value, the cell colour will be red.
#' Cell colour will be green if the p value is equal or over the threshold.
#' Default: 0.05
#' @param workflow1_column A column in `dilution_summary` that holds the
#' evaluation results of workflow 1, Default: 'wf1_group'
#' @param workflow2_column A column in `dilution_summary` that holds the
#' evaluation results of workflow 2, Default: 'wf2_group'
#' @param pass_criteria_words A character vector to indicate which words in
#' `workflow1_column` or `workflow2_column` would have its excel cell coloured
#' green and the rest to red.
#' Default: c("Good Linearity")
#' @param testing To indicate if we are running a test,
#' if so, no excel file is given out
#' @examples
#' transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
#'                      "Lipid2", "Lipid2", "Lipid2", "Lipid2")
#' dilution_batch <- c("B1", "B1", "B1", "B1",
#'                     "B2", "B2", "B2", "B2")
#' wf1_group <- c("Poor Linearity", "Good Linearity",
#'                "Poor Linearity", "Poor Linearity",
#'                "Poor Linearity", "Good Linearity",
#'                "Poor Linearity", "Poor Linearity")
#' wf2_group <- c("Saturation", "Good Linearity",
#'                "LOD", "Poor Linearity",
#'                "Saturation", "Good Linearity",
#'                "LOD", "Poor Linearity")
#' r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462,
#'             0.970618, 0.969348, 0.343838, 0.383552)
#' pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473,
#'                 72.91220, 72.36528, -233.05949, -172.13659)
#' mandel_p_val <- c(2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
#'                   3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02)
#' concavity <- c(-4133.501328, -4146.745747, -3.350942, -3.393617,
#'                0.3942824, 0.4012963, -19.9469621, -22.6144875)
#' dilution_summary <- data.frame(Transition_Name = transition_name,
#'                                Dilution_Batch = dilution_batch,
#'                                wf1_group = wf1_group, wf2_group = wf2_group,
#'                                r_corr = r_corr, pra_linear = pra_linear,
#'                                mandel_p_val = mandel_p_val,
#'                                concavity = concavity)
#' # Create an excel report, set testing = FALSE to output results
#' create_excel_report(dilution_summary, file_name = "dilution_summary.xlsx",
#'                     testing = TRUE)
#' @export
create_excel_report <- function(dilution_summary, file_name,
                                sheet_name = "Dilution Summary",
                                corrcoef_column = "r_corr",
                                corrcoef_min_threshold = 0.8,
                                pra_column = "pra_linear",
                                pra_min_threshold = 80,
                                mandel_p_val_column = "mandel_p_val",
                                mandel_p_val_threshold = 0.05,
                                workflow1_column = "wf1_group",
                                workflow2_column = "wf2_group",
                                pass_criteria_words = c("Good Linearity"),
                                testing = FALSE
                                ) {

  ## Excel setup ##

  # Create a new workbook
  my_workbook <- openxlsx::createWorkbook()

  # Create a new worksheet
  openxlsx::addWorksheet(wb = my_workbook, sheetName = sheet_name)

  # Create numeric style based on column name
  dilution_summary %>%
    mark_near_zero_columns() %>%
    get_column_number_style(workbook = my_workbook,
                            sheet = sheet_name)

  # Set the width of the columns to fit the column names
  # of the data. Taken from
  # https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width

  font_size <- as.integer(openxlsx::getBaseFont(my_workbook)$size$val)
  openxlsx::setColWidths(wb = my_workbook, sheet = sheet_name,
                         cols = seq_len(ncol(dilution_summary)),
                         widths = nchar(colnames(dilution_summary)) + font_size
                         - 6
                         )

  # Write to worksheet as an Excel Table
  openxlsx::writeDataTable(wb = my_workbook, sheet = sheet_name,
                           x = dilution_summary,
                           withFilter = TRUE,
                           bandedRows = FALSE
                           )

  # Conditional formatting can only be done after data is written to excel sheet
  two_col_num_cond_format(workbook = my_workbook, sheet = sheet_name,
                          dilution_summary = dilution_summary,
                          conditional_column = corrcoef_column,
                          threshold_value =
                            as.character(corrcoef_min_threshold),
                          pass_criteria = "above"
  )
  two_col_num_cond_format(workbook = my_workbook, sheet = sheet_name,
                          dilution_summary = dilution_summary,
                          conditional_column = pra_column,
                          threshold_value = as.character(pra_min_threshold),
                          pass_criteria = "above"
  )
  two_col_num_cond_format(workbook = my_workbook, sheet = sheet_name,
                          dilution_summary = dilution_summary,
                          conditional_column = mandel_p_val_column,
                          threshold_value =
                            as.character(mandel_p_val_threshold),
                          pass_criteria = "above"
  )

  two_col_word_cond_format(workbook = my_workbook, sheet = sheet_name,
                           dilution_summary = dilution_summary,
                           conditional_column = workflow1_column,
                           pass_criteria_words = pass_criteria_words)
  two_col_word_cond_format(workbook = my_workbook, sheet = sheet_name,
                           dilution_summary = dilution_summary,
                           conditional_column = workflow2_column,
                           pass_criteria_words = pass_criteria_words)


  # Export to file if we are not testing
  if (!testing) {
    openxlsx::saveWorkbook(wb = my_workbook, file = file_name,
                           overwrite = TRUE)
  }

}
