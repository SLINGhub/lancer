#' @title Validate Dilution Annotation
#' @description Validate Dilution Annotation
#' `r lifecycle::badge("deprecated")`
#'
#' `validate_dilution_annot` was renamed to
#' `validate_curve_annot`.
#' @keywords internal
#' @export
validate_dilution_annot <- function(dilution_annot,
                                    needed_column = c("Sample_Name")) {

  lifecycle::deprecate_warn(when = "0.0.6.9000",
                            what = "validate_dilution_annot()",
                            with = "validate_curve_annot()")

  validate_curve_annot(curve_annot = dilution_annot,
                       needed_column = needed_column)
}

#' @title Validate Curve Annotation
#' @description Validate Curve Annotation
#' @param curve_annot A data frame or tibble that contains information
#' of your curve batch. A column with sample name should be present.
#' @param needed_column A vector consisting of needed column names that
#' must be found in `curve_annot`,
#' Default: c("Sample_Name")
#' @return An error if the things in `needed_column`
#' is not found in the Dilution Data
#' @examples
#' dilution_percent <- c(
#'   10, 20, 40, 60, 80, 100,
#'   10, 20, 40, 60, 80, 100
#' )
#'
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a", "Sample_040a",
#'   "Sample_060a", "Sample_080a", "Sample_100a",
#'   "Sample_010b", "Sample_020b", "Sample_040b",
#'   "Sample_060b", "Sample_080b", "Sample_100b"
#' )
#'
#' curve_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent
#' )
#'
#' validate_curve_annot(
#'   curve_annot = curve_annot,
#'   needed_column = c("Sample_Name")
#' )
#'
#' @rdname validate_curve_annot
#' @export
validate_curve_annot <- function(curve_annot,
                                 needed_column = c("Sample_Name")) {

  # Check if things in needed_column are in curve_annot
  assertable::assert_colnames(curve_annot, needed_column,
    only_colnames = FALSE, quiet = TRUE
  )
}

#' @title Validate Lipid Data Wide
#' @description Validate Lipid Data in wide form
#' `r lifecycle::badge("deprecated")`
#'
#' `validate_lipid_data_wide` was renamed to
#' `validate_curve_data_wide`.
#' @keywords internal
#' @export
validate_lipid_data_wide <- function(lipid_data_wide,
                                     needed_column = c("Sample_Name")) {

  lifecycle::deprecate_warn(when = "0.0.6.9000",
                            what = "validate_lipid_data_wide()",
                            with = "validate_curve_data_wide()")

  validate_curve_data_wide(curve_data_wide = lipid_data_wide,
                           needed_column = needed_column)

}

#' @title Validate Curve Data Wide
#' @description Validate Curve Data in wide form
#' @param curve_data_wide A wide format data frame or tibble
#' that contains the Sample Name usually at the first column followed
#' by different curves. Each curve is meant to provide
#' a `signal_var` for each sample.
#' @param needed_column A vector consisting of needed column names that
#' must be found in `curve_data_wide`,
#' Default: c("Sample_Name")
#' @return An error if the things in `needed_column`
#' is not found in the Lipid Data in wide form
#' @examples
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a", "Sample_040a",
#'   "Sample_060a", "Sample_080a", "Sample_100a",
#'   "Sample_010b", "Sample_020b", "Sample_040b",
#'   "Sample_060b", "Sample_080b", "Sample_100b"
#' )
#'
#' lipid1_area <- c(
#'   22561, 31178, 39981, 48390, 52171, 53410,
#'   32561, 41178, 49981, 58390, 62171, 63410
#' )
#'
#' lipid2_area <- c(
#'   2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'   2300075, 4137350, 7021062, 8923063, 9289742, 11366710
#' )
#'
#' curve_data_wide <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Lipid1 = lipid1_area,
#'   Lipid2 = lipid2_area
#' )
#'
#' validate_curve_data_wide(
#'   curve_data_wide = curve_data_wide,
#'   needed_column = c("Sample_Name")
#' )
#'
#' @rdname validate_curve_data_wide
#' @export
validate_curve_data_wide <- function(curve_data_wide,
                                     needed_column = c("Sample_Name")) {

  # Check if things in needed_column are in curve_data_wide
  assertable::assert_colnames(curve_data_wide, needed_column,
    only_colnames = FALSE, quiet = TRUE
  )
}

#' @title Validate Dilution Table
#' @description Validate Dilution Table
#' `r lifecycle::badge("deprecated")`
#'
#' `validate_dilution_table` was renamed to
#' `validate_curve_table`.
#' @keywords internal
#' @export
validate_dilution_table <- function(dilution_table,
                                    needed_column = c(
                                      "Transition_Name",
                                      "Dilution_Batch_Name",
                                      "Dilution_Percent",
                                      "Area"
                                    )) {

  lifecycle::deprecate_warn(when = "0.0.6.9000",
                            what = "validate_dilution_table()",
                            with = "validate_curve_table()")

  validate_curve_table(curve_table = dilution_table,
                       needed_column = needed_column)

}

#' @title Validate Curve Table
#' @description Validate Curve Table
#' @param curve_table A curve table containing data of curve points,
#' variables to group the curve points together and some additional
#' annotations about the curve points
#' @param needed_column A vector consisting of needed column names that
#' must be found in `curve_table`,
#' Default: c("Transition_Name", "Dilution_Batch_Name",
#'            "Dilution_Percent", "Area")
#' @return An error if the things in `needed_column`
#' is not found in `curve_table`
#' @examples
#' dilution_percent <- c(
#'   10, 20, 40, 60, 80, 100,
#'   10, 20, 40, 60, 80, 100
#' )
#'
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' lipid1_area <- c(
#'   22561, 31178, 39981, 48390, 52171, 53410,
#'   32561, 41178, 49981, 58390, 62171, 63410
#' )
#'
#' transition_name <- c(
#'   "Lipid1", "Lipid1", "Lipid1",
#'   "Lipid1", "Lipid1", "Lipid1",
#'   "Lipid1", "Lipid1", "Lipid1",
#'   "Lipid1", "Lipid1", "Lipid1"
#' )
#'
#' curve_table <- tibble::tibble(
#'   Transition_Name = transition_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Area = lipid1_area,
#'   Dilution_Percent = dilution_percent
#' )
#'
#' validate_curve_table(
#'   curve_table = curve_table,
#'   needed_column = c(
#'     "Transition_Name",
#'     "Dilution_Batch_Name",
#'     "Dilution_Percent",
#'     "Area"
#'   )
#' )
#'
#' @rdname validate_curve_table
#' @export
validate_curve_table <- function(curve_table,
                                 needed_column =
                                   c("Transition_Name",
                                     "Dilution_Batch_Name",
                                     "Dilution_Percent",
                                     "Area")) {

  # Check if things in needed_column are in curve_table
  assertable::assert_colnames(curve_table, needed_column,
    only_colnames = FALSE, quiet = TRUE
  )
}

#' @title Create Dilution Table
#' @description Create Dilution Table from two data frame or tibble inputs
#' `r lifecycle::badge("deprecated")`
#'
#' `create_dilution_table` was renamed to
#' `create_curve_table`.
#' @keywords internal
#' @export
create_dilution_table <- function(dilution_annot, lipid_data_wide,
                                  common_column = c("Sample_Name"),
                                  signal_var = "Area",
                                  column_group = "Transition_Name") {

  lifecycle::deprecate_warn(when = "0.0.6.9000",
                            what = "create_dilution_table()",
                            with = "create_curve_table()")

  create_curve_table(curve_annot = dilution_annot,
                     curve_data_wide = lipid_data_wide,
                     common_column = common_column,
                     signal_var = signal_var,
                     column_group = column_group)

}

#' @title Create Curve Table
#' @description Create Curve Table from two data frame or tibble inputs
#' `curve_annot` and `curve_data_wide`. Both input must have at least one
#' common column. The default common column names is `Sample_Name`.
#' @param curve_annot A data frame or tibble that contains information
#' of your curve batch. A column with sample name should be present.
#' @param curve_data_wide A wide format data frame or tibble
#' that contains the Sample Name usually at the first column followed
#' by different curves. Each curve is meant to provide
#' a `signal_var` for each sample.
#' @param common_column A vector consisting of common column names that
#' must be found in both `curve_annot` and `curve_data_wide`,
#' Default: c("Sample_Name")
#' @param signal_var Value provided in `curve_data_wide` for each
#' curve which will be used as a column name when the merged data
#' is converted to a long format,
#' Default: 'Area'
#' @param column_group Value used to group the curves in
#' `curve_data_wide` when the merged data is converted to a long format,
#' Default: 'Transition_Name'
#' @return A data frame or tibble in long format
#' @details We first merge the `curve_annot` with the `curve_data_wide`
#' via one or more common columns. Next we convert the data from a wide to
#' a long format. Merging with a Sample Annotation data can be done after this
#' @examples
#' # Data Creation
#' dilution_percent <- c(
#'   10, 20, 40, 60, 80, 100,
#'   10, 20, 40, 60, 80, 100
#' )
#'
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a", "Sample_040a",
#'   "Sample_060a", "Sample_080a", "Sample_100a",
#'   "Sample_010b", "Sample_020b", "Sample_040b",
#'   "Sample_060b", "Sample_080b", "Sample_100b"
#' )
#'
#' lipid1_area <- c(
#'   22561, 31178, 39981, 48390, 52171, 53410,
#'   32561, 41178, 49981, 58390, 62171, 63410
#' )
#'
#' lipid2_area <- c(
#'   2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'   2300075, 4137350, 7021062, 8923063, 9289742, 11366710
#' )
#'
#' curve_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Lipid1 = lipid1_area,
#'   Lipid2 = lipid2_area
#' )
#'
#' # Create dilution table
#' curve_table <- create_curve_table(
#'   curve_annot = curve_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Area",
#'   column_group = "Transition_Name"
#' )
#'
#' print(curve_table, width = 100)
#'
#' @rdname create_curve_table
#' @export
create_curve_table <- function(curve_annot, curve_data_wide,
                               common_column = c("Sample_Name"),
                               signal_var = "Area",
                               column_group = "Transition_Name") {

  # Check if curve_annot and curve_data_wide is valid
  validate_curve_annot(curve_annot, common_column)
  validate_curve_data_wide(curve_data_wide, common_column)

  # Merge the two data together and make it long
  curve_table <- dplyr::inner_join(curve_annot, curve_data_wide,
    by = common_column
  ) %>%
    tidyr::pivot_longer(-dplyr::any_of(colnames(curve_annot)),
      names_to = column_group, values_to = signal_var
    )

  return(curve_table)
}


#' @title Summarize Dilution Table
#' @description Get the summary statistics of each group from
#' the dilution table
#' `r lifecycle::badge("deprecated")`
#'
#' `summarise_dilution_table` was renamed to
#' `summarise_curve_table`.
#' @keywords internal
#' @export
summarise_dilution_table <- function(dilution_table,
                                     grouping_variable = c(
                                       "Transition_Name",
                                       "Dilution_Batch_Name"
                                     ),
                                     conc_var = "Dilution_Percent",
                                     signal_var = "Area") {

  lifecycle::deprecate_warn(when = "0.0.6.9000",
                            what = "summarise_dilution_table()",
                            with = "summarise_curve_table()")

  summarise_curve_table(curve_table = dilution_table,
                        grouping_variable = grouping_variable,
                        conc_var = conc_var,
                        signal_var = signal_var)

}


#' @title Summarize Curve Table
#' @description Get the summary statistics of each group from
#' the curve table
#' @param curve_table Output given from the function [create_curve_table()]
#' It is in long table format with columns indicating at least the
#' curve name, the concentration and signal. Other columns may be
#' present if it is used to group the curves together
#' @param conc_var Column name in `curve_table` to indicate concentration
#' Default: 'Dilution_Percent'
#' @param signal_var Column name in `curve_table` to indicate signal
#' Default: 'Area'
#' @param grouping_variable A character vector of
#' column names in `curve_table`to indicate how each curve
#' should be grouped by.
#' @return A curve summary table output from the function
#' [summarise_curve_data()] for each group
#' @details The function first perform `tidyr::nest` on `curve_table`
#' based on the `grouping_variable` to organise the dilution curve data
#' for each group. Next for each group, the function
#' [summarise_curve_data()] is used to get the summary statistics.
#' @examples
#' # Data Creation
#' dilution_percent <- c(
#'   10, 20, 40, 60, 80, 100,
#'   10, 20, 40, 60, 80, 100
#' )
#'
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a", "Sample_040a",
#'   "Sample_060a", "Sample_080a", "Sample_100a",
#'   "Sample_010b", "Sample_020b", "Sample_040b",
#'   "Sample_060b", "Sample_080b", "Sample_100b"
#' )
#'
#' lipid1_area <- c(
#'   22561, 31178, 39981, 48390, 52171, 53410,
#'   32561, 41178, 49981, 58390, 62171, 63410
#' )
#'
#' lipid2_area <- c(
#'   2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'   2300075, 4137350, 7021062, 8923063, 9289742, 11366710
#' )
#'
#' curve_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Lipid1 = lipid1_area,
#'   Lipid2 = lipid2_area
#' )
#'
#' # Create curve table
#' curve_table <- create_curve_table(
#'   curve_annot = curve_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Area",
#'   column_group = "Transition_Name"
#' )
#'
#' # Give summary result for each curve grouped by Transition_Name
#' # and Dilution_Batch_Name
#' curve_summary <- curve_table %>%
#'   summarise_dilution_table(
#'     grouping_variable = c(
#'       "Transition_Name",
#'       "Dilution_Batch_Name"
#'     ),
#'     conc_var = "Dilution_Percent",
#'     signal_var = "Area"
#'   )
#'
#' print(curve_summary, width = 100)
#'
#' @rdname summarise_curve_table
#' @export
summarise_curve_table <- function(curve_table,
                                  grouping_variable = c("Transition_Name",
                                                        "Dilution_Batch_Name"),
                                  conc_var = "Dilution_Percent",
                                  signal_var = "Area") {


  # Check if curve_table is valid with the relevant columns
  validate_curve_table(
    curve_table = curve_table,
    needed_column = c(
      grouping_variable,
      conc_var,
      signal_var
    )
  )

  # Group/Nest the curve data for each group
  # and do a curve summary for each of them
  curve_summary <- curve_table %>%
    dplyr::group_by_at(
      grouping_variable
      ) %>%
    dplyr::relocate(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dil_summary = purrr::map(.data$data,
      summarise_curve_data,
      conc_var = conc_var,
      signal_var = signal_var
    )) %>%
    tidyr::unnest(c(
      dplyr::any_of("dil_summary")
      )) %>%
    dplyr::select(!c(
      dplyr::any_of("data")
      ))

  return(curve_summary)
}
