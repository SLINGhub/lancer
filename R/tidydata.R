#' @title Validate Dilution Annotation
#' @description Validate Dilution Annotation
#' @param dilution_annot A data frame or tibble that contains information
#' of your dilution batch. A column with sample name should be present.
#' @param common_column A vector consisting of common column names that
#' must be found in `dilution_annot`,
#' Default: c("Sample_Name")
#' @return An error if the things in `common_column`
#' is not found in the Dilution Data
#' @examples
#' dilution_percent <- c(10, 20, 40, 60, 80, 100,
#'                       10, 20, 40, 60, 80, 100)
#' dilution_batch <- c("B1", "B1", "B1", "B1", "B1", "B1",
#'                     "B2", "B2", "B2", "B2", "B2", "B2")
#' sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
#'                  "Sample_060a", "Sample_080a", "Sample_100a",
#'                  "Sample_010b", "Sample_020b", "Sample_040b",
#'                  "Sample_060b", "Sample_080b", "Sample_100b")
#' dilution_annot <- tibble::tibble(Sample_Name = sample_name,
#'                                  Dilution_Batch = dilution_batch,
#'                                  Dilution_Percent = dilution_percent)
#' validate_dilution_annot(dilution_annot,
#'                         common_column = c("Sample_Name"))
#' @rdname validate_dilution_annot
#' @export
validate_dilution_annot <- function(dilution_annot,
                                    common_column = c("Sample_Name")) {

  #Check if things in common_column are in dilution_annot
  assertable::assert_colnames(dilution_annot, common_column,
                              only_colnames = FALSE, quiet = TRUE)


}



#' @title Validate Lipid Data Wide
#' @description Validate Lipid Data in wide form
#' @param lipid_data_wide A wide format data frame or tibble
#' that contains the Sample Name usually at the first column followed
#' by different Transitions/Lipids. Each Transition/Lipid is meant to provide
#' a `signal_var` for each sample.
#' @param common_column A vector consisting of common column names that
#' must be found in `lipid_data_wide`,
#' Default: c("Sample_Name")
#' @return An error if the things in `common_column`
#' is not found in the Lipid Data in wide form
#' @examples
#' sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
#'                  "Sample_060a", "Sample_080a", "Sample_100a",
#'                  "Sample_010b", "Sample_020b", "Sample_040b",
#'                  "Sample_060b", "Sample_080b", "Sample_100b")
#' lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410,
#'                  32561, 41178, 49981, 58390, 62171, 63410)
#' lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'                  2300075, 4137350, 7021062, 8923063, 9289742, 11366710)
#' lipid_data_wide <- tibble::tibble(Sample_Name = sample_name,
#'                                   Lipid1 = lipid1_area,
#'                                   Lipid2 = lipid2_area)
#' validate_lipid_data_wide(lipid_data_wide,
#'                          common_column =c("Sample_Name"))
#' @rdname validate_lipid_data_wide
#' @export
validate_lipid_data_wide <- function(lipid_data_wide,
                                     common_column = c("Sample_Name")) {

  #Check if things in common_column are in lipid_data_wide
  assertable::assert_colnames(lipid_data_wide, common_column,
                              only_colnames = FALSE, quiet = TRUE)


}


#' @title Create Dilution Table
#' @description Create Dilution Table from two data frame or tibble inputs
#' `dilution_annot` and `lipid_data_wide`. Both input must have at least one
#' common column. The default common column names is `Sample_Name`.
#' @param dilution_annot A data frame or tibble that contains information
#' of your dilution batch. A column with sample name should be present.
#' @param lipid_data_wide A wide format data frame or tibble
#' that contains the Sample Name usually at the first column followed
#' by different Transitions/Lipids. Each Transition/Lipid is meant to provide
#' a `signal_var` for each sample.
#' @param common_column A vector consisting of common column names that
#' must be found in both `dilution_annot` and `lipid_data_wide`,
#' Default: c("Sample_Name")
#' @param signal_var Value provided in `lipid_data_wide` for each transition
#' or lipid which will be used as a column name when the merged data
#' is converted to a long format,
#' Default: 'Area'
#' @param column_group Value used to group the transition column names in
#' `lipid_data_wide` when the merged data is converted to a long format,
#' Default: 'Transition_Name'
#' @return A data frame or tibble in long format
#' @details We first merge the `dilution_annot` with the `lipid_data_wide`
#' via one or more common columns. Next we convert the data from a wide to
#' a long format. Merging with a Sample Annotation data can be done after this
#' @examples
#' #Data Creation
#' dilution_percent <- c(10, 20, 40, 60, 80, 100,
#'                       10, 20, 40, 60, 80, 100)
#' dilution_batch <- c("B1", "B1", "B1", "B1", "B1", "B1",
#'                     "B2", "B2", "B2", "B2", "B2", "B2")
#' sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
#'                  "Sample_060a", "Sample_080a", "Sample_100a",
#'                  "Sample_010b", "Sample_020b", "Sample_040b",
#'                  "Sample_060b", "Sample_080b", "Sample_100b")
#' lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410,
#'                  32561, 41178, 49981, 58390, 62171, 63410)
#' lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'                  2300075, 4137350, 7021062, 8923063, 9289742, 11366710)
#' dilution_annot <- tibble::tibble(Sample_Name = sample_name,
#'                                  Dilution_Batch = dilution_batch,
#'                                  Dilution_Percent = dilution_percent)
#' lipid_data <- tibble::tibble(Sample_Name = sample_name,
#'                              Lipid1 = lipid1_area,
#'                              Lipid2 = lipid2_area)
#' #Create dilution table
#' dil_data <- create_dilution_table(dilution_annot,lipid_data,
#'                                   common_column = "Sample_Name",
#'                                   signal_var = "Area",
#'                                   column_group = "Transition_Name")
#' @rdname create_dilution_table
#' @export
create_dilution_table <- function(dilution_annot, lipid_data_wide,
                                  common_column = c("Sample_Name"),
                                  signal_var = "Area",
                                  column_group = "Transition_Name") {


  validate_dilution_annot(dilution_annot, common_column)
  validate_lipid_data_wide(lipid_data_wide, common_column)

  dilution_table <- dplyr::inner_join(dilution_annot, lipid_data_wide,
                                      by = common_column) %>%
    tidyr::pivot_longer(-tidyselect::any_of(colnames(dilution_annot)),
                        names_to = column_group, values_to = signal_var)

  return(dilution_table)

}

# group_dilution_table <- function(dilution_table,
#                                  grouping_variable = c("Transition_Name",
#                                                        "Dilution_Batch"),
#                                  conc_var = "Dilution_Percent",
#                                  signal_var = "Area") {
#
#
# }
