#' @title Validate Trellis Table
#' @description Validate trellis table to see if it can be used to create
#' `trelliscopejs` report
#' @param trellis_table The trellis table generated
#' by function [add_plotly_panel()]
#' @param grouping_variable A character vector of
#' column names in `trellis_table` to indicate how each dilution curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report. The function will check
#' if these columns are indeed conditional cognostics
#' Default: c("Transition_Name", "Dilution_Batch_Name")
#' @param panel_variable A column name in `trellis_table` to indicate
#' the list of plots to be used in the `trelliscopejs` report. This column
#' name must be fixed to the name `panel` and it must be of class
#' `trelliscope_panels`
#' Default: 'panel'
#' @details A valid trellis table must have a column named `panel` with
#' a list of plots and class `trelliscope_panels`, conditional cognostics
#' columns as indicated in `grouping_variable` and the rest of the columns
#' as common cognostics
#' @examples
#' sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
#'                  "Sample_060a", "Sample_080a", "Sample_100a",
#'                  "Sample_010b", "Sample_020b", "Sample_040b",
#'                  "Sample_060b", "Sample_080b", "Sample_100b")
#' lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410,
#'                  32561, 41178, 49981, 58390, 62171, 63410)
#' lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'                  2300075, 4137350, 7021062, 8923063, 9289742, 11366710)
#' lipid_data <- tibble::tibble(Sample_Name = sample_name,
#'                              Lipid1 = lipid1_area,
#'                              Lipid2 = lipid2_area)
#' dilution_percent <- c(10, 20, 40, 60, 80, 100,
#'                       10, 20, 40, 60, 80, 100)
#' dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1", "B1",
#'                          "B2", "B2", "B2", "B2", "B2", "B2")
#' dilution_annot <- tibble::tibble(Sample_Name = sample_name,
#'                                  Dilution_Batch_Name = dilution_batch_name,
#'                                  Dilution_Percent = dilution_percent)
#' # Create dilution table
#' dilution_table <- create_dilution_table(dilution_annot, lipid_data,
#'                                         common_column = "Sample_Name",
#'                                         signal_var = "Area",
#'                                         column_group = "Transition_Name"
#' )
#' # Create a trellis table without dilution summary
#' trellis_table_auto <- add_plotly_panel(dilution_table) %>%
#'    convert_to_cog(cog_df = NULL,
#'                   grouping_variable = c("Transition_Name",
#'                                         "Dilution_Batch_Name"),
#'                   panel_variable = "panel",
#'                   col_name_vec = "col_name_vec",
#'                   desc_vec = "desc_vec",
#'                   type_vec = "type_vec")
#'
#' # Check if trellis_table_auto is valid
#' validate_trellis_table(trellis_table_auto)
#' @rdname validate_trellis_table
#' @export
validate_trellis_table <- function(trellis_table,
                                   grouping_variable = c("Transition_Name",
                                                         "Dilution_Batch_Name"),
                                   panel_variable = "panel") {

  #Check if grouping_variable is a valid input
  grouping_variable <- grouping_variable[!is.na(grouping_variable)]
  if(length(grouping_variable) == 0) {
    stop("input grouping variable cannot be an empty vector or vector with NA")
  }

  important_variable <- c(grouping_variable, panel_variable) %>%
    unique()

  # Check if things in needed_column are in trellis_table
  assertable::assert_colnames(trellis_table,
                              important_variable,
                              only_colnames = FALSE, quiet = TRUE)

  non_grouping_variable <- trellis_table %>%
    dplyr::select(-dplyr::one_of(important_variable)) %>%
    colnames()


  # Check if panel_variable is valid
  trellis_column_class <- attributes(trellis_table[[panel_variable]])$class
  if (!("trelliscope_panels" %in% trellis_column_class)) {
    stop(paste("column panel is not of class 'trelliscope_panels' "))
  }

  # Check if grouping_variable are valid
  for (variable in grouping_variable) {

    trellis_column_class <- attributes(trellis_table[[variable]])$class
    trellis_column_group <- attributes(
      trellis_table[[variable]])$cog_attrs$group

    if (!("cog" %in% trellis_column_class)) {
      stop(paste("column", variable, "is not of class 'cog' "))
    }
    if ("condVar" != trellis_column_group) {
      stop(paste("column", variable, "is not of cogonostic group 'condVar' "))
    }

  }

  # Check if non_grouping_variable are valid
  for (variable in non_grouping_variable) {

    trellis_column_class <- attributes(trellis_table[[variable]])$class
    trellis_column_group <- attributes(
      trellis_table[[variable]])$cog_attrs$group

    if (!("cog" %in% trellis_column_class)) {
      stop(paste("column", variable, "is not of class 'cog' "))
    }
    if ("common" != trellis_column_group) {
      stop(paste("column", variable, "is not of cogonostic group 'common' "))
    }

  }

}

#' @title View `ggplot` or `plotly` Dilution Plots As Trellis
#' @description View `ggplot` or `plotly` dilution plots in a `trelliscope`
#' html report
#' @param trellis_table The trellis table generated
#' by function [add_ggplot_panel()] and [convert_to_cog()] or
#' [add_plotly_panel()] and [convert_to_cog()]
#' @param trellis_report_name A string to indicate the title
#' of the trellis report
#' Default: 'Dilution_Plot'
#' @param trellis_report_folder A string to indicate the output
#' folder of the plots found in `panel_variable`
#' Default: 'Dilution_Plot'
#' @param grouping_variable A character vector of
#' column names in `trellis_table` to indicate how each dilution curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report. The function will check
#' if these columns are indeed conditional cognostics
#' Default: c("Transition_Name", "Dilution_Batch_Name")
#' @param panel_variable A column name in `trellis_table` to indicate
#' the list of plots to be used in the `trelliscopejs` report. This column
#' name must be fixed to the name `panel` and it must be of class
#' `trelliscope_panels`
#' Default: 'panel'
#' @param trellis_additional_labels A character vector of columns names
#' in `trellis_table` to display in the trellis report in addition to the
#' conditional cognostics in the `grouping varaible`,
#' Default: c()
#' @param testing To indicate if we are running a test,
#' if so, no trellis report is given out
#' Default: FALSE
#' @examples
#' sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
#'                  "Sample_060a", "Sample_080a", "Sample_100a",
#'                  "Sample_010b", "Sample_020b", "Sample_040b",
#'                  "Sample_060b", "Sample_080b", "Sample_100b")
#' lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410,
#'                  32561, 41178, 49981, 58390, 62171, 63410)
#' lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'                  2300075, 4137350, 7021062, 8923063, 9289742, 11366710)
#' lipid_data <- tibble::tibble(Sample_Name = sample_name,
#'                              Lipid1 = lipid1_area,
#'                              Lipid2 = lipid2_area)
#' dilution_percent <- c(10, 20, 40, 60, 80, 100,
#'                       10, 20, 40, 60, 80, 100)
#' dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1", "B1",
#'                          "B2", "B2", "B2", "B2", "B2", "B2")
#' dilution_annot <- tibble::tibble(Sample_Name = sample_name,
#'                                  Dilution_Batch_Name = dilution_batch_name,
#'                                  Dilution_Percent = dilution_percent)
#' # Create dilution table
#' dilution_table <- create_dilution_table(dilution_annot, lipid_data,
#'                                         common_column = "Sample_Name",
#'                                         signal_var = "Area",
#'                                         column_group = "Transition_Name"
#' )
#' # Add a plotly panel without dilution summary
#' trellis_table_auto <- add_plotly_panel(dilution_table) %>%
#'    convert_to_cog(cog_df = NULL,
#'                  grouping_variable = c("Transition_Name",
#'                                        "Dilution_Batch_Name"),
#'                  panel_variable = "panel",
#'                  col_name_vec = "col_name_vec",
#'                  desc_vec = "desc_vec",
#'                  type_vec = "type_vec")
#'
#' # Create a trellis report, set testing = FALSE to output results
#' view_trellis_html(trellis_table_auto, testing = TRUE)
#' @rdname view_trellis_html
#' @export
view_trellis_html <- function(trellis_table,
                              trellis_report_name = "Dilution_Plot",
                              trellis_report_folder = "Dilution_Plot",
                              grouping_variable = c("Transition_Name",
                                                    "Dilution_Batch_Name"),
                              panel_variable = "panel",
                              trellis_additional_labels = c(),
                              testing = FALSE) {


  validate_trellis_table(trellis_table,
                         grouping_variable = grouping_variable,
                         panel_variable = panel_variable)


  assertable::assert_colnames(trellis_table,
                              trellis_additional_labels,
                              only_colnames = FALSE, quiet = TRUE)

  #Conditional cognostics must always be displayed as a label
  trellis_labels <- c(grouping_variable,
                      trellis_additional_labels) %>%
    unique()

  if(!testing) {
    suppressWarnings(trelliscopejs::trelliscope(trellis_table,
                                                name = trellis_report_name,
                                                panel_col = panel_variable,
                                                path = trellis_report_folder,
                                                state = list(
                                                  labels = trellis_labels
                                                ),
                                                nrow = 2,
                                                ncol = 2,
                                                height = 500, width = 1000,
                                                self_contained = FALSE,
                                                thumb = FALSE,
                                                auto_cog = FALSE)
    )
  }

}
