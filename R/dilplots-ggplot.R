#' @title Plot Dilution Data using ggplot2
#' @description Plot Dilution Data using ggplot2
#' @param dilution_data A data frame or tibble containing dilution data
#' @param title Title to use for each dilution plot
#' @param dil_batch_var Column name in `dilution_data`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param conc_var_units Unit of measure for `conc_var`
#' @param conc_var_interval Distance between two tick labels
#' during `conc_var` plotting. Tick label will alway start from 0
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @param pal Input palette for each dilution batch group in `dil_batch_var`.
#' It is a named char vector where each value is a colour and
#' name is a dilution batch group given in `dil_batch_var`
#' @return Output ggplot dilution plot data of one dilution batch per transition
#' @rdname dilution_plot_ggplot
#' @export
dilution_plot_ggplot = function(dilution_data,
                                title,
                                dil_batch_var,
                                conc_var, conc_var_units,
                                conc_var_interval,
                                signal_var,
                                pal) {

  # Drop values that are NA in signal_var
  dilution_data <- tidyr::drop_na(dilution_data, .data[[signal_var]])

  # Get maximum concentration value for scaling
  max_conc <- max(dilution_data[[conc_var]], na.rm =TRUE)

  ggplot2::ggplot(dilution_data) +
    ggplot2::aes(x = .data[[conc_var]],
                 y = .data[[signal_var]]
                ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = factor(.data[[dil_batch_var]])),
      fill = "black",
      size = 2) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                         colour = "black",
                         size = 0.5, se = FALSE) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2),
                         colour = "red",
                         size = 0.5, se = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(0, max_conc,by = conc_var_interval),
                                labels = scales::number) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.y = ggplot2::element_text(angle = 0,
                                           vjust = 1)
      ) +
    ggplot2::labs(title = title,
                  x = paste0(conc_var, " (",  conc_var_units, ")"),
                  y = signal_var)
}


#' @title Create a ggplot table
#' @description Create a ggplot table suited for a pdf report
#' @param dilution_table Output given from the function `create_dilution_table`
#' It is in long table format with columns indicating at least the
#' lipid/transition name, the concentration and signal. Other columns may be
#' present if it is used to group the dilution curve together
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' but it can also be any generic data frame or tibble.
#' If there is no input given in this, the program will create one using
#' the function `summarise_dilution_table` and `evaluate_linearity` with
#' `grouping_variable`, `conc_var` and `signal_var` as inputs
#' Default: NULL
#' @param grouping_variable A character vector of
#' column names in `dilution_table`to indicate how each dilution curve
#' should be grouped by,
#' Default: c("Transition_Name", "Dilution_Batch")
#' @param dil_batch_var Column name in `dilution_table`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' Default: 'Dilution_Batch'
#' @param dil_batch_col A vector of colurs to be used for the dilution
#' batch group named given in `dil_batch_var`,
#' Default: c("#377eb8", "#4daf4a", "#9C27B0", "#BCAAA4", "#FF8A65", "#EFBBCF")
#' @param conc_var Column name in `dilution_table` to indicate concentration
#' Default: 'Dilution_Percent'
#' @param conc_var_units Unit of measure for `conc_var`, Default: '%'
#' @param conc_var_interval Distance between two tick labels in the dilution plot,
#' Default: 50
#' @param signal_var Column name in `dilution_table` to indicate signal
#' Default: 'Area'
#' @return A table with columns from `groupung variable`
#' and a new column `panel` created containing a ggplot dilution plot
#' in each row. This column is used to create the plot figure in the
#' pdf report.
#' @rdname create_ggplot_table
#' @export
create_ggplot_table <- function(dilution_table, dilution_summary = NULL,
                                grouping_variable = c("Transition_Name",
                                                      "Dilution_Batch"),
                                dil_batch_var = "Dilution_Batch",
                                dil_batch_col = c("#377eb8",
                                                  "#4daf4a",
                                                  "#9C27B0",
                                                  "#BCAAA4",
                                                  "#FF8A65",
                                                  "#EFBBCF"),
                                conc_var = "Dilution_Percent",
                                conc_var_units = "%",
                                conc_var_interval = 50,
                                signal_var = "Area") {

  # Check if dilution_table is valid with the relevant columns
  validate_dilution_table(dilution_table,
                          needed_column = c(grouping_variable,
                                            dil_batch_var,
                                            conc_var,
                                            signal_var)
  )

  # Try to create dilution summary if you do not have one.
  if(is.null(dilution_summary)) {
    dilution_summary <- dilution_table %>%
      summarise_dilution_table(grouping_variable = grouping_variable,
                               conc_var = conc_var,
                               signal_var = signal_var) %>%
      evaluate_linearity(grouping_variable = grouping_variable)
  }

  # Check if things in needed_column are in dilution_summary
  assertable::assert_colnames(dilution_summary, grouping_variable,
                              only_colnames = FALSE, quiet = TRUE)


  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_table %>%
    dplyr::pull(.data[[dil_batch_var]]) %>%
    unique() %>%
    as.character()

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    create_char_seq(output_length = length(dilution_batch_name)) %>%
    stats::setNames(dilution_batch_name)

  # Add a dummy Dilution Batch Name, to be used for plotting later
  # Nest the data to be used for plotting
  dilution_table <- dilution_table %>%
    dplyr::mutate(Dilution_Batch_Name = .data[[dil_batch_var]]) %>%
    dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
    dplyr::relocate(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  # Create a title name for each group
  #https://stackoverflow.com/questions/44613279/dplyr-concat-columns-stored-in-variable-mutate-and-non-standard-evaluation?rq=1
  dilution_table <- dilution_table %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = paste0(
      dplyr::across(dplyr::all_of(grouping_variable)),
      collapse = "_")) %>%
    dplyr::ungroup()

  # Start the plotting
  ggplot_table <- dilution_table %>%
    dplyr::mutate(panel =
                    purrr::map2(
                      .data$data,
                      .data$title,
                      dilution_plot_ggplot,
                      dil_batch_var = "Dilution_Batch_Name",
                      conc_var = conc_var,
                      conc_var_units = conc_var_units,
                      conc_var_interval = conc_var_interval,
                      signal_var = signal_var,
                      pal = pal
                    )
    )

  return(ggplot_table)

}

