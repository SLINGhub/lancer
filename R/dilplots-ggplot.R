#' @title Plot Dilution Summary tables for one group
#' @description Plot Dilution Summary tables for one group
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A gridtable object consisting of two tables. One from
#' `dilution_summary_char_table` and `dilution_summary_num_table`
#' @examples
#' wf1_group <- c("Poor Linearity")
#' wf2_group <- c("Saturation")
#' r_corr <- c(0.951956)
#' pra_linear <- c(65.78711)
#' mandel_p_val <- c(2.899006e-07)
#' concavity <- c(-4133.501328)
#' dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
#'                                     wf2_group = wf2_group,
#'                                     r_corr = r_corr,
#'                                     pra_linear = pra_linear,
#'                                     mandel_p_val = mandel_p_val,
#'                                     concavity = concavity)
#' table <- dilution_summary_group_table(dilution_summary_grp)
#' @rdname dilution_summary_group_table
#' @export
dilution_summary_group_table <- function(dilution_summary_grp) {
  dilution_char_data <- dilution_summary_char_table(dilution_summary_grp)
  dilution_num_data <- dilution_summary_num_table(dilution_summary_grp)

  if (is.null(dilution_char_data) && is.null(dilution_num_data)) {
    return(NULL)
  } else if (is.null(dilution_char_data)) {
    return(dilution_num_data)
  } else if (is.null(dilution_num_data)) {
    return(dilution_char_data)
  } else {
    p <- gridExtra::gtable_combine(dilution_char_data, dilution_num_data,
                                   along = 2)
    return(p)
  }
}

#' @title Plot Dilution Summary character table for one group
#' @description Plot Dilution Summary character table for one group
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A gridtable object consisting of one table. The first
#' column is the column names of `dilution_summary_grp` which
#' are characters or factors or logical. The second column is their
#' corresponding values. If there are no character/factor/logical
#' columns in `dilution_summary_grp`, NULL will be returned
#' @examples
#' wf1_group <- c("Poor Linearity")
#' wf2_group <- c("Saturation")
#' r_corr <- c(0.951956)
#' pra_linear <- c(65.78711)
#' mandel_p_val <- c(2.899006e-07)
#' concavity <- c(-4133.501328)
#' dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
#'                                     wf2_group = wf2_group,
#'                                     r_corr = r_corr,
#'                                     pra_linear = pra_linear,
#'                                     mandel_p_val = mandel_p_val,
#'                                     concavity = concavity)
#' table <- dilution_summary_char_table(dilution_summary_grp)
#' @rdname dilution_summary_char_table
#' @export
dilution_summary_char_table <- function(dilution_summary_grp) {

  dilution_char_data <- dilution_summary_grp %>%
    dplyr::select_if(function(col) is.character(col) |
                                   is.factor(col) |
                                   is.logical(col)
                     )

  remaining_cols <- length(colnames(dilution_char_data))

  if (remaining_cols < 1) {
    dilution_char_data <- NULL
  } else {
    dilution_char_data <- dilution_char_data %>%
      dplyr::mutate_if(is.logical,as.character) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      gridExtra::tableGrob(rows = NULL, cols = NULL)
  }

  return(dilution_char_data)

}

#' @title Plot Dilution Summary numeric table for one group
#' @description Plot Dilution Summary numeric table for one group
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A gridtable object consisting of one table. The first
#' column is the column names of `dilution_summary_grp` which
#' are numeric. The second column is their
#' corresponding values. If there are numeric columns
#' in `dilution_summary_grp`, NULL will be returned
#' @examples
#' wf1_group <- c("Poor Linearity")
#' wf2_group <- c("Saturation")
#' r_corr <- c(0.951956)
#' pra_linear <- c(65.78711)
#' mandel_p_val <- c(2.899006e-07)
#' concavity <- c(-4133.501328)
#' dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
#'                                     wf2_group = wf2_group,
#'                                     r_corr = r_corr,
#'                                     pra_linear = pra_linear,
#'                                     mandel_p_val = mandel_p_val,
#'                                     concavity = concavity)
#' table <- dilution_summary_num_table(dilution_summary_grp)
#' @rdname dilution_summary_num_table
#' @export
dilution_summary_num_table <- function(dilution_summary_grp) {

  dilution_num_data <- dilution_summary_grp %>%
    mark_near_zero_columns() %>%
    dplyr::select_if(function(col) is.numeric(col) |
                       class(col) == "scientific"
    )

  remaining_cols <- length(colnames(dilution_num_data))

  if (remaining_cols < 1) {
    dilution_num_data <- NULL
  } else {
    dilution_num_data <- dilution_num_data %>%
    dplyr::mutate_if(function(col) class(col) == "scientific",
                     formatC, format = "e", digits = 2) %>%
    dplyr::mutate_if(is.numeric,
                     formatC, format = "f", digits = 2) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    gridExtra::tableGrob(rows = NULL, cols = NULL)
  }

  return(dilution_num_data)

}


#' @title Plot Dilution Data using ggplot2
#' @description Plot Dilution Data using ggplot2
#' @param dilution_data A data frame or tibble containing dilution data
#' @param dilution_summary_grp A data frame or tibble containing
#' dilution summary data for one group
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
dilution_plot_ggplot <- function(dilution_data,
                                 dilution_summary_grp,
                                 title,
                                 dil_batch_var,
                                 conc_var, conc_var_units,
                                 conc_var_interval,
                                 signal_var,
                                 pal) {


  # Number of dilution batches
  no_of_dil_batch <- dilution_data %>%
    dplyr::pull(.data[[dil_batch_var]]) %>%
    unique() %>%
    length()

  # Name of dilution batch
  names_of_dil_batch <- dilution_data %>%
    dplyr::pull(.data[[dil_batch_var]]) %>%
    unique()

  # Filter the dilution palatte based on what batches are
  # in the dilution_data
  filtered_pal <- pal[which(names(pal) %in% names_of_dil_batch)]

  # Give an error if the palatte colour is not listed in
  # dilution_batch
  stopifnot(length(filtered_pal) > 0)

  reg_col_vec <- c("Lin" = "black", "Quad" = "red", "Lin Par" = "blue")

  # Drop values that are NA in signal_var
  dilution_data <- tidyr::drop_na(dilution_data, .data[[signal_var]])

  # Create the table
  tables <- dilution_summary_group_table(dilution_summary_grp)

  # Create an empty dilution plot first

  p <- ggplot2::ggplot(dilution_data) +
    ggplot2::aes(x = .data[[conc_var]],
                 y = .data[[signal_var]]
    )

  if (nrow(dilution_data) > 3) {

    # Get the points for the partial linear curve
    partial_conc_points <- dilution_data %>%
      dplyr::pull(.data$Dilution_Percent) %>%
      as.numeric() %>%
      sort() %>%
      unique()

    partial_conc_points <- partial_conc_points[1:ceiling(length(partial_conc_points)/2)]

    partial_dilution_Data <- dilution_data %>%
      dplyr::filter(.data[[conc_var]] %in% partial_conc_points)


    # Model the data
    linear_model <- create_dil_linear_model(dilution_data, conc_var, signal_var)
    quad_model <- create_dil_quad_model(dilution_data, conc_var, signal_var)
    partial_linear_model <- create_dil_linear_model(partial_dilution_Data,
                                                    conc_var, signal_var)

    dilution <- seq(min(dilution_data[[conc_var]]),
                    max(dilution_data[[conc_var]]),
                    length.out = 15)

    # Create the y values for the line
    y_lin_predict <- stats::predict(linear_model,
                                data.frame(Dilution_Percent = dilution))
    y_quad_predict <- stats::predict(quad_model,
                                     data.frame(Dilution_Percent = dilution))
    y_partial_lin_predict <- stats::predict(partial_linear_model,
                                     data.frame(Dilution_Percent = dilution))
    reg_data <- data.frame(dilution = dilution,
                           y_lin_predict = y_lin_predict,
                           y_quad_predict = y_quad_predict)

    # Clean the y value so that it is not too large or too small
    partial_reg_data <- data.frame(dilution = dilution,
                                   y_partial_lin_predict = y_partial_lin_predict)
    # partial_reg_data <- partial_reg_data %>%
    #   dplyr::mutate(y_partial_lin_predict =
    #                   dplyr::case_when(
    #                     .data$y_partial_lin_predict > max(dilution_data[[signal_var]]) ~ NA_real_,
    #                     .data$y_partial_lin_predict < min(dilution_data[[signal_var]]) ~ NA_real_,
    #                     TRUE ~ .data$y_partial_lin_predict
    #                   )
    #                 ) %>%
    #   dplyr::filter(!is.na(.data$y_partial_lin_predict))

    # Get maximum concentration value for scaling
    max_conc <- max(dilution_data[[conc_var]], na.rm = TRUE)

    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(colour = factor(.data[[dil_batch_var]])
        ),
        size = 2,
      ) +
      ggplot2::geom_line(data = reg_data,
                         mapping = ggplot2::aes(x = dilution, y=y_lin_predict,
                                                colour = "Lin")
      ) +
      ggplot2::geom_line(data = reg_data,
                         mapping = ggplot2::aes(x = dilution, y=y_quad_predict,
                                                colour = "Quad")
      ) +
      ggplot2::geom_line(data = partial_reg_data,
                         mapping = ggplot2::aes(x = dilution, y=y_partial_lin_predict,
                                                colour = "Lin Par")
      ) +
      # ggplot2::geom_smooth(method = "lm", formula = y ~ x,
      #                      size = 0.5, se = FALSE,
      #                      ggplot2::aes(colour = "Linear")) +
      # ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2),
      #                      size = 0.5, se = FALSE,
      #                      ggplot2::aes(colour = "Quadratic")) +
      ggplot2::scale_colour_manual(values = c(filtered_pal,reg_col_vec),
                                   labels = names(c(reg_col_vec,filtered_pal)),
                                   guide = ggplot2::guide_legend(
                                     override.aes = list(
                                       linetype = c(rep("solid", length(reg_col_vec)),
                                                    rep("blank", no_of_dil_batch)
                                       ),
                                       shape = c(rep(NA, length(reg_col_vec)),
                                                 rep(16,no_of_dil_batch)
                                       ),
                                       colour = c(reg_col_vec,filtered_pal)
                                       )
                                     )
                                   ) +
      ggplot2::scale_x_continuous(breaks = seq(0, max_conc,
                                               by = conc_var_interval),
                                  labels = scales::number) +
      ggplot2::scale_y_continuous(labels = scales::scientific) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "top",
        axis.title.y = ggplot2::element_text(angle = 0,
                                             vjust = 1)
      ) +
      ggplot2::labs(title = title,
                    x = paste0(conc_var, " (",  conc_var_units, ")"),
                    y = signal_var)


  }

  if (!is.null(tables)) {
    #p <- ggarrange(p, tables, ncol = 2, nrow = 1, widths = (c(2, 1)))
    p <- patchwork::wrap_plots(p, tables, ncol = 2, nrow = 1)
  }

  return(p)

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
#' @param conc_var_interval Distance between two tick labels
#' in the dilution plot,
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
  if (is.null(dilution_summary)) {
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

  # Create a gg table for each group
  dilution_summary <- dilution_summary %>%
    dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::rename(summary = .data$data)

  # Add a dummy Dilution Batch Name, to be used for plotting later
  # Nest the data to be used for plotting
  dilution_table <- dilution_table %>%
    dplyr::mutate(Dilution_Batch_Name = .data[[dil_batch_var]]) %>%
    dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
    dplyr::relocate(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dilution_summary, by = grouping_variable)

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
    dplyr::mutate(panel = purrr::pmap(list(.data$data,
                                           .data$summary,
                                           .data$title),
                                      dilution_plot_ggplot,
                                      dil_batch_var = "Dilution_Batch_Name",
                                      conc_var = conc_var,
                                      conc_var_units = conc_var_units,
                                      conc_var_interval = conc_var_interval,
                                      signal_var = signal_var,
                                      pal = pal)
                  )

  return(ggplot_table)
}
