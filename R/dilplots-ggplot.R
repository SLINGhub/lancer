#' @title Plot Dilution Summary Character Table For One Group
#' @description
#' Plot dilution summary character table for one group or batch
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A `gridtable` object consisting of one table. The first
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
#' dilution_summary_grp <- data.frame(
#'   wf1_group = wf1_group,
#'   wf2_group = wf2_group,
#'   r_corr = r_corr,
#'   pra_linear = pra_linear,
#'   mandel_p_val = mandel_p_val,
#'   concavity = concavity
#' )
#' table <- plot_summary_table_char(dilution_summary_grp)
#' grid::grid.draw(table)
#'
#' # No character/factor/logical column case
#' dilution_summary_grp <- data.frame(r_corr = r_corr)
#' table <- plot_summary_table_char(dilution_summary_grp)
#' table
#' @rdname plot_summary_table_char
#' @export
plot_summary_table_char <- function(dilution_summary_grp) {
  dilution_char_data <- dilution_summary_grp %>%
    dplyr::select_if(
      function(col) {
        is.character(col) |
          is.factor(col) |
          is.logical(col)
      }
    )

  remaining_cols <- length(colnames(dilution_char_data))

  if (remaining_cols < 1) {
    dilution_char_data <- NULL
  } else {
    dilution_char_data <- dilution_char_data %>%
      dplyr::mutate_if(is.logical, as.character) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      gridExtra::tableGrob(rows = NULL, cols = NULL)
  }

  return(dilution_char_data)
}

#' @title Plot Dilution Summary Numeric Table For One Group
#' @description Plot dilution Summary numeric table for one group
#' or batch
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A `gridtable` object consisting of one table. The first
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
#' dilution_summary_grp <- data.frame(
#'   wf1_group = wf1_group,
#'   wf2_group = wf2_group,
#'   r_corr = r_corr,
#'   pra_linear = pra_linear,
#'   mandel_p_val = mandel_p_val,
#'   concavity = concavity
#' )
#' table <- plot_summary_table_num(dilution_summary_grp)
#' grid::grid.draw(table)
#'
#' # No numeric column case
#' dilution_summary_grp <- data.frame(wf2_group = wf2_group)
#' table <- plot_summary_table_num(dilution_summary_grp)
#' table
#' @rdname plot_summary_table_num
#' @export
plot_summary_table_num <- function(dilution_summary_grp) {
  dilution_num_data <- dilution_summary_grp %>%
    mark_near_zero_columns() %>%
    dplyr::select_if(
      function(col) {
        is.numeric(col) | class(col) == "scientific"
      }
    )

  remaining_cols <- length(colnames(dilution_num_data))

  if (remaining_cols < 1) {
    dilution_num_data <- NULL
  } else {
    dilution_num_data <- dilution_num_data %>%
      dplyr::mutate_if(function(col) class(col) == "scientific",
        formatC,
        format = "e", digits = 2
      ) %>%
      dplyr::mutate_if(is.numeric,
        formatC,
        format = "f", digits = 2
      ) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      gridExtra::tableGrob(rows = NULL, cols = NULL)
  }

  return(dilution_num_data)
}

#' @title Plot Dilution Summary Table For One Group
#' @description Plot dilution summary table for one group
#' @param dilution_summary_grp
#' A one row data frame or tibble containing dilution summary
#' @return A `gridtable` object consisting of two tables. One from
#' [plot_summary_table_char()] and [plot_summary_table_num()]
#' @examples
#' wf1_group <- c("Poor Linearity")
#' wf2_group <- c("Saturation")
#' r_corr <- c(0.951956)
#' pra_linear <- c(65.78711)
#' mandel_p_val <- c(2.899006e-07)
#' concavity <- c(-4133.501328)
#' dilution_summary_grp <- data.frame(
#'   wf1_group = wf1_group,
#'   wf2_group = wf2_group,
#'   r_corr = r_corr,
#'   pra_linear = pra_linear,
#'   mandel_p_val = mandel_p_val,
#'   concavity = concavity
#' )
#' table <- plot_summary_table(dilution_summary_grp)
#' grid::grid.draw(table)
#'
#' # No column case
#' dilution_summary_grp <- data.frame()
#' table <- plot_summary_table(dilution_summary_grp)
#' table
#' @rdname plot_summary_table
#' @export
plot_summary_table <- function(dilution_summary_grp) {
  if (is.null(dilution_summary_grp) || isTRUE(is.na(dilution_summary_grp))) {
    return(NULL)
  }
  dilution_char_data <- plot_summary_table_char(dilution_summary_grp)
  dilution_num_data <- plot_summary_table_num(dilution_summary_grp)

  if (is.null(dilution_char_data) && is.null(dilution_num_data)) {
    return(NULL)
  } else if (is.null(dilution_char_data)) {
    return(dilution_num_data)
  } else if (is.null(dilution_num_data)) {
    return(dilution_char_data)
  } else {
    p <- gridExtra::gtable_combine(dilution_char_data, dilution_num_data,
      along = 2
    )
    return(p)
  }
}

# regression_colour_vector

#' @title Create Regression Colour Vector
#' @description Internal function that create a named vector
#' to indicate which regression line has what colour
#' @param plot_first_half_lin_reg Decide if we plot an extra
#' regression line that best fits the first half
#' of `conc_var` dilution points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra
#' regression line that best fits the last half
#' of `conc_var` dilution points.
#' Default: FALSE
#' @return A named vector in which a linear regression is
#' named as "Lin" and is given the colour black. A
#' quadratic regression is named as "Quad" and is given
#' the colour red. A linear regression of the first
#' half of the dilution points is named as "Lin First Half"
#' and is given the colour blue. A linear regression of
#' the last half of the dilution points is
#' named as "Lin Last Half" and is given the colour purple.
#' @examples
#' # Data Creation
#' regression_colour_vector <- c(
#'   plot_first_half_lin_reg = TRUE,
#'   plot_last_half_lin_reg = TRUE
#' )
#'
#' regression_colour_vector
#'
#' @rdname create_reg_col_vec
#' @export
#' @keywords internal
create_reg_col_vec <- function(plot_first_half_lin_reg = FALSE,
                               plot_last_half_lin_reg = FALSE) {
  if (plot_first_half_lin_reg && plot_last_half_lin_reg) {
    reg_col_vec <- c(
      "Lin" = "black", "Quad" = "red",
      "Lin First Half" = "blue",
      "Lin Last Half" = "purple"
    )
  } else if (plot_first_half_lin_reg && !plot_last_half_lin_reg) {
    reg_col_vec <- c(
      "Lin" = "black", "Quad" = "red",
      "Lin First Half" = "blue"
    )
  } else if (!plot_first_half_lin_reg && plot_last_half_lin_reg) {
    reg_col_vec <- c(
      "Lin" = "black", "Quad" = "red",
      "Lin Last Half" = "purple"
    )
  } else {
    reg_col_vec <- c("Lin" = "black", "Quad" = "red")
  }

  return(reg_col_vec)
}


#' @title Plot Dilution Curve Using `ggplot2`
#' @description Plot dilution curve using `ggplot2`
#' @param dilution_data A data frame or tibble containing dilution data
#' @param dilution_summary_grp A data frame or tibble containing
#' dilution summary data for one group
#' @param title Title to use for each dilution plot
#' @param pal Input palette for each dilution batch group in `dil_batch_var`.
#' It is a named char vector where each value is a colour and
#' name is a dilution batch group given in `dil_batch_var`
#' @param dil_batch_var Column name in `dilution_table`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' Default: 'Dilution_Batch_Name'
#' @param conc_var Column name in `dilution_table` to indicate concentration
#' Default: 'Dilution_Percent'
#' @param conc_var_units Unit of measure for `conc_var`, Default: '%'
#' @param conc_var_interval Distance between two tick labels
#' in the dilution plot,
#' Default: 50
#' @param signal_var Column name in `dilution_table` to indicate signal
#' Default: 'Area'
#' @param plot_first_half_lin_reg Decide if we plot an extra
#' regression line that best fits the first half
#' of `conc_var` dilution points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra
#' regression line that best fits the last half
#' of `conc_var` dilution points.
#' Default: FALSE
#' @return Output `ggplot` dilution plot data of one dilution
#' batch per transition
#' @rdname plot_curve_ggplot
#' @examples
#' # Data Creation
#' dilution_percent <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a"
#' )
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1"
#' )
#' transition_name <- c(
#'   "Lipid1", "Lipid1", "Lipid1", "Lipid1",
#'   "Lipid1", "Lipid1", "Lipid1", "Lipid1",
#'   "Lipid1", "Lipid1", "Lipid1"
#' )
#' lipid1_area_saturated <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060
#' )
#'
#' dilution_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent,
#'   Transition_Name = transition_name,
#'   Area = lipid1_area_saturated,
#' )
#'
#' grouping_variable <- c("Transition_Name", "Dilution_Batch_Name")
#'
#' # Get the dilution batch name from dilution_table
#' dilution_batch_name <- dilution_batch_name %>%
#'   unique() %>%
#'   as.character()
#'
#' dil_batch_col <- c("#377eb8")
#'
#' # Create palette for each dilution batch for plotting
#' pal <- dil_batch_col %>%
#'   stats::setNames(dilution_batch_name)
#'
#'
#' # Create dilution statistical summary
#' dilution_summary_grp <- dilution_data %>%
#'   summarise_dilution_table(
#'     grouping_variable = grouping_variable,
#'     conc_var = "Dilution_Percent",
#'     signal_var = "Area"
#'   ) %>%
#'   evaluate_linearity(grouping_variable = grouping_variable) %>%
#'   dplyr::select(-c(dplyr::all_of(grouping_variable)))
#'
#'
#' # Create the ggplot
#' p <- plot_curve_ggplot(dilution_data,
#'   dilution_summary_grp = dilution_summary_grp,
#'   title = "Lipid_Saturated",
#'   pal = pal,
#'   dil_batch_var = "Dilution_Batch_Name",
#'   conc_var = "Dilution_Percent",
#'   conc_var_units = "%",
#'   conc_var_interval = 50,
#'   signal_var = "Area"
#' )
#' p
#' @export
plot_curve_ggplot <- function(dilution_data,
                              dilution_summary_grp,
                              title,
                              pal,
                              dil_batch_var = "Dilution_Batch_Name",
                              conc_var = "Dilution_Percent",
                              conc_var_units = "%",
                              conc_var_interval = 50,
                              signal_var = "Area",
                              plot_first_half_lin_reg = FALSE,
                              plot_last_half_lin_reg = FALSE) {


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
  # dilution_batch_name
  stopifnot(length(filtered_pal) > 0)

  # Get conc_vector before we drop the rows
  conc_vector <- dilution_data[[conc_var]]

  # Drop values that are NA in signal_var
  dilution_data <- dilution_data %>%
    tidyr::drop_na(dplyr::all_of(signal_var))

  # Named vector to represent the colours of the regression lines
  reg_col_vec <- NA

  # Create the table
  tables <- plot_summary_table(dilution_summary_grp)

  # Create an empty dilution plot first
  p <- ggplot2::ggplot(dilution_data) +
    ggplot2::aes(
      x = .data[[conc_var]],
      y = .data[[signal_var]]
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(colour = factor(.data[[dil_batch_var]])),
      size = 5
    )

  if (nrow(dilution_data) > 3) {

    # When we need to plot a horizontal line
    if (stats::sd(dilution_data[[signal_var]]) == 0) {
      reg_col_vec <- c("Lin" = "black")

      min_x <- min(dilution_data[[conc_var]], na.rm = TRUE)
      max_x <- max(dilution_data[[conc_var]], na.rm = TRUE)
      cont_y <- unique(dilution_data[[signal_var]])

      p <- p +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = min_x, xend = max_x,
            y = cont_y, yend = cont_y,
            colour = "Lin"
          )
        )
    } else if (stats::sd(dilution_data[[conc_var]]) == 0) {
      # When we need to plot a vertical line

      reg_col_vec <- c("Lin" = "black")

      min_y <- min(dilution_data[[signal_var]], na.rm = TRUE)
      max_y <- max(dilution_data[[signal_var]], na.rm = TRUE)
      cont_x <- unique(dilution_data[[conc_var]])

      p <- p +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = cont_x, xend = cont_x,
            y = min_y, yend = max_y,
            colour = "Lin"
          )
        )
    } else {
      reg_col_vec <- create_reg_col_vec(
        plot_first_half_lin_reg = plot_first_half_lin_reg,
        plot_last_half_lin_reg = plot_last_half_lin_reg
      )

      # Model the data
      linear_model <- create_linear_model(dilution_data, conc_var, signal_var)
      quad_model <- create_quad_model(dilution_data, conc_var, signal_var)

      dilution <- seq(min(dilution_data[[conc_var]], na.rm = TRUE),
        max(dilution_data[[conc_var]], na.rm = TRUE),
        length.out = 15
      )

      # Create the y values for the line
      y_lin_predict <- stats::predict(
        linear_model,
        tibble::tibble(!!conc_var := dilution)
      )
      y_quad_predict <- stats::predict(
        quad_model,
        tibble::tibble(!!conc_var := dilution)
      )
      reg_data <- data.frame(
        dilution = dilution,
        y_lin_predict = y_lin_predict,
        y_quad_predict = y_quad_predict
      )

      # Add the regression lines
      p <- p +
        ggplot2::geom_line(
          data = reg_data,
          mapping = ggplot2::aes(
            x = dilution, y = y_lin_predict,
            colour = "Lin"
          )
        ) +
        ggplot2::geom_line(
          data = reg_data,
          mapping = ggplot2::aes(
            x = dilution, y = y_quad_predict,
            colour = "Quad"
          )
        )


      if (plot_first_half_lin_reg) {

        # Get the points for the partial linear curve
        partial_conc_points <- dilution_data %>%
          dplyr::pull(.data[[conc_var]]) %>%
          as.numeric() %>%
          sort() %>%
          unique()

        first_half_index <- 1:ceiling(length(partial_conc_points) / 2)
        partial_conc_points <- partial_conc_points[first_half_index]

        partial_dilution_data <- dilution_data %>%
          dplyr::filter(.data[[conc_var]] %in% partial_conc_points)


        # Create the partial model
        partial_linear_model <- create_linear_model(
          partial_dilution_data,
          conc_var, signal_var
        )

        y_partial_lin_predict <- stats::predict(
          partial_linear_model,
          tibble::tibble(!!conc_var := dilution)
        )

        partial_reg_data <- data.frame(
          dilution = dilution,
          y_partial_lin_predict = y_partial_lin_predict
        )

        # Plot the half regression line
        p <- p +
          ggplot2::geom_line(
            data = partial_reg_data,
            mapping = ggplot2::aes(
              x = dilution,
              y = y_partial_lin_predict,
              colour = "Lin First Half"
            )
          )
      }

      if (plot_last_half_lin_reg) {

        # Get the points for the partial linear curve
        partial_conc_points <- dilution_data %>%
          dplyr::pull(.data[[conc_var]]) %>%
          as.numeric() %>%
          sort() %>%
          unique()

        last_half_index <-
          ceiling(length(partial_conc_points) / 2):length(partial_conc_points)
        partial_conc_points <- partial_conc_points[last_half_index]

        partial_dilution_data <- dilution_data %>%
          dplyr::filter(.data[[conc_var]] %in% partial_conc_points)


        # Create the partial model
        partial_linear_model <- create_linear_model(
          partial_dilution_data,
          conc_var, signal_var
        )

        y_partial_lin_predict <- stats::predict(
          partial_linear_model,
          tibble::tibble(!!conc_var := dilution)
        )

        partial_reg_data <- data.frame(
          dilution = dilution,
          y_partial_lin_predict = y_partial_lin_predict
        )

        # Plot the half regression line
        p <- p +
          ggplot2::geom_line(
            data = partial_reg_data,
            mapping = ggplot2::aes(
              x = dilution,
              y = y_partial_lin_predict,
              colour = "Lin Last Half"
            )
          )
      }
    }
  }

  # Get maximum concentration value for scaling
  if (nrow(dilution_data) == 0) {
    conc_vector <- conc_vector[!is.na(conc_vector)]
    max_conc <- ifelse(length(conc_vector) == 0,
      0, max(conc_vector, na.rm = TRUE)
    )
  } else {
    max_conc <- max(dilution_data[[conc_var]], na.rm = TRUE)
  }

  conc_tick_points <- seq(0, max_conc, by = conc_var_interval)

  # If conc_var_units is empty, do not add brackets
  x_title <- conc_var
  if (conc_var_units != "") {
    x_title <- paste0(conc_var, " (", conc_var_units, ")")
  }

  # Create the layout for legend, colours, axis
  legend_nrow <- 1
  if (plot_first_half_lin_reg && plot_last_half_lin_reg) {
    legend_nrow <- 2
  }

  p <- p +
    ggplot2::scale_colour_manual(
      values = c(filtered_pal, reg_col_vec),
      labels = names(c(reg_col_vec, filtered_pal)),
      guide = ggplot2::guide_legend(
        override.aes = list(
          linetype = c(
            rep("solid", length(reg_col_vec)),
            rep("blank", no_of_dil_batch)
          ),
          shape = c(
            rep(NA, length(reg_col_vec)),
            rep(16, no_of_dil_batch)
          ),
          colour = c(reg_col_vec, filtered_pal)
        ),
        nrow = legend_nrow
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = conc_tick_points,
      labels = scales::number
    ) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      axis.title.y = ggplot2::element_text(
        angle = 0,
        vjust = 1
      ),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = signal_var
    )


  if (!is.null(tables)) {
    p <- patchwork::wrap_plots(p, tables, ncol = 2, nrow = 1)
  }

  return(p)
}


#' @title Add A `ggplot` Panel Column
#' @description Create a column which contains a list of `ggplot`
#' suited for a pdf report
#' @param dilution_table Output given from
#' the function [create_dilution_table()]
#' It is in long table format with columns indicating at least the
#' lipid/transition name, the concentration and signal. Other columns may be
#' present if it is used to group the dilution curve together
#' @param dilution_summary The summary table generated
#' by function [summarise_dilution_table()] and/or
#' [evaluate_linearity()] but it can also be any generic
#' data frame or tibble.
#' If there is no input given in this, the program will create one using
#' the function [summarise_dilution_table()] and [evaluate_linearity()]
#' with `grouping_variable`, `conc_var` and `signal_var` as inputs
#' Default: NULL
#' @param grouping_variable A character vector of
#' column names in `dilution_table`to indicate how each dilution curve
#' should be grouped by,
#' Default: c("Transition_Name", "Dilution_Batch_Name")
#' @param dil_batch_var Column name in `dilution_table`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' Default: 'Dilution_Batch_Name'
#' @param dil_batch_col A vector of colours to be used for the dilution
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
#' @param have_plot_title Indicate if you want to have a plot title in
#' the `ggplot` plot.
#' Default: TRUE
#' @param plot_summary_table Indicate if you want to plot the summary table
#' in the `ggplot` plot.
#' Default: TRUE
#' @param plot_first_half_lin_reg Decide if we plot an extra regression line
#' that best fits the first half of `conc_var` dilution points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra regression line
#' that best fits the last half of `conc_var` dilution points.
#' Default: FALSE
#' @return A table with columns from `grouping variable`
#' and a new column `panel` created containing a `ggplot` dilution plot
#' in each row. This column is used to create the plot figure in the
#' pdf report.
#' @rdname add_ggplot_panel
#' @examples
#' # Data Creation
#' dilution_percent <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150,
#'   10, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2",
#'   "B2", "B2", "B2", "B2", "B2"
#' )
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a",
#'   "Sample_010b", "Sample_025b",
#'   "Sample_040b", "Sample_050b", "Sample_060b",
#'   "Sample_075b", "Sample_080b", "Sample_100b",
#'   "Sample_125b", "Sample_150b"
#' )
#' lipid1_area_saturated <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060,
#'   5192648, 16594991, 32507833, 46499896,
#'   55388856, 62505210, 62778078, 72158161,
#'   78044338, 86158414
#' )
#' lipid2_area_linear <- c(
#'   31538, 53709, 69990, 101977, 146436, 180960,
#'   232881, 283780, 298289, 344519, 430432,
#'   25463, 63387, 90624, 131274, 138069,
#'   205353, 202407, 260205, 292257, 367924
#' )
#' lipid3_area_lod <- c(
#'   544, 397, 829, 1437, 1808, 2231,
#'   3343, 2915, 5268, 8031, 11045,
#'   500, 903, 1267, 2031, 2100,
#'   3563, 4500, 5300, 8500, 10430
#' )
#' lipid4_area_nonlinear <- c(
#'   380519, 485372, 478770, 474467, 531640, 576301,
#'   501068, 550201, 515110, 499543, 474745,
#'   197417, 322846, 478398, 423174, 418577,
#'   426089, 413292, 450190, 415309, 457618
#' )
#'
#' dilution_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent
#' )
#' lipid_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Lipid1 = lipid1_area_saturated,
#'   Lipid2 = lipid2_area_linear,
#'   Lipid3 = lipid3_area_lod,
#'   Lipid4 = lipid4_area_nonlinear
#' )
#'
#'
#' # Create dilution table
#' dilution_table <- create_dilution_table(dilution_annot, lipid_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Area",
#'   column_group = "Transition_Name"
#' )
#'
#' # Create dilution table and dilution statistical summary
#' dilution_summary <- dilution_table %>%
#'   summarise_dilution_table(
#'     grouping_variable = c(
#'       "Transition_Name",
#'       "Dilution_Batch_Name"
#'     ),
#'     conc_var = "Dilution_Percent",
#'     signal_var = "Area"
#'   ) %>%
#'   dplyr::arrange(.data$Transition_Name) %>%
#'   evaluate_linearity(grouping_variable = c(
#'     "Transition_Name",
#'     "Dilution_Batch_Name"
#'   ))
#'
#'
#' # Create a ggplot table
#' ggplot_table <- add_ggplot_panel(dilution_table,
#'   dilution_summary = dilution_summary
#' )
#' ggplot_list <- ggplot_table$panel
#'
#' ggplot_list[[1]]
#' ggplot_list[[2]]
#' ggplot_list[[3]]
#' @export
add_ggplot_panel <- function(dilution_table, dilution_summary = NULL,
                             grouping_variable = c(
                               "Transition_Name",
                               "Dilution_Batch_Name"
                             ),
                             dil_batch_var = "Dilution_Batch_Name",
                             dil_batch_col = c(
                               "#377eb8",
                               "#4daf4a",
                               "#9C27B0",
                               "#BCAAA4",
                               "#FF8A65",
                               "#EFBBCF"
                             ),
                             conc_var = "Dilution_Percent",
                             conc_var_units = "%",
                             conc_var_interval = 50,
                             signal_var = "Area",
                             have_plot_title = TRUE,
                             plot_summary_table = TRUE,
                             plot_first_half_lin_reg = FALSE,
                             plot_last_half_lin_reg = FALSE) {

  # Check if dilution_table is valid with the relevant columns
  validate_dilution_table(dilution_table,
    needed_column = c(
      grouping_variable,
      dil_batch_var,
      conc_var,
      signal_var
    )
  )

  # Try to create dilution summary if you do not have one.
  if (is.null(dilution_summary)) {
    dilution_summary <- dilution_table %>%
      summarise_dilution_table(
        grouping_variable = grouping_variable,
        conc_var = conc_var,
        signal_var = signal_var
      ) %>%
      evaluate_linearity(grouping_variable = grouping_variable)
  }

  # Check if things in needed_column are in dilution_summary
  assertable::assert_colnames(dilution_summary, grouping_variable,
    only_colnames = FALSE, quiet = TRUE
  )


  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_table %>%
    dplyr::pull(.data[[dil_batch_var]]) %>%
    unique() %>%
    as.character()

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    create_char_seq(output_length = length(dilution_batch_name)) %>%
    stats::setNames(dilution_batch_name)

  # Create a summary table for each group for plotting the
  # summary table using gridExtra::tableGrob

  if (isTRUE(plot_summary_table)) {
    nested_dilution_summary <- dilution_summary %>%
      dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::rename(summary = .data$data)
  } else {
    nested_dilution_summary <- dilution_summary %>%
      dplyr::select(dplyr::all_of(grouping_variable)) %>%
      dplyr::mutate(summary = NA)
  }

  # Add dil_batch_var in the nested data
  # Will not work if dil_batch_var is also a grouping_variable

  dilution_table <- dilution_table %>%
    dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
    dplyr::relocate(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest()

  # If this is the case, we need to make a copy
  # of the variable inside the nested data

  if (dil_batch_var %in% grouping_variable) {
    dilution_table <- dilution_table %>%
      dplyr::mutate(data = purrr::map2(
        .x = .data$data,
        .y = .data[[dil_batch_var]],
        .f = function(df, dilution_batch_name) {
          df <- df %>%
            dplyr::mutate(!!dil_batch_var := dilution_batch_name)
          return(df)
        }
      ))
  }

  dilution_table <- dilution_table %>%
    dplyr::ungroup() %>%
    dplyr::left_join(nested_dilution_summary, by = grouping_variable)

  # Create a title name for each group
  # https://stackoverflow.com/questions/44613279/dplyr-concat-columns
  #-stored-in-variable-mutate-and-non-standard-evaluation?rq=1
  if (isTRUE(have_plot_title)) {
    dilution_table <- dilution_table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(title = paste0(
        dplyr::across(dplyr::all_of(grouping_variable)),
        collapse = "_"
      )) %>%
      dplyr::ungroup()
  } else {
    dilution_table <- dilution_table %>%
      dplyr::mutate(title = "")
  }

  # Start the plotting
  dilution_plots <- dilution_table %>%
    dplyr::mutate(panel = purrr::pmap(
      .l = list(
        .data$data,
        .data$summary,
        .data$title
      ),
      .f = plot_curve_ggplot,
      pal = pal,
      dil_batch_var = dil_batch_var,
      conc_var = conc_var,
      conc_var_units = conc_var_units,
      conc_var_interval = conc_var_interval,
      signal_var = signal_var,
      plot_first_half_lin_reg = plot_first_half_lin_reg,
      plot_last_half_lin_reg = plot_last_half_lin_reg
    ))


  # Left Join with the dilution_summary
  ggplot_table <- dilution_plots %>%
    dplyr::select(dplyr::all_of(c(grouping_variable))) %>%
    dplyr::bind_cols(dilution_plots %>%
      dplyr::select(.data[["panel"]])) %>%
    dplyr::left_join(dilution_summary, by = grouping_variable) %>%
    dplyr::relocate(.data[["panel"]],
      .after = dplyr::last_col()
    )


  return(ggplot_table)
}
