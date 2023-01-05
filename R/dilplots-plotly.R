#' @title Plot Curve Using `plotly`
#' @description Plot curve using `plotly`.
#' @param curve_data A data frame or tibble containing curve data.
#' @param title Title to use for each curve plot.
#' @param pal Input palette for each curve batch group in `curve_batch_var`.
#' It is a named char vector where each value is a colour and
#' name is a curve batch group given in `curve_batch_var`.
#' @param sample_name_var Column name in `curve_data`
#' to indicate the sample name.
#' @param curve_batch_var Column name in `curve_data`
#' to indicate the group name of each curve batch,
#' used to colour the points in the curve plot.
#' @param dilution_data `r lifecycle::badge("deprecated")`
#' `dilution_data` was renamed to
#' `curve_data`.
#' @param dil_batch_var `r lifecycle::badge("deprecated")`
#' `dil_batch_var` was renamed to
#' `curve_batch_var`.
#' @param conc_var Column name in `curve_data` to indicate concentration.
#' @param conc_var_units Unit of measure for `conc_var` in the curve plot.
#' @param conc_var_interval Distance between two tick labels.
#' @param signal_var Column name in `curve_data` to indicate signal.
#' @param plot_first_half_lin_reg Decide if we plot an extra regression line
#' that best fits the first half of `conc_var` curve points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra regression line
#' that best fits the last half of `conc_var` curve points.
#' Default: FALSE
#' @return Output `plotly` curve plot data of one curve batch
#' per transition.
#' @examples
#'
#' # Data Creation
#' concentration <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a"
#' )
#'
#' curve_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1"
#' )
#'
#' curve_1_saturation_regime <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Concentration = concentration,
#'   Signal = curve_1_saturation_regime,
#'   Curve_Batch_Name = curve_batch_name
#' )
#'
#' # Get the curve batch name from curve_table
#' curve_batch_name <- curve_batch_name |>
#'   unique() |>
#'   as.character()
#'
#' curve_batch_col <- c("#377eb8")
#'
#' # Create palette for each curve batch for plotting
#' pal <- curve_batch_col |>
#'   stats::setNames(curve_batch_name)
#'
#' # Plot the html
#' p <- plot_curve_plotly(curve_data,
#'   title = "Curve_Saturated",
#'   sample_name_var = "Sample_Name",
#'   pal = pal,
#'   curve_batch_var = "Curve_Batch_Name",
#'   conc_var = "Concentration",
#'   conc_var_units = "%",
#'   conc_var_interval = 50,
#'   signal_var = "Signal"
#' )
#'
#' p
#'
#' @rdname plot_curve_plotly
#' @export
plot_curve_plotly <- function(
    curve_data,
    title = "",
    pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    dilution_data = lifecycle::deprecated(),
    dil_batch_var = lifecycle::deprecated(),
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE,
    plot_last_half_lin_reg = FALSE) {

  if (lifecycle::is_present(dilution_data)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "plot_curve_plotly(dilution_data)",
      with = "plot_curve_plotly(curve_data)")
    curve_data <- dilution_data
  }

  if (lifecycle::is_present(dil_batch_var)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "plot_curve_plotly(dil_batch_var)",
      with = "plot_curve_plotly(curve_batch_var)")
    curve_batch_var <- dil_batch_var
  }

  # Convert the column that holds the curve_batch_var
  # to character
  curve_data[[curve_batch_var]] <- curve_data[[curve_batch_var]] |>
    as.character()

  # Drop rows whose value of signal_var is NA
  curve_data <- curve_data |>
    tidyr::drop_na(dplyr::all_of(signal_var))

  # For the hover text
  text_input <- glue::glue(
    "<b>{curve_data[[sample_name_var]]}</b>\\
     <br>{conc_var}: {curve_data[[conc_var]]}\\
     <br>{signal_var}: {\\
    format(curve_data[[signal_var]], big.mark = ", ", nsmall = 1)}"
  )

  # Create the dots in the curve plot
  p <- plotly::plot_ly() |>
    plotly::add_trace(
      data = curve_data,
      x = ~ curve_data[[conc_var]],
      y = ~ curve_data[[signal_var]],
      type = "scattergl", mode = "markers",
      marker = list(
        size = 10, opacity = 1,
        line = list(color = "black", width = 1.5)
      ),
      name = ~ curve_data[[curve_batch_var]],
      color = ~ curve_data[[curve_batch_var]],
      colors = pal,
      hoverinfo = "text",
      text = ~ curve_data[[sample_name_var]],
      hovertemplate = text_input,
      inherit = FALSE
    )

  if (nrow(curve_data) > 3) {

    # When we need to plot a horizontal line
    if (stats::sd(curve_data[[signal_var]]) == 0) {
      min_x <- min(curve_data[[conc_var]], na.rm = TRUE)
      max_x <- max(curve_data[[conc_var]], na.rm = TRUE)
      cont_y <- unique(curve_data[[signal_var]])

      p <- p |>
        plotly::add_segments(
          x = min_x, xend = max_x,
          y = cont_y, yend = cont_y,
          name = "lin reg",
          line = list(color = "black", width = 1),
          inherit = FALSE
        )
    } else if (stats::sd(curve_data[[conc_var]]) == 0) {
      # When we need to plot a vertical line
      min_y <- min(curve_data[[signal_var]], na.rm = TRUE)
      max_y <- max(curve_data[[signal_var]], na.rm = TRUE)
      cont_x <- unique(curve_data[[conc_var]])

      p <- p |>
        plotly::add_segments(
          x = cont_x, xend = cont_x,
          y = min_y, yend = max_y,
          name = "lin reg",
          line = list(color = "black", width = 1),
          inherit = FALSE
        )
    } else {

      # Plot the curves

      # Model the data
      linear_model <- create_linear_model(curve_data, conc_var, signal_var)
      quad_model <- create_quad_model(curve_data, conc_var, signal_var)

      curve <- seq(min(curve_data[[conc_var]]),
        max(curve_data[[conc_var]]),
        length.out = 15
      )

      # Create the linear and quadratic curve in the curve plot
      p <- p |>
        plotly::add_trace(
          data = curve_data,
          x = curve,
          y = stats::predict(
            linear_model,
            tibble::tibble(!!conc_var := curve)
          ),
          type = "scattergl", mode = "lines", name = "lin reg",
          line = list(color = "black", width = 1),
          inherit = FALSE
        ) |>
        plotly::add_trace(
          data = curve_data,
          x = curve,
          y = stats::predict(
            quad_model,
            tibble::tibble(!!conc_var := curve)
          ),
          type = "scattergl", mode = "lines", name = "quad reg",
          line = list(
            color = "red",
            width = 1,
            opacity = 0.25
          ),
          inherit = FALSE
        )


      if (isTRUE(plot_first_half_lin_reg)) {

        # Get the points for the partial linear curve
        partial_conc_points <- curve_data |>
          dplyr::pull(.data[[conc_var]]) |>
          as.numeric() |>
          sort() |>
          unique()

        partial_conc_points <-
          partial_conc_points[1:ceiling(length(partial_conc_points) / 2)]

        partial_curve_data <- curve_data |>
          dplyr::filter(.data[[conc_var]] %in% partial_conc_points)

        # Create the partial model
        partial_linear_model <- create_linear_model(
          partial_curve_data,
          conc_var, signal_var
        )

        # Create the lines in the curve plot
        p <- p |>
          plotly::add_trace(
            data = partial_curve_data,
            x = curve,
            y = stats::predict(
              partial_linear_model,
              tibble::tibble(!!conc_var := curve)
            ),
            type = "scattergl", mode = "lines", name = "lin first half reg",
            line = list(color = "blue", width = 1),
            inherit = FALSE
          )
      }

      if (isTRUE(plot_last_half_lin_reg)) {

        # Get the points for the partial linear curve
        partial_conc_points <- curve_data |>
          dplyr::pull(.data[[conc_var]]) |>
          as.numeric() |>
          sort() |>
          unique()

        last_half_index <-
          ceiling(length(partial_conc_points) / 2):length(partial_conc_points)
        partial_conc_points <- partial_conc_points[last_half_index]

        partial_curve_data <- curve_data |>
          dplyr::filter(.data[[conc_var]] %in% partial_conc_points)

        # Create the partial model
        partial_linear_model <- create_linear_model(
          partial_curve_data,
          conc_var, signal_var
        )

        # Create the lines in the curve plot
        p <- p |>
          plotly::add_trace(
            data = partial_curve_data,
            x = curve,
            y = stats::predict(
              partial_linear_model,
              tibble::tibble(!!conc_var := curve)
            ),
            type = "scatter", mode = "lines", name = "lin last half reg",
            line = list(color = "purple", width = 1),
            inherit = FALSE
          )
      }
    }
  }

  # If conc_var_units is empty, do not add brackets
  x_title <- conc_var
  if (conc_var_units != "") {
    x_title <- paste0(conc_var, " (", conc_var_units, ")")
  }

  # Create the layout to be the same as ggplot2
  p <- p |>
    plotly::layout(
      title = list(
        text = title,
        x = 0.1
      ),
      xaxis = list(
        title = x_title,
        titlefont = list(size = 10),
        gridcolor = "rgb(255,255,255)",
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        tickcolor = "rgb(127,127,127)",
        ticks = "outside",
        zeroline = FALSE,
        tickfont = list(size = 10),
        tick0 = 0,
        dtick = conc_var_interval,
        showspikes = TRUE,
        spikemode = "toaxis+marker",
        spikesnap = "data"
      ),
      # Make y axis to have no title because it
      # will be added later as an annotaton.
      yaxis = list(
        title = "",
        autorange = TRUE,
        fixedrange = FALSE,
        titlefont = list(size = 10),
        gridcolor = "rgb(255,255,255)",
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        tickcolor = "rgb(127,127,127)",
        ticks = "outside",
        zeroline = FALSE,
        tickfont = list(size = 10),
        exponentformat = "e",
        showspikes = TRUE,
        spikemode = "toaxis+marker",
        spikesnap = "data"
      ),
      hovermode = "closest",
      legend = list(
        orientation = "v",
        font = list(size = 10)
      ),
      paper_bgcolor = "rgb(255,255,255)",
      plot_bgcolor = "rgb(229,229,229)",
      showlegend = TRUE
    ) |>
    plotly::add_annotations(
      x = 0,
      y = 1,
      xref = "paper",
      yref = "paper",
      xanchor = "right",
      yanchor = "bottom",
      text = signal_var,
      showarrow = FALSE
    )

  return(p)
}

#' @title Add A `plotly` Panel Column
#' @description Create a column which contains a list of `plotly`
#' plots suited for a `trelliscopejs` visualisation.
#' @param curve_table Output given from
#' the function [create_curve_table()].
#' It is in long table format with columns indicating at least the
#' lipid/transition name, the concentration and signal. Other columns may be
#' present if it is used to group the curve together.
#' @param curve_summary The summary table generated
#' by function [summarise_curve_table()] and/or
#' [evaluate_linearity()]
#' but it can also be any generic data frame or tibble.
#' Default: NULL
#' @param dilution_table `r lifecycle::badge("deprecated")`
#' `dilution_table` was renamed to
#' `curve_table`.
#' @param dilution_summary `r lifecycle::badge("deprecated")`
#' `dilution_summary` was renamed to
#' `curve_summary`.
#' @param grouping_variable A character vector of
#' column names in `curve_table`to indicate how each curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report.
#' Default: c("Curve_Name", "Curve_Batch_Name")
#' @param sample_name_var Column name in `curve_table`
#' to indicate the sample name. To be used in the curve plot.
#' Default: 'Sample_Name'
#' @param curve_batch_var Column name in `curve_table`
#' to indicate the group name of each curve batch,
#' used to colour the points in the curve plot.
#' Default: 'Curve_Batch_Name'
#' @param curve_batch_col A vector of colours to be used for the curve
#' batch group named given in `curve_batch_var`.
#' Default: c("#377eb8", "#4daf4a", "#9C27B0", "#BCAAA4", "#FF8A65", "#EFBBCF")
#' @param dil_batch_var `r lifecycle::badge("deprecated")`
#' `dil_batch_var` was renamed to
#' `curve_batch_var`.
#' @param dil_batch_col `r lifecycle::badge("deprecated")`
#' `dil_batch_col` was renamed to
#' `curve_batch_col`.
#' @param conc_var Column name in `curve_table` to indicate concentration.
#' Default: 'Concentration'
#' @param conc_var_units Unit of measure for `conc_var`. Default: '%'
#' @param conc_var_interval Distance between two tick labels
#' in the curve plot.
#' Default: 50
#' @param signal_var Column name in `curve_table` to indicate signal.
#' Default: 'Area'
#' @param have_plot_title Indicate if you want to have a plot title in
#' the `plotly` plot.
#' Default: FALSE
#' @param plot_first_half_lin_reg Decide if we plot an extra regression line
#' that best fits the first half of `conc_var` curve points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra regression line
#' that best fits the last half of `conc_var` curve points.
#' Default: FALSE
#' @return A table that is suited for a `trelliscopejs` visualisation with
#' `grouping variable` columns converted to conditional cognostics,
#' other columns in `curve_summary` converted to cognostics and
#' a new column `panel` created containing a `plotly` curve plot in each row.
#' This column is used to create the plot figure in the
#' `trelliscopejs` visualisation.
#' @examples
#'
#' # Data Creation
#' concentration <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150,
#'   10, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#'
#' curve_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2",
#'   "B2", "B2", "B2", "B2", "B2"
#' )
#'
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
#'
#' curve_1_saturation_regime <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060,
#'   5192648, 16594991, 32507833, 46499896,
#'   55388856, 62505210, 62778078, 72158161,
#'   78044338, 86158414
#' )
#'
#' curve_2_good_linearity <- c(
#'   31538, 53709, 69990, 101977, 146436, 180960,
#'   232881, 283780, 298289, 344519, 430432,
#'   25463, 63387, 90624, 131274, 138069,
#'   205353, 202407, 260205, 292257, 367924
#' )
#'
#' curve_3_noise_regime <- c(
#'   544, 397, 829, 1437, 1808, 2231,
#'   3343, 2915, 5268, 8031, 11045,
#'   500, 903, 1267, 2031, 2100,
#'   3563, 4500, 5300, 8500, 10430
#' )
#'
#' curve_4_poor_linearity <- c(
#'   380519, 485372, 478770, 474467, 531640, 576301,
#'   501068, 550201, 515110, 499543, 474745,
#'   197417, 322846, 478398, 423174, 418577,
#'   426089, 413292, 450190, 415309, 457618
#' )
#'
#' curve_batch_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Curve_Batch_Name = curve_batch_name,
#'   Concentration = concentration
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   `Curve_1` = curve_1_saturation_regime,
#'   `Curve_2` = curve_2_good_linearity,
#'   `Curve_3` = curve_3_noise_regime,
#'   `Curve_4` = curve_4_poor_linearity
#' )
#'
#' # Create curve table
#' curve_table <- create_curve_table(
#'   curve_batch_annot = curve_batch_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Signal",
#'   column_group = "Curve_Name"
#' )
#'
#' # Create curve statistical summary
#' curve_summary <- curve_table |>
#'   summarise_curve_table(
#'     grouping_variable = c(
#'       "Curve_Name",
#'       "Curve_Batch_Name"
#'     ),
#'     conc_var = "Concentration",
#'     signal_var = "Signal"
#'   ) |>
#'   dplyr::arrange(.data[["Curve_Name"]]) |>
#'   evaluate_linearity(grouping_variable = c(
#'     "Curve_Name",
#'     "Curve_Batch_Name"
#'   ))
#'
#' # Create a trellis table
#' trellis_table <- add_plotly_panel(
#'   curve_table,
#'   curve_summary = curve_summary,
#'   grouping_variable = c("Curve_Name",
#'                         "Curve_Batch_Name"),
#'   sample_name_var = "Sample_Name",
#'   curve_batch_var = "Curve_Batch_Name",
#'       conc_var = "Concentration",
#'       conc_var_units = "%",
#'       conc_var_interval = 50,
#'       signal_var = "Signal"
#' )
#'
#' plotly_list <- trellis_table$panel
#'
#' plotly_list[[1]]
#'
#' plotly_list[[2]]
#'
#' plotly_list[[3]]
#'
#' @rdname add_plotly_panel
#' @export
add_plotly_panel <- function(
    curve_table,
    curve_summary = NULL,
    dilution_table = lifecycle::deprecated(),
    dilution_summary = lifecycle::deprecated(),
    grouping_variable = c("Curve_Name",
                          "Curve_Batch_Name"),
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    curve_batch_col = c("#377eb8", "#4daf4a",
                       "#9C27B0", "#BCAAA4",
                       "#FF8A65", "#EFBBCF"),
    dil_batch_var = lifecycle::deprecated(),
    dil_batch_col = lifecycle::deprecated(),
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    have_plot_title = FALSE,
    plot_first_half_lin_reg = FALSE,
    plot_last_half_lin_reg = FALSE) {

  if (lifecycle::is_present(dilution_table)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "add_plotly_panel(dilution_table)",
      with = "add_plotly_panel(curve_table)")
    curve_table <- dilution_table
  }

  if (lifecycle::is_present(dilution_summary)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "add_plotly_panel(dilution_summary)",
      with = "add_plotly_panel(curve_summary)")
    curve_summary <- dilution_summary
  }

  if (lifecycle::is_present(dil_batch_var)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "add_plotly_panel(dil_batch_var)",
      with = "add_plotly_panel(curve_batch_var)")
    curve_batch_var <- dil_batch_var
  }

  if (lifecycle::is_present(dil_batch_col)) {
    lifecycle::deprecate_warn(
      when = "0.0.6.9000",
      what = "add_plotly_panel(dil_batch_col)",
      with = "add_plotly_panel(curve_batch_col)")
    curve_batch_col <- dil_batch_col
  }

  # Check if curve_table is valid with the relevant columns
  validate_curve_table(
    curve_table = curve_table,
    needed_column = c(
      grouping_variable,
      sample_name_var,
      curve_batch_var,
      conc_var,
      signal_var
    )
  )

  # Try to create curve summary if you do not have one.
  if (is.null(curve_summary)) {
    curve_summary <- curve_table |>
      summarise_curve_table(
        grouping_variable = grouping_variable,
        conc_var = conc_var,
        signal_var = signal_var
      ) |>
      evaluate_linearity(grouping_variable = grouping_variable)
  }

  # Check if things in needed_column are in curve_summary
  assertable::assert_colnames(curve_summary, grouping_variable,
    only_colnames = FALSE, quiet = TRUE
  )

  # Get the curve batch name from curve_table
  curve_batch_name <- curve_table |>
    dplyr::pull(.data[[curve_batch_var]]) |>
    unique() |>
    as.character()

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col |>
    create_char_seq(output_length = length(curve_batch_name)) |>
    stats::setNames(curve_batch_name)


  # Create a title name for each group
  if (isTRUE(have_plot_title)) {
    curve_table <- curve_table |>
      dplyr::rowwise() |>
      dplyr::mutate(title = paste0(
        dplyr::across(dplyr::all_of(grouping_variable)),
        collapse = "_"
      )) |>
      dplyr::ungroup()
  } else {
    curve_table <- curve_table |>
      dplyr::mutate(title = "")
  }

  # Add curve_batch_var in the nested data
  # Will not work if curve_batch_var is also a grouping_variable

  curve_table <- curve_table |>
    dplyr::group_by_at(
      c(grouping_variable, "title")
    ) |>
    dplyr::relocate(dplyr::all_of(c(grouping_variable, "title"))) |>
    tidyr::nest()

  # If this is the case, we need to make a copy
  # of the variable inside the nested data

  if (curve_batch_var %in% grouping_variable) {
    curve_table <- curve_table |>
      dplyr::mutate(data = purrr::map2(
        .x = .data$data,
        .y = .data[[curve_batch_var]],
        .f = function(df, curve_batch_name) {
          df <- df |>
            dplyr::mutate(!!curve_batch_var := curve_batch_name)
          return(df)
        }
      ))
  }

  # Group/Nest the curve data for each group
  # and do a curve plot for each of them
  curve_plots <- curve_table |>
    dplyr::ungroup() |>
    dplyr::mutate(panel = trelliscopejs::map2_plot(
      .x = .data$data,
      .y = .data$title,
      .f = plot_curve_plotly,
      pal = pal,
      sample_name_var = sample_name_var,
      curve_batch_var = curve_batch_var,
      conc_var = conc_var,
      conc_var_units = conc_var_units,
      conc_var_interval = conc_var_interval,
      signal_var = signal_var,
      plot_first_half_lin_reg = plot_first_half_lin_reg,
      plot_last_half_lin_reg = plot_last_half_lin_reg
    ))


  # Left Join plots with grouping variable and curve_summary
  trellis_table <- curve_plots |>
    dplyr::select(dplyr::all_of(c(grouping_variable))) |>
    dplyr::bind_cols(curve_plots |>
      dplyr::select(dplyr::any_of("panel"))) |>
    dplyr::left_join(curve_summary, by = grouping_variable) |>
    dplyr::relocate(
      dplyr::any_of("panel"),
      .after = dplyr::last_col()
    )

  return(trellis_table)
}
