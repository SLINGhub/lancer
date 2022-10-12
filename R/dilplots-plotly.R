#' @title Plot Dilution Curve Using `plotly`
#' @description Plot Dilution Data using `plotly`
#' @param dilution_data A data frame or tibble containing dilution data
#' @param title Title to use for each dilution plot
#' @param pal Input palette for each dilution batch group in `dil_batch_var`.
#' It is a named char vector where each value is a colour and
#' name is a dilution batch group given in `dil_batch_var`
#' @param sample_name_var Column name in `dilution_data`
#' to indicate the sample name
#' @param dil_batch_var Column name in `dilution_data`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param conc_var_units Unit of measure for `conc_var` in the dilution plot
#' @param conc_var_interval Distance between two tick labels
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @param plot_first_half_lin_reg Decide if we plot an extra regression line
#' that best fits the first half of `conc_var` dilution points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra regression line
#' that best fits the last half of `conc_var` dilution points.
#' Default: FALSE
#' @return Output `plotly` dilution plot data of one dilution batch
#' per transition
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
#' lipid1_area_saturated <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060
#' )
#'
#' dilution_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Percent = dilution_percent,
#'   Area = lipid1_area_saturated,
#'   Dilution_Batch_Name = dilution_batch_name
#' )
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
#' # Plot the html
#' p <- plot_curve_plotly(dilution_data,
#'   title = "Lipid_Saturated",
#'   sample_name_var = "Sample_Name",
#'   pal = pal,
#'   dil_batch_var = "Dilution_Batch_Name",
#'   conc_var = "Dilution_Percent",
#'   conc_var_units = "%",
#'   conc_var_interval = 50,
#'   signal_var = "Area"
#' )
#' @rdname plot_curve_plotly
#' @export
plot_curve_plotly <- function(dilution_data,
                              title,
                              pal,
                              sample_name_var = "Sample_Name",
                              dil_batch_var = "Dilution_Batch_Name",
                              conc_var = "Dilution_Percent",
                              conc_var_units = "%",
                              conc_var_interval = 50,
                              signal_var = "Area",
                              plot_first_half_lin_reg = FALSE,
                              plot_last_half_lin_reg = FALSE) {

  # Convert the column that holds the dilution_batch_var
  # to character
  dilution_data[[dil_batch_var]] <- dilution_data[[dil_batch_var]] %>%
    as.character()

  # Drop rows whose value of signal_var is NA
  dilution_data <- dilution_data %>%
    tidyr::drop_na(.data[[signal_var]])

  # For the hover text
  text_input <- glue::glue(
    "<b>{dilution_data[[sample_name_var]]}</b>\\
     <br>{conc_var}: {dilution_data[[conc_var]]}\\
     <br>{signal_var}: {\\
    format(dilution_data[[signal_var]], big.mark = ", ", nsmall = 1)}"
  )

  # Create the dots in the dilution plot
  p <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = dilution_data,
      x = ~ dilution_data[[conc_var]],
      y = ~ dilution_data[[signal_var]],
      type = "scattergl", mode = "markers",
      marker = list(
        size = 10, opacity = 1,
        line = list(color = "black", width = 1.5)
      ),
      name = ~ dilution_data[[dil_batch_var]],
      color = ~ dilution_data[[dil_batch_var]],
      colors = pal,
      hoverinfo = "text",
      text = ~ dilution_data[[sample_name_var]],
      hovertemplate = text_input,
      inherit = FALSE
    )

  if (nrow(dilution_data) > 3) {

    # When we need to plot a horizontal line
    if (stats::sd(dilution_data[[signal_var]]) == 0) {
      min_x <- min(dilution_data[[conc_var]], na.rm = TRUE)
      max_x <- max(dilution_data[[conc_var]], na.rm = TRUE)
      cont_y <- unique(dilution_data[[signal_var]])

      p <- p %>%
        plotly::add_segments(
          x = min_x, xend = max_x,
          y = cont_y, yend = cont_y,
          name = "lin reg",
          line = list(color = "black", width = 1),
          inherit = FALSE
        )
    } else if (stats::sd(dilution_data[[conc_var]]) == 0) {
      # When we need to plot a vertical line
      min_y <- min(dilution_data[[signal_var]], na.rm = TRUE)
      max_y <- max(dilution_data[[signal_var]], na.rm = TRUE)
      cont_x <- unique(dilution_data[[conc_var]])

      p <- p %>%
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
      linear_model <- create_linear_model(dilution_data, conc_var, signal_var)
      quad_model <- create_quad_model(dilution_data, conc_var, signal_var)

      dilution <- seq(min(dilution_data[[conc_var]]),
        max(dilution_data[[conc_var]]),
        length.out = 15
      )

      # Create the linear and quadratic curve in the dilution plot
      p <- p %>%
        plotly::add_trace(
          data = dilution_data,
          x = dilution,
          y = stats::predict(
            linear_model,
            tibble::tibble(!!conc_var := dilution)
          ),
          type = "scattergl", mode = "lines", name = "lin reg",
          line = list(color = "black", width = 1),
          inherit = FALSE
        ) %>%
        plotly::add_trace(
          data = dilution_data,
          x = dilution,
          y = stats::predict(
            quad_model,
            tibble::tibble(!!conc_var := dilution)
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
        partial_conc_points <- dilution_data %>%
          dplyr::pull(.data[[conc_var]]) %>%
          as.numeric() %>%
          sort() %>%
          unique()

        partial_conc_points <-
          partial_conc_points[1:ceiling(length(partial_conc_points) / 2)]

        partial_dilution_data <- dilution_data %>%
          dplyr::filter(.data[[conc_var]] %in% partial_conc_points)

        # Create the partial model
        partial_linear_model <- create_linear_model(
          partial_dilution_data,
          conc_var, signal_var
        )

        # Create the lines in the dilution plot
        p <- p %>%
          plotly::add_trace(
            data = partial_dilution_data,
            x = dilution,
            y = stats::predict(
              partial_linear_model,
              tibble::tibble(!!conc_var := dilution)
            ),
            type = "scattergl", mode = "lines", name = "lin first half reg",
            line = list(color = "blue", width = 1),
            inherit = FALSE
          )
      }

      if (isTRUE(plot_last_half_lin_reg)) {

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

        # Create the lines in the dilution plot
        p <- p %>%
          plotly::add_trace(
            data = partial_dilution_data,
            x = dilution,
            y = stats::predict(
              partial_linear_model,
              tibble::tibble(!!conc_var := dilution)
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
  p <- p %>%
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
    ) %>%
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
#' @description Create a column which contains a list of `plotly
#' suited for a `trelliscopejs` visualisation
#' @param dilution_table Output given from
#' the function [create_dilution_table()]
#' It is in long table format with columns indicating at least the
#' lipid/transition name, the concentration and signal. Other columns may be
#' present if it is used to group the dilution curve together
#' @param dilution_summary The summary table generated
#' by function [summarise_dilution_table()] and/or
#' [evaluate_linearity()]
#' but it can also be any generic data frame or tibble
#' Default: NULL
#' @param grouping_variable A character vector of
#' column names in `dilution_table`to indicate how each dilution curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report.
#' Default: c("Transition_Name", "Dilution_Batch_Name")
#' @param sample_name_var Column name in `dilution_table`
#' to indicate the sample name. To be used in the dilution plot
#' Default: 'Sample_Name'
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
#' the `plotly` plot.
#' Default: FALSE
#' @param plot_first_half_lin_reg Decide if we plot an extra regression line
#' that best fits the first half of `conc_var` dilution points.
#' Default: FALSE
#' @param plot_last_half_lin_reg Decide if we plot an extra regression line
#' that best fits the last half of `conc_var` dilution points.
#' Default: FALSE
#' @return A table that is suited for a `trelliscopejs` visualisation with
#' `grouping variable` columns converted to conditional cognostics,
#' other columns in `dilution_summary` converted to cognostics and
#' a new column `panel` created containing a `plotly` dilution plot in each row.
#' This column is used to create the plot figure in the
#' `trelliscopejs` visualisation.
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
#' # Create a trellis table
#' trellis_table <- add_plotly_panel(dilution_table,
#'   dilution_summary = dilution_summary
#' )
#' @rdname add_plotly_panel
#' @export
add_plotly_panel <- function(dilution_table, dilution_summary = NULL,
                             grouping_variable = c(
                               "Transition_Name",
                               "Dilution_Batch_Name"
                             ),
                             sample_name_var = "Sample_Name",
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
                             have_plot_title = FALSE,
                             plot_first_half_lin_reg = FALSE,
                             plot_last_half_lin_reg = FALSE) {



  # Check if dilution_table is valid with the relevant columns
  validate_dilution_table(dilution_table,
    needed_column = c(
      grouping_variable,
      sample_name_var,
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


  # Create a title name for each group
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

  # Add dil_batch_var in the nested data
  # Will not work if dil_batch_var is also a grouping_variable

  dilution_table <- dilution_table %>%
    dplyr::group_by_at(dplyr::all_of(c(grouping_variable, "title"))) %>%
    dplyr::relocate(dplyr::all_of(c(grouping_variable, "title"))) %>%
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

  # Group/Nest the dilution data for each group
  # and do a dilution plot for each of them
  dilution_plots <- dilution_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(panel = trelliscopejs::map2_plot(
      .x = .data$data,
      .y = .data$title,
      .f = plot_curve_plotly,
      pal = pal,
      sample_name_var = sample_name_var,
      dil_batch_var = "Dilution_Batch_Name",
      conc_var = conc_var,
      conc_var_units = conc_var_units,
      conc_var_interval = conc_var_interval,
      signal_var = signal_var,
      plot_first_half_lin_reg = plot_first_half_lin_reg,
      plot_last_half_lin_reg = plot_last_half_lin_reg
    ))


  # Left Join plots with grouping variable and dilution_summary
  trellis_table <- dilution_plots %>%
    dplyr::select(dplyr::all_of(c(grouping_variable))) %>%
    dplyr::bind_cols(dilution_plots %>%
      dplyr::select(.data[["panel"]])) %>%
    dplyr::left_join(dilution_summary, by = grouping_variable) %>%
    dplyr::relocate(.data[["panel"]],
      .after = dplyr::last_col()
    )

  return(trellis_table)
}
