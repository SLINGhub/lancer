#' @title Plot Dilution Data using plotly
#' @description Plot Dilution Data using plotly
#' @param dilution_data A data frame or tibble containing dilution data
#' @param sample_name_var Column name in `dilution_data`
#' to indicate the sample name
#' @param dil_batch_var Column name in `dilution_data`
#' to indicate the group name of each dilution batch,
#' used to colour the points in the dilution plot
#' @param conc_var Column name in `dilution_data` to indicate concentration
#' @param conc_var_units Unit of measure for `conc_var`
#' @param conc_var_interval Distance between two tick labels
#' @param signal_var Column name in `dilution_data` to indicate signal
#' @param pal Input palette for each dilution batch group in `dil_batch_var`.
#' It is a named char vector where each value is a colour and
#' name is a dilution batch group given in `dil_batch_var`
#' @return Output Dilution Plot data of one dilution batch per transition
#' @rdname dilution_plot_plotly
#' @export
dilution_plot_plotly <- function(dilution_data,
                                 sample_name_var,
                                 dil_batch_var,
                                 conc_var, conc_var_units,
                                 conc_var_interval,
                                 signal_var,
                                 pal) {

  # Drop values that are NA in signal_var
  dilution_data <- tidyr::drop_na(dilution_data, .data[[signal_var]])

  # Create the dots in the dilution plot
  p <- plotly::plot_ly() %>%
    plotly::add_trace(data = dilution_data,
                      x = ~dilution_data[[conc_var]],
                      y = ~dilution_data[[signal_var]],
                      type = "scattergl", mode = "markers",
                      marker = list(size = 10, opacity = 1,
                                    line = list(color = "black", width = 1.5)),
                      name = ~dilution_data[[dil_batch_var]],
                      color = ~dilution_data[[dil_batch_var]],
                      colors = pal,
                      hoverinfo = "text",
                      text = ~dilution_data[[sample_name_var]],
                      hovertemplate = paste(
                        "<b>%{text}</b><br>",
                        "%{xaxis.title.text}: %{x}<br>",
                        "%{yaxis.title.text}: %{y:,0f}<br>",
                        "<extra></extra>"),
                      inherit = FALSE)

  if (nrow(dilution_data) > 3) {

    # Create the formula
    linear_formula <- stats::as.formula(paste(signal_var, "~",
                                              paste(conc_var, collapse = " + ")
    )
    )
    # Create the linear model on dilution data
    linear_model <- stats::lm(linear_formula, data = dilution_data)

    # Create the formula
    quad_formula <- stats::as.formula(paste(signal_var, "~",
                                            paste(conc_var, "+",
                                                  paste0("I(", conc_var, " * ",
                                                         conc_var, ")")
                                            )
    )
    )

    # Create the quadratic model on dilution data
    quad_model <- stats::lm(quad_formula, data = dilution_data)

    dilution <- seq(min(dilution_data[[conc_var]]),
                    max(dilution_data[[conc_var]]),
                    length.out = 15)

    # Create the lines in the dilution plot
    p <- p %>%
      plotly::add_trace(data = dilution_data, x = dilution,
                        y = stats::predict(linear_model,
                                           data.frame(Dilution_Percent = dilution)),
                        type = "scattergl", mode = "lines", name = "lin reg",
                        line = list(color = "black", width = 1),
                        inherit = FALSE) %>%
      plotly::add_trace(data = dilution_data, x = dilution,
                        y = stats::predict(quad_model,
                                           data.frame(Dilution_Percent = dilution)),
                        type = "scattergl", mode = "lines", name = "quad reg",
                        line = list(color = "red", width = 1, opacity = 0.25),
                        inherit = FALSE)
  }

  # Create the layout to be the same as ggplot2
  p <- p %>%
    plotly::layout(xaxis = list(title = paste0(conc_var, " (",  conc_var_units, ")"),
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
                                spikesnap = "data"),
                   yaxis = list(title = signal_var,
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
                                spikesnap = "data"),
                   hovermode = "x",
                   legend = list(orientation = "v",
                                 font = list(size = 10)),
                   paper_bgcolor = "rgb(255,255,255)",
                   plot_bgcolor = "rgb(229,229,229)",
                   showlegend = FALSE
    )

  return(p)
}


#' @title Create cyclic character sequence
#' @description Create cyclic character sequence
#' @param group_name A character vector as input
#' @param output_length The length of the output sequence
#' @return A cyclic character sequence with length `output_length`
#' @details Taken from
#' https://community.rstudio.com/t/fill-in-a-sequence-of-letters-based-on-a-given-order/88823/3
#' @examples
#' group_name <- c("red", "green", "blue")
#' create_char_seq(group_name, output_length = 2)
#' create_char_seq(group_name, output_length = 5)
#'
#' @rdname create_char_seq
#' @export
create_char_seq <- function(group_name, output_length) {

  # whole integer division
  i <- output_length %/% length(group_name)

  # remainder
  r <- output_length %% length(group_name)

  # Set the number of cycles needed
  if (r > 0) {
    t <- i + 1
  } else {
    t <- i
  }

  # Create the set and cut by the output_length
  set <- rep(group_name, t)
  output <- set[1:output_length]

  return(output)

}

#' @export
create_trellis_table <- function(dilution_table, dilution_summary = NULL,
                                 cog_df = NULL,
                                 col_name_vec = "col_name_vec",
                                 desc_vec = "desc_vec",
                                 type_vec = "type_vec",
                                 default_label_vec = "default_label_vec",
                                 grouping_variable = c("Transition_Name",
                                                       "Dilution_Batch"),
                                 sample_name_var = "Sample_Name",
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
                                            sample_name_var,
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


  # Get cognostics for dilution_summary
  # Grouping variables must be the conditional columns
  dilution_summary <- dilution_summary %>%
    convert_to_cognostics(cog_df = cog_df,
                          grouping_variable = grouping_variable,
                          col_name_vec = col_name_vec,
                          desc_vec = desc_vec,
                          type_vec = type_vec,
                          default_label_vec = default_label_vec)


  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_table %>%
    dplyr::pull(.data[[dil_batch_var]]) %>%
    unique() %>%
    as.character()

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    create_char_seq(output_length = length(dilution_batch_name)) %>%
    stats::setNames(dilution_batch_name)

  # Group/Nest the dilution data for each group
  # and do a dilution plot for each of them
  dilution_plots <- dilution_table %>%
    dplyr::mutate(Dilution_Batch_Name = .data[[dil_batch_var]]) %>%
    dplyr::group_by_at(dplyr::all_of(grouping_variable)) %>%
    dplyr::relocate(dplyr::all_of(grouping_variable)) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(panel = trelliscopejs::map_plot(.data$data,
                                                  dilution_plot_plotly,
                                                  sample_name_var = sample_name_var,
                                                  dil_batch_var = "Dilution_Batch_Name",
                                                  conc_var = conc_var,
                                                  conc_var_units = conc_var_units,
                                                  conc_var_interval = conc_var_interval,
                                                  signal_var = signal_var,
                                                  pal = pal)
                  )

  # Convert the grouping variables to conditioning cognostics
  # Left Join with the dilution_summary
  trellis_table <- dilution_plots %>%
    dplyr::select(dplyr::all_of(grouping_variable)) %>%
    trelliscopejs::as_cognostics(cond_cols = grouping_variable,
                                 needs_cond = TRUE,
                                 needs_key = FALSE) %>%
    dplyr::bind_cols(dilution_plots %>%
                       dplyr::select(.data[["panel"]])) %>%
    dplyr::left_join(dilution_summary, by = grouping_variable)

  return(trellis_table)


}
