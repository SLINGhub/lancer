test_that("Able to plot curve data with plotly correctly", {

  # Data Creation
  concentration <- c(
    10, 20, 25, 40, 50, 60,
    75, 80, 100, 125, 150
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a",
    "Sample_025a", "Sample_040a", "Sample_050a",
    "Sample_060a", "Sample_075a", "Sample_080a",
    "Sample_100a", "Sample_125a", "Sample_150a"
  )

  curve_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1",
    "B1", "B1", "B1", "B1", "B1", "B1"
  )

  curve_1_saturation_regime <- c(
    5748124, 16616414, 21702718, 36191617,
    49324541, 55618266, 66947588, 74964771,
    75438063, 91770737, 94692060
  )

  bad_signal <- c(
    2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2
  )

  bad_concentration <- c(
    50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50
  )

  na_signal <- c(
    NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA
  )

  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Concentration = concentration,
    Signal = curve_1_saturation_regime,
    Curve_Batch_Name = curve_batch_name
  )

  curve_batch_name <- curve_batch_name |>
    unique() |>
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col |>
    stats::setNames(curve_batch_name)

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Curve_Saturated",
    pal = pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = TRUE,
    plot_last_half_lin_reg = TRUE
  )

  # The first trace is markers (scatter plot)
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )

  # The next four traces are lines (regression)
  for (trace_index in c(3:6)) {
    testthat::expect_equal(
      p$x$attrs[[trace_index]]$mode,
      "lines"
    )
  }

  # Handle the case of horizontal line
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Concentration = concentration,
    Signal = bad_signal,
    Curve_Batch_Name = curve_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Curve_Horizontal",
    pal = pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  )


  # The first trace is markers (scatter plot)
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )

  # The next trace is lines (horizontal line)
  for (trace_index in c(3)) {
    testthat::expect_equal(
      p$x$attrs[[trace_index]]$mode,
      "lines"
    )
  }

  # Handle the case of vertical line
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Concentration = bad_concentration,
    Signal = curve_1_saturation_regime,
    Curve_Batch_Name = curve_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Curve_Vertical",
    pal = pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  )

  # The first trace is markers (scatter plot)
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )

  # The next trace is lines (vertical line)
  for (trace_index in c(3)) {
    testthat::expect_equal(
      p$x$attrs[[trace_index]]$mode,
      "lines"
    )
  }

  # Handle the case of a point
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Concentration = bad_concentration,
    Signal = bad_signal,
    Curve_Batch_Name = curve_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Curve_Point",
    pal = pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  )

  # The first trace is markers (scatter plot)
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )

  # The next trace is lines (a dot)
  for (trace_index in c(3)) {
    testthat::expect_equal(
      p$x$attrs[[trace_index]]$mode,
      "lines"
    )
  }

  # Handle the case of a plot that gives no points
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Concentration = bad_concentration,
    Signal = na_signal,
    Curve_Batch_Name = curve_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Curve_No_Point",
    pal = pal,
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  )

  # The first trace is markers (scatter plot)
  # No lines traces is present
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )
})

test_that("Create a plotly table with plot title", {

  # Data Creation
  concentration <- c(
    10, 20, 25, 40, 50, 60,
    75, 80, 100, 125, 150
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a",
    "Sample_025a", "Sample_040a", "Sample_050a",
    "Sample_060a", "Sample_075a", "Sample_080a",
    "Sample_100a", "Sample_125a", "Sample_150a"
  )

  curve_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1",
    "B1", "B1", "B1", "B1", "B1", "B1"
  )

  curve_name <- c(
    "Curve_1", "Curve_1", "Curve_1", "Curve_1",
    "Curve_1", "Curve_1", "Curve_1", "Curve_1",
    "Curve_1", "Curve_1", "Curve_1"
  )

  curve_1_saturation_regime <- c(
    5748124, 16616414, 21702718, 36191617,
    49324541, 55618266, 66947588, 74964771,
    75438063, 91770737, 94692060
  )

  curve_batch_annot <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration
  )

  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    `Curve_1` = curve_1_saturation_regime
  )

  # Create curve table
  curve_table <- create_curve_table(
    curve_batch_annot = curve_batch_annot,
    curve_data_wide = curve_data,
    common_column = "Sample_Name",
    signal_var = "Signal",
    column_group = "Curve_Name"
  )

  # Create curve statistical summary
  curve_summary <- curve_table |>
    summarise_curve_table(
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      conc_var = "Concentration",
      signal_var = "Signal"
    ) |>
    dplyr::arrange(.data[["Curve_Name"]]) |>
    evaluate_linearity(grouping_variable = c(
      "Curve_Name",
      "Curve_Batch_Name"
    ))

  # Create a plotly table with plot title
  plotly_table <- add_plotly_panel(
    curve_table = curve_table,
    curve_summary = curve_summary,
    grouping_variable = c("Curve_Name",
                          "Curve_Batch_Name"),
    curve_batch_var = "Curve_Batch_Name",
    curve_batch_col = c("#377eb8"),
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    have_plot_title = TRUE,
    plot_first_half_lin_reg = FALSE,
    plot_last_half_lin_reg = FALSE
  )

  p <- plotly_table$panel[[1]]

  # Plot title is Curve_1_B1
  testthat::expect_equal(
    p$x$layoutAttrs[[p$x$cur_data]]$title$text,
    "Curve_1_B1"
  )

})


