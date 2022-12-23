test_that("Able to plot dilution data with plotly correctly", {

  # Data Creation
  dilution_percent <- c(
    10, 20, 25, 40, 50, 60,
    75, 80, 100, 125, 150
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a",
    "Sample_025a", "Sample_040a", "Sample_050a",
    "Sample_060a", "Sample_075a", "Sample_080a",
    "Sample_100a", "Sample_125a", "Sample_150a"
  )

  dilution_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1",
    "B1", "B1", "B1", "B1", "B1", "B1"
  )

  lipid1_area_saturated <- c(
    5748124, 16616414, 21702718, 36191617,
    49324541, 55618266, 66947588, 74964771,
    75438063, 91770737, 94692060
  )

  lipid2_area_linear <- c(
    31538, 53709, 69990, 101977, 146436, 180960,
    232881, 283780, 298289, 344519, 4304324
  )

  lipid3_area_lod <- c(
    544, 397, 829, 1437, 1808, 2231,
    3343, 2915, 5268, 8031, 11045
  )

  lipid4_area_nonlinear <- c(
    380519, 485372, 478770, 474467, 531640, 576301,
    501068, 550201, 515110, 499543, 474745
  )

  bad_area <- c(
    2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2
  )

  bad_conc <- c(
    50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50
  )

  na_area <- c(
    NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA
  )

  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Dilution_Percent = dilution_percent,
    Area = lipid1_area_saturated,
    Dilution_Batch_Name = dilution_batch_name
  )

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col <- c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Lipid_Saturated",
    pal = pal,
    sample_name_var = "Sample_Name",
    curv_batch_var = "Dilution_Batch_Name",
    conc_var = "Dilution_Percent",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Area",
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
    Dilution_Percent = dilution_percent,
    Area = bad_area,
    Dilution_Batch_Name = dilution_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Lipid_Horizontal",
    pal = pal,
    sample_name_var = "Sample_Name",
    curv_batch_var = "Dilution_Batch_Name",
    conc_var = "Dilution_Percent",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Area"
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
    Dilution_Percent = bad_conc,
    Area = lipid1_area_saturated,
    Dilution_Batch_Name = dilution_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Lipid_Vertical",
    pal = pal,
    sample_name_var = "Sample_Name",
    curv_batch_var = "Dilution_Batch_Name",
    conc_var = "Dilution_Percent",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Area"
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
    Dilution_Percent = bad_conc,
    Area = bad_area,
    Dilution_Batch_Name = dilution_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Lipid_Point",
    pal = pal,
    sample_name_var = "Sample_Name",
    curv_batch_var = "Dilution_Batch_Name",
    conc_var = "Dilution_Percent",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Area"
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
    Dilution_Percent = dilution_percent,
    Area = na_area,
    Dilution_Batch_Name = dilution_batch_name
  )

  # Plot the html
  p <- plot_curve_plotly(curve_data,
    title = "Lipid_No_Point",
    pal = pal,
    sample_name_var = "Sample_Name",
    curv_batch_var = "Dilution_Batch_Name",
    conc_var = "Dilution_Percent",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Area"
  )

  # The first trace is markers (scatter plot)
  # No lines traces is present
  testthat::expect_equal(
    p$x$attrs[[2]]$mode,
    "markers"
  )
})
