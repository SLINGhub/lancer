test_that("Able to plot good curve data with ggplot correctly", {

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

  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration,
    Curve_Name = curve_name,
    Signal = curve_1_saturation_regime,
  )

  grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

  curve_batch_name <- curve_batch_name %>%
    unique() %>%
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col %>%
    stats::setNames(curve_batch_name)

  # Create curve statistical summary
  curve_summary_grp <- curve_data %>%
    summarise_curve_table(
      grouping_variable = grouping_variable,
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))

  # Create the ggplot
  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Lipid_Saturated",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE
  )

  vdiffr::expect_doppelganger("A usual curve plot", p)

  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Lipid_Saturated",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = TRUE
  )

  vdiffr::expect_doppelganger("A Lin First Half curve plot", p)

  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Lipid_Saturated",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_last_half_lin_reg = TRUE
  )

  vdiffr::expect_doppelganger("A Lin Last Half curve plot", p)

  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Lipid_Saturated",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = TRUE,
    plot_last_half_lin_reg = TRUE
  )

  vdiffr::expect_doppelganger("A Lin First and Last Half curve plot", p)
})


test_that("Able to plot horizontal curve data with ggplot correctly", {

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

  bad_signal <- c(
    2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2
  )

  # Handle the case of horizontal line
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration,
    Curve_Name = curve_name,
    Signal = bad_signal,
  )

  grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

  curve_batch_name <- curve_batch_name %>%
    unique() %>%
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col %>%
    stats::setNames(curve_batch_name)

  # Create curve statistical summary
  curve_summary_grp <- curve_data %>%
    summarise_curve_table(
      grouping_variable = grouping_variable,
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))

  # Create the ggplot
  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Curve_Horizontal",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE
  )

  vdiffr::expect_doppelganger("A horizontal curve plot", p)
})


test_that("Able to plot vertical curve data with ggplot correctly", {

  # Data Creation
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

  bad_concentration <- c(
    50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50
  )

  # Handle the case of vertical line
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = bad_concentration,
    Curve_Name = curve_name,
    Signal = curve_1_saturation_regime,
  )

  grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

  curve_batch_name <- curve_batch_name %>%
    unique() %>%
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col %>%
    stats::setNames(curve_batch_name)

  # Create curve statistical summary
  curve_summary_grp <- curve_data %>%
    summarise_curve_table(
      grouping_variable = grouping_variable,
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))

  # Create the ggplot
  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Curve_Vertical",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE
  )

  vdiffr::expect_doppelganger("A vertical curve plot", p)
})

test_that("Able to plot single point curve data with ggplot correctly", {

  # Data Creation
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

  bad_signal <- c(
    2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2
  )

  bad_concentration <- c(
    50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50
  )

  # Handle the case of a point
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = bad_concentration,
    Curve_Name = curve_name,
    Signal = bad_signal,
  )

  grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

  curve_batch_name <- curve_batch_name %>%
    unique() %>%
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col %>%
    stats::setNames(curve_batch_name)

  # Create curve statistical summary
  curve_summary_grp <- curve_data %>%
    summarise_curve_table(
      grouping_variable = grouping_variable,
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Curve_Point",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE
  )

  vdiffr::expect_doppelganger("A single point curve plot", p)
})

test_that("Able to plot NA curve data with ggplot correctly", {

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

  na_signal <- c(
    NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA
  )

  # Handle the case of a plot that gives no points
  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration,
    Curve_Name = curve_name,
    Signal = na_signal,
  )

  grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

  curve_batch_name <- curve_batch_name %>%
    unique() %>%
    as.character()

  curve_batch_col <- c("#377eb8")

  # Create palette for each curve batch for plotting
  pal <- curve_batch_col %>%
    stats::setNames(curve_batch_name)

  # Create curve statistical summary
  curve_summary_grp <- curve_data %>%
    summarise_curve_table(
      grouping_variable = grouping_variable,
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(curve_data,
    curve_summary_grp = curve_summary_grp,
    title = "Curve_No_Point",
    pal = pal,
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    plot_first_half_lin_reg = FALSE
  )

  vdiffr::expect_doppelganger("A NA curve plot", p)
})


test_that("Able to plot summary (numeric and characters) grid table for one curve group", {
    wf1_group <- c("Poor Linearity")
    wf2_group <- c("Saturation")
    r_corr <- c(0.951956)
    pra_linear <- c(65.78711)
    mandel_p_val <- c(2.899006e-07)
    concavity <- c(-4133.501328)
    logical <- TRUE

    curve_summary_grp <- data.frame(
      wf1_group = wf1_group,
      wf2_group = wf2_group,
      logical = logical,
      r_corr = r_corr,
      pra_linear = pra_linear,
      mandel_p_val = mandel_p_val,
      concavity = concavity
    )

    # Test that it works in the usual case
    table <- plot_summary_table(curve_summary_grp)

    vdiffr::expect_doppelganger(
      "complete summary table",
      grid::grid.draw(table)
    )
  }
)

test_that("Able to plot summary (only numeric) grid table for one curve group", {
  r_corr <- c(0.951956)
  pra_linear <- c(65.78711)
  mandel_p_val <- c(2.899006e-07)
  concavity <- c(-4133.501328)

  curve_summary_grp <- data.frame(
    r_corr = r_corr,
    pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity
  )
  testthat::expect_null(plot_summary_table_char(curve_summary_grp))

  # Test that it works even if there is no character or factor
  # or logical columns
  table <- plot_summary_table(curve_summary_grp)

  vdiffr::expect_doppelganger("numeric summary table", grid::grid.draw(table))
})

test_that("Able to plot summary (only character) grid table for one curve group", {
  wf1_group <- c("Poor Linearity")
  wf2_group <- c("Saturation")
  r_corr <- c(0.951956)
  pra_linear <- c(65.78711)
  mandel_p_val <- c(2.899006e-07)
  concavity <- c(-4133.501328)
  logical <- TRUE

  curve_summary_grp <- data.frame(
    wf1_group = wf1_group,
    wf2_group = wf2_group,
    logical = logical
  )

  testthat::expect_null(plot_summary_table_num(curve_summary_grp))

  # Test that it works even if there is no numeric columns
  table <- plot_summary_table(curve_summary_grp)

  vdiffr::expect_doppelganger("character summary table", grid::grid.draw(table))
})

test_that("Able to return NULL when there is no summary for one curve group", {
  curve_summary_grp <- data.frame()

  # Test that it gives NULL when both columns types are missing

  testthat::expect_null(
    plot_summary_table(curve_summary_grp)
  )

  testthat::expect_null(
    plot_summary_table(NA)
  )

  testthat::expect_null(
    plot_summary_table(NULL)
  )

})

test_that("Test Regression Colour Vector Output", {

  regression_colour_vector_both_true <- create_reg_col_vec(
    plot_first_half_lin_reg = TRUE,
    plot_last_half_lin_reg = TRUE
  )

  regression_colour_vector_first_true <- create_reg_col_vec(
    plot_first_half_lin_reg = TRUE,
    plot_last_half_lin_reg = FALSE
  )

  regression_colour_vector_last_true <- create_reg_col_vec(
    plot_first_half_lin_reg = FALSE,
    plot_last_half_lin_reg = TRUE
  )

  regression_colour_vector_both_false <- create_reg_col_vec(
    plot_first_half_lin_reg = FALSE,
    plot_last_half_lin_reg = FALSE
  )

  correct_colour_vector_both_true <- c(Lin = "black", Quad = "red",
                                       `Lin First Half` = "blue",
                                       `Lin Last Half` = "purple")

  correct_colour_vector_first_true <- c(Lin = "black", Quad = "red",
                                       `Lin First Half` = "blue")

  correct_colour_vector_last_true <- c(Lin = "black", Quad = "red",
                                       `Lin Last Half` = "purple")

  correct_colour_vector_both_false <- c(Lin = "black", Quad = "red")

  testthat::expect_mapequal(
    regression_colour_vector_both_true,
    correct_colour_vector_both_true)

  testthat::expect_mapequal(
    regression_colour_vector_first_true,
    correct_colour_vector_first_true)

  testthat::expect_mapequal(
    regression_colour_vector_last_true,
    correct_colour_vector_last_true)

  testthat::expect_mapequal(
    regression_colour_vector_both_false,
    correct_colour_vector_both_false)

})

test_that("dilution_summary, dilution_summary_grp argument in plot_summary_table related functions are deprecated", {

  wf1_group <- c("Poor Linearity")

  wf2_group <- c("Saturation")

  r_corr <- c(0.951956)

  pra_linear <- c(65.78711)

  mandel_p_val <- c(2.899006e-07)

  concavity <- c(-4133.501328)

  curve_summary_grp <- data.frame(
    wf1_group = wf1_group,
    wf2_group = wf2_group,
    r_corr = r_corr,
    pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity
  )

  testthat::expect_snapshot({
    table <- plot_summary_table_char(
      dilution_summary_grp = curve_summary_grp)
  })

  testthat::expect_snapshot({
    table <- plot_summary_table_num(
      dilution_summary_grp = curve_summary_grp)
  })

  testthat::expect_snapshot({
    table <- plot_summary_table(
      dilution_summary_grp = curve_summary_grp)
  })

})

test_that("dilution_data, dilution_summary_grp and dil_batch_var argument in plot_curve_ggplot are deprecated", {

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

 curve_data <- tibble::tibble(
   Sample_Name = sample_name,
   Curve_Batch_Name = curve_batch_name,
   Concentration = concentration,
   Curve_Name = curve_name,
   Signal = curve_1_saturation_regime,
 )

 grouping_variable <- c("Curve_Name", "Curve_Batch_Name")

 # Get the curve batch name from curve_table
 curve_batch_name <- curve_batch_name %>%
   unique() %>%
   as.character()

 curve_batch_col <- c("#377eb8")

 # Create palette for each curve batch for plotting
 pal <- curve_batch_col %>%
   stats::setNames(curve_batch_name)

 # Create curve statistical summary
 curve_summary_grp <- curve_data %>%
   summarise_curve_table(
     grouping_variable = grouping_variable,
     conc_var = "Concentration",
     signal_var = "Signal"
   ) %>%
   evaluate_linearity(grouping_variable = grouping_variable) %>%
   dplyr::select(-c(dplyr::all_of(grouping_variable)))

 # Create the ggplot
 testthat::expect_snapshot({
   p <- plot_curve_ggplot(
     dilution_data = curve_data,
     dilution_summary_grp = curve_summary_grp,
     pal = pal,
     title = "Lipid_Saturated",
     dil_batch_var = "Curve_Batch_Name",
     conc_var = "Concentration",
     conc_var_units = "%",
     conc_var_interval = 50,
     signal_var = "Signal"
   )
 })

})

test_that("dilution_data, dilution_summary, dil_batch_var, dil_batch_col argument in add_ggplot_panel are deprecated", {

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
  curve_summary <- curve_table %>%
    summarise_curve_table(
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    dplyr::arrange(.data[["Curve_Name"]]) %>%
    evaluate_linearity(grouping_variable = c(
      "Curve_Name",
      "Curve_Batch_Name"
    ))

  # Create a ggplot table
  testthat::expect_snapshot({
    ggplot_table <- add_ggplot_panel(
      dilution_table = curve_table,
      dilution_summary = curve_summary,
      grouping_variable = c("Curve_Name",
                            "Curve_Batch_Name"),
      dil_batch_var = "Curve_Batch_Name",
      dil_batch_col = c("#377eb8"),
      conc_var = "Concentration",
      conc_var_units = "%",
      conc_var_interval = 50,
      signal_var = "Signal",
      have_plot_title = TRUE,
      plot_summary_table = TRUE,
      plot_first_half_lin_reg = FALSE,
      plot_last_half_lin_reg = FALSE
    )
  })

})
