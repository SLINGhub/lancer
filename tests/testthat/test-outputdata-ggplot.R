test_that("Able to plot curve data with its
          statistical summary in a pdf report", {

  # Data Creation
  concentration <- c(
    10, 20, 25, 40, 50, 60,
    75, 80, 100, 125, 150,
    10, 25, 40, 50, 60,
    75, 80, 100, 125, 150
  )

  curve_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1",
    "B1", "B1", "B1", "B1", "B1", "B1",
    "B2", "B2", "B2", "B2", "B2",
    "B2", "B2", "B2", "B2", "B2"
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a",
    "Sample_025a", "Sample_040a", "Sample_050a",
    "Sample_060a", "Sample_075a", "Sample_080a",
    "Sample_100a", "Sample_125a", "Sample_150a",
    "Sample_010b", "Sample_025b",
    "Sample_040b", "Sample_050b", "Sample_060b",
    "Sample_075b", "Sample_080b", "Sample_100b",
    "Sample_125b", "Sample_150b"
  )

  curve_1_saturation_regime <- c(
    5748124, 16616414, 21702718, 36191617,
    49324541, 55618266, 66947588, 74964771,
    75438063, 91770737, 94692060,
    5192648, 16594991, 32507833, 46499896,
    55388856, 62505210, 62778078, 72158161,
    78044338, 86158414
  )

  curve_2_good_linearity <- c(
    31538, 53709, 69990, 101977, 146436, 180960,
    232881, 283780, 298289, 344519, 430432,
    25463, 63387, 90624, 131274, 138069,
    205353, 202407, 260205, 292257, 367924
  )

  curve_3_noise_regime <- c(
    544, 397, 829, 1437, 1808, 2231,
    3343, 2915, 5268, 8031, 11045,
    500, 903, 1267, 2031, 2100,
    3563, 4500, 5300, 8500, 10430
  )

  curve_4_poor_linearity <- c(
    380519, 485372, 478770, 474467, 531640, 576301,
    501068, 550201, 515110, 499543, 474745,
    197417, 322846, 478398, 423174, 418577,
    426089, 413292, 450190, 415309, 457618
  )

  curve_batch_annot <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration
  )

  curve_data <- tibble::tibble(
    Sample_Name = sample_name,
    `Curve_1` = curve_1_saturation_regime,
    `Curve_2` = curve_2_good_linearity,
    `Curve_3` = curve_3_noise_regime,
    `Curve_4` = curve_4_poor_linearity
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

  # Create a ggplot table with curv_batch_var
  # as a grouping variable
  testthat::expect_silent(
    ggplot_table <- add_ggplot_panel(
      curve_table,
      curve_summary = curve_summary,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      curve_batch_var = "Curve_Batch_Name",
      conc_var = "Concentration",
      signal_var = "Signal",
      plot_first_half_lin_reg = FALSE
    )
  )

  # Create a ggplot table with curve_batch_var
  # not as a grouping variable
  testthat::expect_silent(
    ggplot_table_no_curve_batch_var <- add_ggplot_panel(
      curve_table,
      curve_summary = curve_summary,
      grouping_variable = c(
        "Curve_Name"
      ),
      curve_batch_var = "Curve_Batch_Name",
      conc_var = "Concentration",
      signal_var = "Signal",
      plot_first_half_lin_reg = FALSE
    )
  )

  # Create a ggplot table without curve summary
  testthat::expect_silent(
    ggplot_table_auto <- add_ggplot_panel(
      curve_table,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      curve_batch_var = "Curve_Batch_Name",
      conc_var = "Concentration",
      signal_var = "Signal",
      plot_first_half_lin_reg = FALSE
    )
  )

  # Get the list of ggplot list for each group
  ggplot_list <- ggplot_table$panel

  # Create a pdf report, set testing = FALSE to output results
  testthat::expect_silent(
    view_ggplot_pdf(ggplot_list,
                    filename = "curve_plot.pdf",
                    ncol_per_page = 2,
                    nrow_per_page = 2,
                    testing = FALSE
    )
  )

  # Expect number of columns and rows per page to be two
  pdf_page_list <- view_ggplot_pdf(
    ggplot_list,
    filename = "curve_plot.pdf",
    ncol_per_page = 2,
    nrow_per_page = 2,
    testing = TRUE
  )

  testthat::expect_mapequal(
    c(ncol = pdf_page_list$`1`$patches$layout$ncol,
      nrow = pdf_page_list$`1`$patches$layout$nrow),
    c(ncol = 2,
      nrow = 2)
  )

  # Expect number of rows per page to be two
  pdf_page_list <- view_ggplot_pdf(
    ggplot_list,
    filename = "curve_plot.pdf",
    ncol_per_page = NULL,
    nrow_per_page = 2,
    testing = TRUE
  )

  testthat::expect_mapequal(
    c(ncol = pdf_page_list$`1`$patches$layout$ncol,
      nrow = pdf_page_list$`1`$patches$layout$nrow),
    c(ncol = NULL,
      nrow = 2)
  )

  # Expect number of cols per page to be two
  pdf_page_list <- view_ggplot_pdf(
    ggplot_list,
    filename = "curve_plot.pdf",
    ncol_per_page = 2,
    nrow_per_page = NULL,
    testing = TRUE
  )

  testthat::expect_mapequal(
    c(ncol = pdf_page_list$`1`$patches$layout$ncol,
      nrow = pdf_page_list$`1`$patches$layout$nrow),
    c(ncol = 2,
      nrow = NULL)
  )

  # Create a pdf report, when
  # ncol_per_page * nrow_per_page exceeds the number of plots
  # in the list.
  # Program should output all in one pdf page.
  # set testing = FALSE to output results
  pdf_page_list <- view_ggplot_pdf(
    ggplot_list,
    filename = "curve_plot.pdf",
    ncol_per_page = 3,
    nrow_per_page = 3,
    width = 20,
    height = 12,
    testing = TRUE
  )

  testthat::expect_mapequal(
    c(ncol = pdf_page_list[[1]]$patches$layout$ncol,
      nrow = pdf_page_list[[1]]$patches$layout$nrow),
    c(ncol = 3,
      nrow = 3)
  )

  if (isTRUE(file.exists("curve_plot.pdf"))) {
    # Delete file if it exists
    testthat::expect_true(file.remove("curve_plot.pdf"))
  }
})

test_that("Get the page layout correctly with various inputs", {

  # Give error if number of plots is less than 1
  testthat::expect_error(create_page_layout(number_of_plots = 0))

  # Set nrow to be number_of_plots if ncol is 1
  page_layout <- create_page_layout(
    number_of_plots = 5,
    ncol = 1, nrow = NULL
  )
  correct_page_layout <- list(ncol = 1, nrow = 5)

  for (key in names(page_layout)) {
    testthat::expect_equal(
      page_layout[[key]],
      correct_page_layout[[key]]
    )
  }

  # Set ncol to be number_of_plots if nrow is 1
  page_layout <- create_page_layout(
    number_of_plots = 5,
    ncol = NULL, nrow = 1
  )
  correct_page_layout <- list(ncol = 5, nrow = 1)

  for (key in names(page_layout)) {
    testthat::expect_equal(
      page_layout[[key]],
      correct_page_layout[[key]]
    )
  }

  # Set ncol, nrow to be user input
  page_layout <- create_page_layout(
    number_of_plots = 5,
    ncol = 2, nrow = 2
  )
  correct_page_layout <- list(ncol = 2, nrow = 2)

  for (key in names(page_layout)) {
    testthat::expect_equal(
      page_layout[[key]],
      correct_page_layout[[key]]
    )
  }
})
