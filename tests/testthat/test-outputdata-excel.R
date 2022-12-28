test_that("Test different pass criteria and equality in format_num_cell_colour", {

  r_corr <- c(
    0.951956, 0.948683, 0.978057, 0.976462,
    0.970618, 0.969348, 0.343838, 0.383552
  )

  pra_linear <- c(
    65.78711, 64.58687, 90.21257, 89.95473,
    72.91220, 72.36528, -233.05949, -172.13659
  )

  mandel_p_val <- c(
    2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
    3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02
  )

  concavity <- c(
    -4133.501328, -4146.745747, -3.350942, -3.393617,
    0.3942824, 0.4012963, -19.9469621, -22.6144875
  )

  curve_summary <- data.frame(
    r_corr = r_corr, pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity
  )

  curve_summary <- mark_near_zero_columns(curve_summary)

  # Create a new workbook
  my_workbook <- openxlsx::createWorkbook()

  # Create a new worksheet
  openxlsx::addWorksheet(wb = my_workbook, sheetName = "Curve Summary")

  # Write to worksheet as an Excel Table
  openxlsx::writeDataTable(
    wb = my_workbook, sheet = "Curve Summary",
    x = curve_summary,
    withFilter = TRUE,
    bandedRows = FALSE
  )

  # Conditional formatting can only be done
  # after data is written to excel sheet
  format_num_cell_colour(
    workbook = my_workbook, sheet = "Curve Summary",
    curve_summary = curve_summary,
    conditional_column = "r_corr",
    threshold_value = "0.8",
    pass_criteria = "above",
    pass_equality = TRUE
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[1]],
    "<cfRule type=\"expression\" dxfId=\"0\" priority=\"2\"><formula>A2&gt;=0.8</formula></cfRule>"
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[2]],
    "<cfRule type=\"expression\" dxfId=\"1\" priority=\"1\"><formula>A2&lt;0.8</formula></cfRule>"
  )

  format_num_cell_colour(
    workbook = my_workbook, sheet = "Curve Summary",
    curve_summary = curve_summary,
    conditional_column = "r_corr",
    threshold_value = "0.8",
    pass_criteria = "above",
    pass_equality = FALSE
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[1]],
    "<cfRule type=\"expression\" dxfId=\"0\" priority=\"4\"><formula>A2&gt;=0.8</formula></cfRule>"
    )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[2]],
    "<cfRule type=\"expression\" dxfId=\"1\" priority=\"3\"><formula>A2&lt;0.8</formula></cfRule>"
    )

  format_num_cell_colour(
    workbook = my_workbook, sheet = "Curve Summary",
    curve_summary = curve_summary,
    conditional_column = "r_corr",
    threshold_value = "0.8",
    pass_criteria = "below",
    pass_equality = TRUE
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[1]],
    "<cfRule type=\"expression\" dxfId=\"0\" priority=\"6\"><formula>A2&gt;=0.8</formula></cfRule>"
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[2]],
    "<cfRule type=\"expression\" dxfId=\"1\" priority=\"5\"><formula>A2&lt;0.8</formula></cfRule>"
  )

  format_num_cell_colour(
    workbook = my_workbook, sheet = "Curve Summary",
    curve_summary = curve_summary,
    conditional_column = "r_corr",
    threshold_value = "0.8",
    pass_criteria = "below",
    pass_equality = FALSE
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[1]],
    "<cfRule type=\"expression\" dxfId=\"0\" priority=\"8\"><formula>A2&gt;=0.8</formula></cfRule>"
  )

  testthat::expect_equal(
    my_workbook$worksheets[[1]]$conditionalFormatting[[2]],
    "<cfRule type=\"expression\" dxfId=\"1\" priority=\"7\"><formula>A2&lt;0.8</formula></cfRule>"
  )

})

test_that("Able to print curve summary data to excel", {
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
    "Sample_010a", "Sample_020a", "Sample_025a",
    "Sample_040a", "Sample_050a", "Sample_060a",
    "Sample_075a", "Sample_080a", "Sample_100a",
    "Sample_125a", "Sample_150a",
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

  # Create curve table and statistical summary
  curve_summary <- create_curve_table(
    curve_batch_annot = curve_batch_annot,
    curve_data_wide = curve_data,
    common_column = "Sample_Name",
    signal_var = "Signal",
    column_group = "Curve_Name"
  ) %>%
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

  # Testing if the change in class for near zero column works
  class_change_check <- curve_summary %>%
    mark_near_zero_columns() %>%
    purrr::map_chr(class)

  testthat::expect_equal(
    "scientific",
    unname(class_change_check["mandel_p_val"])
  )

  # Output to excel
  testthat::expect_silent(
    write_summary_excel(curve_summary, file_name = "dilution_summary.xlsx")
  )

  if (file.exists("dilution_summary.xlsx")) {
    # Delete file if it exists
    file.remove("dilution_summary.xlsx")
  }
})

test_that("Argument dilution summary is deprecated", {

  r_corr <- c(
    0.951956, 0.948683, 0.978057, 0.976462,
    0.970618, 0.969348, 0.343838, 0.383552
  )

  pra_linear <- c(
    65.78711, 64.58687, 90.21257, 89.95473,
    72.91220, 72.36528, -233.05949, -172.13659
  )

  mandel_p_val <- c(
    2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
    3.195779e-08, 6.366588e-08, 3.634004e-02, 1.864090e-02
  )

  concavity <- c(
    -4133.501328, -4146.745747, -3.350942, -3.393617,
    0.3942824, 0.4012963, -19.9469621, -22.6144875
  )

  curve_summary <- data.frame(
    r_corr = r_corr, pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity
  )

  testthat::expect_snapshot({
    column_max_char <- calculate_column_max_char(
      dilution_summary = curve_summary)
  })

  testthat::expect_snapshot({
    curve_summary <- mark_near_zero_columns(
      dilution_summary = curve_summary)
  })

  testthat::expect_snapshot({
    column_max_char <- mark_near_zero_columns(
      dilution_summary = curve_summary)
  })

  my_workbook <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb = my_workbook, sheetName = "Curve Summary")

  testthat::expect_snapshot({
    format_num_cell_style(
      dilution_summary = curve_summary,
      workbook = my_workbook,
      sheet = "Curve Summary"
    )
  })

  testthat::expect_snapshot({
    format_char_cell_colour(
      dilution_summary = curve_summary,
      workbook = my_workbook,
      sheet = "Curve Summary",
      conditional_column = "wf1_group",
      pass_criteria_words = c("Good Linearity")
    )
  })

  testthat::expect_snapshot({
    format_num_cell_colour(
      dilution_summary = curve_summary,
      workbook = my_workbook,
      sheet = "Curve Summary",
      conditional_column = "r_corr",
      threshold_value = "0.8",
      pass_criteria = "above"
    )
  })

})
