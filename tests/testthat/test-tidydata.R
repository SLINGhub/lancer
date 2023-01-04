test_that("create curve table and statistical summary", {

  # Data Creation
  concentration <- c(
    10, 20, 40, 60, 80, 100,
    10, 20, 40, 60, 80, 100
  )

  curve_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1", "B1",
    "B2", "B2", "B2", "B2", "B2", "B2"
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a", "Sample_040a",
    "Sample_060a", "Sample_080a", "Sample_100a",
    "Sample_010b", "Sample_020b", "Sample_040b",
    "Sample_060b", "Sample_080b", "Sample_100b"
  )

  curve_1_good_linearity <- c(
    22561, 31178, 39981, 48390, 52171, 53410,
    32561, 41178, 49981, 58390, 62171, 63410
  )

  curve_2_good_linearity <- c(
    2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
    2300075, 4137350, 7021062, 8923063, 9289742, 11366710
  )

  curve_batch_annot <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration
  )

  curve_data_wide <- tibble::tibble(
    Sample_Name = sample_name,
    `Curve_1` = curve_1_good_linearity,
    `Curve_2` = curve_2_good_linearity
  )

  bad_curve_batch_annot <- tibble::tibble(
    Sample_Nam = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration
  )

  bad_curve_data <- tibble::tibble(
    Sample_Nam = sample_name,
    `Curve_1` = curve_1_good_linearity,
    `Curve_2` = curve_2_good_linearity
  )

  # Create curve table
  curve_table <- create_curve_table(
    curve_batch_annot = curve_batch_annot,
    curve_data_wide = curve_data_wide,
    common_column = "Sample_Name",
    signal_var = "Signal",
    column_group = "Curve_Name"
  )

  # Get curve statistical summary
  curve_summary <- curve_table %>%
    summarise_curve_table(
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      conc_var = "Concentration",
      signal_var = "Signal"
    ) %>%
    dplyr::arrange(.data[["Curve_Name"]])

  # Validating bad inputs for create_curve_table
  testthat::expect_error(create_curve_table(curve_batch_annot, curve_data,
    common_column = "Sample_Nam"
  ))
  testthat::expect_error(create_curve_table(bad_curve_batch_annot, curve_data,
    common_column = "Sample_Name"
  ))
  testthat::expect_error(create_curve_table(curve_batch_annot, bad_curve_data,
    common_column = "Sample_Name"
  ))

  # Validating bad inputs for summarise_curve_table
  testthat::expect_error(
    summarise_curve_table(curve_table,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batc"
      ),
      conc_var = "Concentration",
      signal_var = "Signal"
    )
  )

  testthat::expect_error(
    summarise_curve_table(curve_table,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      conc_var = "Concentratio",
      signal_var = "Signal"
    )
  )

  testthat::expect_error(
    summarise_curve_table(curve_table,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      conc_var = "Concentration",
      signal_var = "Signa"
    )
  )
})

test_that("test deprecated functions", {

  concentration <- c(
    10, 20, 40, 60, 80, 100,
    10, 20, 40, 60, 80, 100
  )

  curve_batch_name <- c(
    "B1", "B1", "B1", "B1", "B1", "B1",
    "B2", "B2", "B2", "B2", "B2", "B2"
  )

  sample_name <- c(
    "Sample_010a", "Sample_020a", "Sample_040a",
    "Sample_060a", "Sample_080a", "Sample_100a",
    "Sample_010b", "Sample_020b", "Sample_040b",
    "Sample_060b", "Sample_080b", "Sample_100b"
  )

  curve_1_good_linearity <- c(
    22561, 31178, 39981, 48390, 52171, 53410,
    32561, 41178, 49981, 58390, 62171, 63410
  )

  curve_2_good_linearity <- c(
    2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
    2300075, 4137350, 7021062, 8923063, 9289742, 11366710
  )

  curve_batch_annot <- tibble::tibble(
    Sample_Name = sample_name,
    Curve_Batch_Name = curve_batch_name,
    Concentration = concentration
  )

  curve_data_wide <- tibble::tibble(
    Sample_Name = sample_name,
    `Curve_1` = curve_1_good_linearity,
    `Curve_2` = curve_2_good_linearity
  )

  # Create curve table
  curve_table <- create_curve_table(
    curve_batch_annot = curve_batch_annot,
    curve_data_wide = curve_data_wide,
    common_column = "Sample_Name",
    signal_var = "Signal",
    column_group = "Curve_Name"
  )

  testthat::expect_snapshot({
    validate_dilution_annot(
      dilution_annot = curve_batch_annot,
      needed_column = c("Sample_Name")
  )
  })

  testthat::expect_snapshot({
    validate_lipid_data_wide(
      lipid_data_wide = curve_data_wide,
      needed_column = c("Sample_Name")
    )
  })

  testthat::expect_snapshot({
    create_dilution_table(
      dilution_annot = curve_batch_annot,
      lipid_data_wide = curve_data_wide,
      common_column = c("Sample_Name"),
      signal_var = "Signal",
      column_group = "Curve_Name"
    )
  })

  testthat::expect_snapshot({
    validate_dilution_table(
      dilution_table = curve_table,
      needed_column = c("Curve_Name",
                        "Curve_Batch_Name",
                        "Concentration",
                        "Signal")
    )
  })

  testthat::expect_snapshot({
    summarise_dilution_table(
      dilution_table = curve_table,
      grouping_variable = c("Curve_Name",
                            "Curve_Batch_Name"
                            ),
      conc_var = "Concentration",
      signal_var = "Signal"
    )
  })

})
