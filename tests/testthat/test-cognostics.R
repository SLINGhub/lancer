test_that("dilution_summary argument is deprecated", {

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

  # Create our own cog_df
  col_name_vec <- c(
    "Curve_Name", "Curve_Batch_Name",
    "Curve_Class"
  )

  desc_vec <- c(
    "Curve_Name", "Curve_Batch_Name",
    "Curve_Class"
  )

  type_vec <- c("factor", "factor", "factor")

  cog_df <- data.frame(
    col_name_vec = col_name_vec,
    desc_vec = desc_vec,
    type_vec = type_vec
  )

  testthat::expect_snapshot({
    updated_summary <- update_cog_manual(
      dilution_summary = curve_summary,
      cog_df,
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    )
  })

  testthat::expect_snapshot({
    updated_summary <- update_cog_auto(
      dilution_summary = curve_summary
    )
  })

  testthat::expect_snapshot({
    updated_summary <- convert_to_cog(
      dilution_summary = curve_summary,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      )
    )
  })

})
