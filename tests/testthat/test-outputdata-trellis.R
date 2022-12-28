test_that("Able to plot curve data with
          its statistical summary in a trellis table", {

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

  # Create a plotly trellis table with curve_batch_var
  # as a grouping variable
  plotly_trellis_table <- add_plotly_panel(
    curve_table,
    curve_summary = curve_summary,
    grouping_variable = c(
      "Curve_Name",
      "Curve_Batch_Name"
    ),
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  ) %>%
    convert_to_cog(
      cog_df = NULL,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      panel_variable = "panel",
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    )

  # Check if trellis_table is valid
  testthat::expect_silent(validate_trellis_table(plotly_trellis_table))

  # Create a plotly trellis table with curve_batch_var
  # not as a grouping variable
  curve_table_filtered <- curve_table %>%
    dplyr::filter(.data[["Curve_Batch_Name"]] == "B2")

  curve_summary_filtered <- curve_summary %>%
    dplyr::filter(.data[["Curve_Batch_Name"]] == "B2")

  plotly_no_curve_batch_var <- add_plotly_panel(
    curve_table = curve_table_filtered,
    curve_summary = curve_summary_filtered,
    grouping_variable = c(
      "Curve_Name"
    ),
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  ) %>%
    convert_to_cog(
      cog_df = NULL,
      grouping_variable = c(
        "Curve_Name"
      ),
      panel_variable = "panel",
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    )

  # Check if trellis_table is valid
  testthat::expect_silent(
    validate_trellis_table(
      trellis_table = plotly_no_curve_batch_var,
      grouping_variable = c(
        "Curve_Name")
      ))

  # Create a trellis table without dilution summary
  plotly_trellis_table_no_summary <- add_plotly_panel(
    curve_table,
    grouping_variable = c(
      "Curve_Name",
      "Curve_Batch_Name"
    ),
    sample_name_var = "Sample_Name",
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
    ) %>%
    convert_to_cog(
      cog_df = NULL,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      panel_variable = "panel",
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    )

  # Check if trellis_table_no_summary is valid
  testthat::expect_silent(validate_trellis_table(plotly_trellis_table_no_summary))

  # Validating bad inputs

  # Non grouping variable is not a
  # cognostic class, other than panel
  invalid_trellis_table <- plotly_trellis_table %>%
    dplyr::mutate(pra_linear = as.numeric(.data$pra_linear))

  testthat::expect_error(validate_trellis_table(invalid_trellis_table))

  # Non grouping variable in the wrong cognostics group
  invalid_trellis_table <- plotly_trellis_table

  attributes(invalid_trellis_table[["pra_linear"]])$cog_attrs$group <-
    "condVar"

  testthat::expect_error(validate_trellis_table(invalid_trellis_table))

  # No grouping variable input
  invalid_trellis_table <- plotly_trellis_table

  testthat::expect_error(
    validate_trellis_table(invalid_trellis_table,
                           grouping_variable = c())
  )

  # Grouping variable is not a
  # cognostic class, other than panel
  invalid_trellis_table <- plotly_trellis_table %>%
    dplyr::mutate(Curve_Name = as.character(.data$Curve_Name))

  testthat::expect_error(validate_trellis_table(invalid_trellis_table))

  # Grouping variable in the wrong cognostics group
  invalid_trellis_table <- plotly_trellis_table

  attributes(invalid_trellis_table[["Curve_Name"]])$cog_attrs$group <-
    "common"

  testthat::expect_error(validate_trellis_table(invalid_trellis_table))

  # Panel column is invalid
  invalid_trellis_table <- plotly_trellis_table
  class(invalid_trellis_table$panel) <- "list"
  testthat::expect_error(validate_trellis_table(invalid_trellis_table))

  # panel_variable cannot be grouping_variable
  testthat::expect_error(
    plotly_trellis_table_bad_panal_variable <- add_plotly_panel(
      curve_table,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      sample_name_var = "Sample_Name",
      curve_batch_var = "Curve_Batch_Name",
      conc_var = "Concentration",
      conc_var_units = "%",
      conc_var_interval = 50,
      signal_var = "Signal"
      ) %>%
      convert_to_cog(
        cog_df = NULL,
        grouping_variable = c("Curve_Name",
                              "Curve_Batch_Name"),
        panel_variable = "Curve_Name",
        col_name_vec = "col_name_vec",
        desc_vec = "desc_vec",
        type_vec = "type_vec"
      )
  )

  # Create the trellis report in plotly
  view_trellis_html(plotly_trellis_table,
    trellis_report_name = "Curve_Plot_Plotly",
    trellis_report_folder = "Curve_Plot"
  )


  # Create a ggplot trellis table
  ggplot_trellis_table <- add_ggplot_panel(
    curve_table,
    curve_summary = curve_summary,
    grouping_variable = c("Curve_Name",
                          "Curve_Batch_Name"),
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal",
    have_plot_title = FALSE,
    plot_summary_table = FALSE
  ) %>%
    convert_to_cog(
      cog_df = NULL,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      panel_variable = "panel",
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    )

  # Create the trellis report in ggplot
  view_trellis_html(ggplot_trellis_table,
    trellis_report_name = "Curve_Plot_Ggplot",
    trellis_report_folder = "Curve_Plot"
  )


  # Check if convert_to_cog and view_trellis_html works
  # if input panel_variable is not called "panel"
  different_panel_table <- add_plotly_panel(
    curve_table,
    curve_summary = curve_summary,
    grouping_variable = c("Curve_Name",
                          "Curve_Batch_Name"),
    curve_batch_var = "Curve_Batch_Name",
    conc_var = "Concentration",
    conc_var_units = "%",
    conc_var_interval = 50,
    signal_var = "Signal"
  ) %>%
    dplyr::rename(paneldiff = panel) %>%
    convert_to_cog(
      cog_df = NULL,
      grouping_variable = c(
        "Curve_Name",
        "Curve_Batch_Name"
      ),
      panel_variable = "paneldiff",
      col_name_vec = "col_name_vec",
      desc_vec = "desc_vec",
      type_vec = "type_vec"
    ) %>%
    view_trellis_html(
      trellis_report_name = "Curve_Plot_Different",
      trellis_report_folder = "Curve_Plot",
      panel_variable = "paneldiff"
    )

  # Check if convert_to_cog and view_trellis_html works
  # if we have only one grouping variable
  view_trellis_html(plotly_no_curve_batch_var,
                    trellis_report_name = "Curve_Plot_One_Group",
                    trellis_report_folder = "Curve_Plot",
                    grouping_variable = c("Curve_Name")
  )

  # Check if convert_to_cog and view_trellis_html works
  # if we add additional labels
  view_trellis_html(plotly_no_curve_batch_var,
                    trellis_report_name = "Curve_Plot_Additional_Group",
                    trellis_report_folder = "Curve_Plot",
                    grouping_variable = c("Curve_Name"),
                    trellis_additional_labels = c("Curve_Batch_Name")
  )

  unlink("Curve_Plot", recursive = TRUE)
})
