test_that("Able to print dilution summary data to excel", {
  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150,
                        10, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  dilution_batch <- c("B1", "B1", "B1", "B1", "B1",
                      "B1", "B1", "B1", "B1", "B1", "B1",
                      "B2", "B2", "B2", "B2", "B2",
                      "B2", "B2", "B2", "B2", "B2")
  sample_name <- c("Sample_010a", "Sample_020a", "Sample_025a",
                   "Sample_040a", "Sample_050a", "Sample_060a",
                   "Sample_075a", "Sample_080a", "Sample_100a",
                   "Sample_125a", "Sample_150a",
                   "Sample_010a", "Sample_025a",
                   "Sample_040a", "Sample_050a", "Sample_060a",
                   "Sample_075a", "Sample_080a", "Sample_100a",
                   "Sample_125a", "Sample_150a")
  lipid1_area_saturated <- c(5748124, 16616414, 21702718, 36191617,
                             49324541, 55618266, 66947588, 74964771,
                             75438063, 91770737, 94692060,
                             5192648, 16594991, 32507833, 46499896,
                             55388856, 62505210, 62778078, 72158161,
                             78044338, 86158414)
  lipid2_area_linear <- c(31538, 53709, 69990, 101977, 146436, 180960,
                          232881, 283780, 298289, 344519, 430432,
                          25463, 63387, 90624, 131274, 138069,
                          205353, 202407, 260205, 292257, 367924)
  lipid3_area_lod <- c(544, 397, 829, 1437, 1808, 2231,
                       3343, 2915, 5268, 8031, 11045,
                       500, 903, 1267, 2031, 2100,
                       3563, 4500, 5300, 8500, 10430)
  lipid4_area_nonlinear <- c(380519, 485372, 478770, 474467, 531640, 576301,
                             501068, 550201, 515110, 499543, 474745,
                             197417, 322846, 478398, 423174, 418577,
                             426089, 413292, 450190, 415309, 457618)

  dilution_annot <- tibble::tibble(Sample_Name = sample_name,
                                   Dilution_Batch = dilution_batch,
                                   Dilution_Percent = dilution_percent)
  lipid_data <- tibble::tibble(Sample_Name = sample_name,
                               Lipid1 = lipid1_area_saturated,
                               Lipid2 = lipid2_area_linear,
                               Lipid3 = lipid3_area_lod,
                               Lipid4 = lipid4_area_nonlinear)

  # Create dilution table and dilution statistical summary
  dilution_summary <- create_dilution_table(dilution_annot, lipid_data,
                                            common_column = "Sample_Name",
                                            signal_var = "Area",
                                            column_group = "Transition_Name"
                                            ) %>%
    summarise_dilution_table(grouping_variable = c("Transition_Name",
                                                   "Dilution_Batch"),
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    dplyr::arrange(.data$Transition_Name)

  # Testing if the change in class for near zero column works
  class_change_check <- dilution_summary %>%
    mark_near_zero_columns() %>%
    purrr::map_chr(class)

  testthat::expect_equal("scientific",
                         unname(class_change_check["mandel_p_val"]))

  #Output to excel
  create_excel_report(dilution_summary, file_name = "dilution_summary.xlsx",
                      testing = TRUE)


})
