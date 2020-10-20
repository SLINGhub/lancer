test_that("create dilution table and statistical summary", {

  #Data Creation
  dilution_percent <- c(10, 20, 40, 60, 80, 100,
                        10, 20, 40, 60, 80, 100)
  dilution_batch <- c("B1", "B1", "B1", "B1", "B1", "B1",
                      "B2", "B2", "B2", "B2", "B2", "B2")
  sample_name <- c("Sample_010a", "Sample_020a", "Sample_040a",
                   "Sample_060a", "Sample_080a", "Sample_100a",
                   "Sample_010b", "Sample_020b", "Sample_040b",
                   "Sample_060b", "Sample_080b", "Sample_100b")
  lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410,
                   32561, 41178, 49981, 58390, 62171, 63410)
  lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
                   2300075, 4137350, 7021062, 8923063, 9289742, 11366710)

  dilution_annot <- tibble::tibble(Sample_Name = sample_name,
                                        Dilution_Batch = dilution_batch,
                                        Dilution_Percent = dilution_percent)
  lipid_data <- tibble::tibble(Sample_Name = sample_name,
                               Lipid1 = lipid1_area,
                               Lipid2 = lipid2_area)
  bad_dilution_annot <- tibble::tibble(Sample_Nam = sample_name,
                                       Dilution_Batch = dilution_batch,
                                       Dilution_Percent = dilution_percent)
  bad_lipid_data <- tibble::tibble(Sample_Nam = sample_name,
                                   Lipid1 = lipid1_area,
                                   Lipid2 = lipid2_area)

  # Create dilution table
  dil_data <- create_dilution_table(dilution_annot, lipid_data,
                                    common_column = "Sample_Name",
                                    signal_var = "Area",
                                    column_group = "Transition_Name")

  testthat::expect_error(create_dilution_table(dilution_annot, lipid_data,
                                              common_column = "Sample_Nam")
  )
  testthat::expect_error(create_dilution_tablee(bad_dilution_annot, lipid_data,
                                                common_column = "Sample_Name")
  )
  testthat::expect_error(create_dilution_table(dilution_annot, bad_lipid_data,
                                               common_column = "Sample_Name")
  )


  # Get dilution statistical summary
  dil_summary <- dil_data %>%
    dplyr::group_by(.data$Transition_Name, .data$Dilution_Batch) %>%
    tidyr::nest() %>%
    dplyr::mutate(dil_summary = purrr::map(.data$data, get_dilution_summary,
                                           conc_var = "Dilution_Percent",
                                           signal_var = "Area")) %>%
    tidyr::unnest(.data$dil_summary) %>%
    dplyr::select(-c("data")) %>%
    dplyr::arrange(.data$Transition_Name)


})
