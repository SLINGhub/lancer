test_that("create dilution table and statistical summary", {

  #Data Creation
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  dilution_batch <- c(1, 1, 1, 1, 1, 1)
  sample_name <- c("Sample_010", "Sample_020", "Sample_040",
                   "Sample_060", "Sample_080", "Sample_100")
  lipid1_area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  lipid2_area <- c(2299075, 4136350, 7020062, 8922063, 9288742, 11365710)

  dilution_annot <- tibble::tibble(Sample_Name = sample_name,
                                        Dilution_Batch = dilution_batch,
                                        Dilution_Percent = dilution_percent)
  lipid_data <- tibble::tibble(Sample_Name = sample_name,
                               Lipid1 = lipid1_area,
                               Lipid2 = lipid2_area)

  # Create dilution table
  dil_data <- create_dil_table(dilution_annot,lipid_data,
                               common_column = "Sample_Name",
                               signal_var = "Area",
                               column_group = "Transition_Name")


  # Get dilution statistical summary
  dil_summary <- dil_data %>%
    dplyr::group_by(.data$Transition_Name,.data$Dilution_Batch) %>%
    tidyr::nest() %>%
    dplyr::mutate(dil_summary = purrr::map(.data$data, get_dilution_summary,
                                           conc_var = "Dilution_Percent",
                                           signal_var = "Area")) %>%
    tidyr::unnest(.data$dil_summary) %>%
    dplyr::select(-c("data")) %>%
    dplyr::arrange(.data$Transition_Name)


})
