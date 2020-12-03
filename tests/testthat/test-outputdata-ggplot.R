test_that("Able to plot dilution data with its statistical summary in a pdf report", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150,
                        10, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  dilution_batch <- c("B1", "B1", "B1", "B1", "B1",
                      "B1", "B1", "B1", "B1", "B1", "B1",
                      "B2", "B2", "B2", "B2", "B2",
                      "B2", "B2", "B2", "B2", "B2")
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a",
                   "Sample_010b", "Sample_025b",
                   "Sample_040b", "Sample_050b", "Sample_060b",
                   "Sample_075b", "Sample_080b", "Sample_100b",
                   "Sample_125b", "Sample_150b")
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


  # Create dilution table
  dilution_table <- create_dilution_table(dilution_annot, lipid_data,
                                          common_column = "Sample_Name",
                                          signal_var = "Area",
                                          column_group = "Transition_Name"
  )

  # Create dilution table and dilution statistical summary
  dilution_summary <- dilution_table %>%
    summarise_dilution_table(grouping_variable = c("Transition_Name",
                                                   "Dilution_Batch"),
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    dplyr::arrange(.data$Transition_Name) %>%
    evaluate_linearity(grouping_variable = c("Transition_Name",
                                             "Dilution_Batch"))


  # Create a ggplot table
  testthat::expect_silent(
    ggplot_table <- create_ggplot_table(dilution_table,
                                        dilution_summary = dilution_summary)
  )

  # Create a ggplot table without dilution summary
  testthat::expect_silent(
    ggplot_table_auto <- create_ggplot_table(dilution_table,
                                             grouping_variable = c("Transition_Name",
                                                                   "Dilution_Batch"),
                                             conc_var = "Dilution_Percent",
                                             signal_var = "Area")
  )

  # Get the list of ggplot list for each group
  ggplot_list <- ggplot_table$panel


  # Create a pdf report, set testing = FALSE to output results
  testthat::expect_silent(
    create_ggplot_pdf_report(ggplot_list, testing = TRUE)
  )




})



test_that("Able to plot statistical summary grid table for one dilution group", {

  wf1_group <- c("Poor Linearity")
  wf2_group <- c("Saturation")
  r_corr <- c(0.951956)
  pra_linear <- c(65.78711)
  mandel_p_val <- c(2.899006e-07)
  concavity <- c(-4133.501328)
  logical <- TRUE

  dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
                                      wf2_group = wf2_group,
                                      logical = logical,
                                      r_corr = r_corr,
                                      pra_linear = pra_linear,
                                      mandel_p_val = mandel_p_val,
                                      concavity = concavity)

  # Test that it works in the usual case
  table <- dilution_summary_group_table(dilution_summary_grp)
  testthat::expect_equal(7,nrow(table))

  dilution_summary_grp  <- data.frame(r_corr = r_corr,
                                      pra_linear = pra_linear,
                                      mandel_p_val = mandel_p_val,
                                      concavity = concavity)
  testthat::expect_null(dilution_summary_char_table(dilution_summary_grp))

  # Test that it works even if there is no character or factor
  # or logical columns
  table <- dilution_summary_group_table(dilution_summary_grp)
  testthat::expect_equal(4,nrow(table))


  dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
                                      wf2_group = wf2_group)

  testthat::expect_null(dilution_summary_num_table(dilution_summary_grp))

  # Test that it works even if there is no numeric columns
  table <- dilution_summary_group_table(dilution_summary_grp)
  testthat::expect_equal(2,nrow(table))


  dilution_summary_grp <- data.frame()
  # Test that it gives NULL when both columns types are missing
  testthat::expect_null(dilution_summary_group_table(dilution_summary_grp))


})
