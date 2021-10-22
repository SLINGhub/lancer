test_that("Able to plot good dilution data with ggplot correctly", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a")
  dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1",
                           "B1", "B1", "B1", "B1", "B1", "B1")
  transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1")
  lipid1_area_saturated <- c(5748124, 16616414, 21702718, 36191617,
                             49324541, 55618266, 66947588, 74964771,
                             75438063, 91770737, 94692060)

  dilution_data <- tibble::tibble(Sample_Name = sample_name,
                                   Dilution_Batch_Name = dilution_batch_name,
                                   Dilution_Percent = dilution_percent,
                                   Transition_Name = transition_name,
                                   Area = lipid1_area_saturated,
  )

  grouping_variable <- c("Transition_Name","Dilution_Batch_Name")

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col = c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)


  # Create dilution statistical summary
  dilution_summary_grp <- dilution_data %>%
    summarise_dilution_table(grouping_variable = grouping_variable,
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(dilution_data,
                            dilution_summary_grp = dilution_summary_grp,
                            title = "Lipid_Saturated",
                            pal = pal,
                            dil_batch_var = "Dilution_Batch_Name",
                            conc_var = "Dilution_Percent",
                            conc_var_units = "%",
                            conc_var_interval = 50,
                            signal_var = "Area",
                            plot_first_half_lin_reg = FALSE)

  vdiffr::expect_doppelganger("A usual dilution plot", p)


  p <- plot_curve_ggplot(dilution_data,
                         dilution_summary_grp = dilution_summary_grp,
                         title = "Lipid_Saturated",
                         pal = pal,
                         dil_batch_var = "Dilution_Batch_Name",
                         conc_var = "Dilution_Percent",
                         conc_var_units = "%",
                         conc_var_interval = 50,
                         signal_var = "Area",
                         plot_first_half_lin_reg = TRUE)

  vdiffr::expect_doppelganger("A Lin First Half dilution plot", p)

  p <- plot_curve_ggplot(dilution_data,
                         dilution_summary_grp = dilution_summary_grp,
                         title = "Lipid_Saturated",
                         pal = pal,
                         dil_batch_var = "Dilution_Batch_Name",
                         conc_var = "Dilution_Percent",
                         conc_var_units = "%",
                         conc_var_interval = 50,
                         signal_var = "Area",
                         plot_last_half_lin_reg = TRUE)

  vdiffr::expect_doppelganger("A Lin Last Half dilution plot", p)

  p <- plot_curve_ggplot(dilution_data,
                         dilution_summary_grp = dilution_summary_grp,
                         title = "Lipid_Saturated",
                         pal = pal,
                         dil_batch_var = "Dilution_Batch_Name",
                         conc_var = "Dilution_Percent",
                         conc_var_units = "%",
                         conc_var_interval = 50,
                         signal_var = "Area",
                         plot_first_half_lin_reg = TRUE,
                         plot_last_half_lin_reg = TRUE)

  vdiffr::expect_doppelganger("A Lin First and Last Half dilution plot", p)

})


test_that("Able to plot horizontal dilution data with ggplot correctly", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a")
  dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1",
                           "B1", "B1", "B1", "B1", "B1", "B1")
  transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1")
  bad_area <- c(2,2,2,2,2,2,
                2,2,2,2,2)

  # Handle the case of horizontal line
  dilution_data <- tibble::tibble(Sample_Name = sample_name,
                                  Dilution_Batch_Name = dilution_batch_name,
                                  Dilution_Percent = dilution_percent,
                                  Transition_Name = transition_name,
                                  Area = bad_area,
  )

  grouping_variable <- c("Transition_Name","Dilution_Batch_Name")

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col = c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)

  # Create dilution statistical summary
  dilution_summary_grp <- dilution_data %>%
    summarise_dilution_table(grouping_variable = grouping_variable,
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(dilution_data,
                            dilution_summary_grp = dilution_summary_grp,
                            title = "Lipid_Horizontal",
                            pal = pal,
                            dil_batch_var = "Dilution_Batch_Name",
                            conc_var = "Dilution_Percent",
                            conc_var_units = "%",
                            conc_var_interval = 50,
                            signal_var = "Area",
                            plot_first_half_lin_reg = FALSE)

  vdiffr::expect_doppelganger("A horizontal dilution plot", p)

})


test_that("Able to plot vertical dilution data with ggplot correctly", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a")
  dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1",
                           "B1", "B1", "B1", "B1", "B1", "B1")
  transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1")
  lipid1_area_saturated <- c(5748124, 16616414, 21702718, 36191617,
                             49324541, 55618266, 66947588, 74964771,
                             75438063, 91770737, 94692060)
  bad_conc <- c(50,50,50,50,50,50,
                50,50,50,50,50)

  # Handle the case of vertical line
  dilution_data <- tibble::tibble(Sample_Name = sample_name,
                                  Dilution_Batch_Name = dilution_batch_name,
                                  Dilution_Percent = bad_conc,
                                  Transition_Name = transition_name,
                                  Area = lipid1_area_saturated,
  )

  grouping_variable <- c("Transition_Name","Dilution_Batch_Name")

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col = c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)

  # Create dilution statistical summary
  dilution_summary_grp <- dilution_data %>%
    summarise_dilution_table(grouping_variable = grouping_variable,
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))

  # Create the ggplot
  p <- plot_curve_ggplot(dilution_data,
                            dilution_summary_grp = dilution_summary_grp,
                            title = "Lipid_Vertical",
                            pal = pal,
                            dil_batch_var = "Dilution_Batch_Name",
                            conc_var = "Dilution_Percent",
                            conc_var_units = "%",
                            conc_var_interval = 50,
                            signal_var = "Area",
                            plot_first_half_lin_reg = FALSE)

  vdiffr::expect_doppelganger("A vertical dilution plot", p)

})


test_that("Able to plot single point dilution data with ggplot correctly", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a")
  dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1",
                           "B1", "B1", "B1", "B1", "B1", "B1")
  transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1")
  bad_area <- c(2,2,2,2,2,2,
                2,2,2,2,2)
  bad_conc <- c(50,50,50,50,50,50,
                50,50,50,50,50)

  # Handle the case of a point
  dilution_data <- tibble::tibble(Sample_Name = sample_name,
                                  Dilution_Batch_Name = dilution_batch_name,
                                  Dilution_Percent = bad_conc,
                                  Transition_Name = transition_name,
                                  Area = bad_area,
  )

  grouping_variable <- c("Transition_Name","Dilution_Batch_Name")

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col = c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)

  # Create dilution statistical summary
  dilution_summary_grp <- dilution_data %>%
    summarise_dilution_table(grouping_variable = grouping_variable,
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(dilution_data,
                            dilution_summary_grp = dilution_summary_grp,
                            title = "Lipid_Point",
                            pal = pal,
                            dil_batch_var = "Dilution_Batch_Name",
                            conc_var = "Dilution_Percent",
                            conc_var_units = "%",
                            conc_var_interval = 50,
                            signal_var = "Area",
                            plot_first_half_lin_reg = FALSE)

  vdiffr::expect_doppelganger("A single point dilution plot", p)

})

test_that("Able to plot NA dilution data with ggplot correctly", {

  # Data Creation
  dilution_percent <- c(10, 20, 25, 40, 50, 60,
                        75, 80, 100, 125, 150)
  sample_name <- c("Sample_010a", "Sample_020a",
                   "Sample_025a", "Sample_040a", "Sample_050a",
                   "Sample_060a", "Sample_075a", "Sample_080a",
                   "Sample_100a", "Sample_125a", "Sample_150a")
  dilution_batch_name <- c("B1", "B1", "B1", "B1", "B1",
                           "B1", "B1", "B1", "B1", "B1", "B1")
  transition_name <- c("Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1", "Lipid1",
                       "Lipid1", "Lipid1", "Lipid1")
  na_area <- c(NA, NA, NA, NA, NA, NA,
               NA, NA, NA, NA, NA)


  # Handle the case of a plot that gives no points
  dilution_data <- tibble::tibble(Sample_Name = sample_name,
                                  Dilution_Batch_Name = dilution_batch_name,
                                  Dilution_Percent = dilution_percent,
                                  Transition_Name = transition_name,
                                  Area = na_area,
  )

  grouping_variable <- c("Transition_Name","Dilution_Batch_Name")

  # Get the dilution batch name from dilution_table
  dilution_batch_name <- dilution_batch_name %>%
    unique() %>%
    as.character()

  dil_batch_col = c("#377eb8")

  # Create palette for each dilution batch for plotting
  pal <- dil_batch_col %>%
    stats::setNames(dilution_batch_name)

  # Create dilution statistical summary
  dilution_summary_grp <- dilution_data %>%
    summarise_dilution_table(grouping_variable = grouping_variable,
                             conc_var = "Dilution_Percent",
                             signal_var = "Area") %>%
    evaluate_linearity(grouping_variable = grouping_variable) %>%
    dplyr::select(-c(dplyr::all_of(grouping_variable)))


  # Create the ggplot
  p <- plot_curve_ggplot(dilution_data,
                            dilution_summary_grp = dilution_summary_grp,
                            title = "Lipid_No_Point",
                            pal = pal,
                            dil_batch_var = "Dilution_Batch_Name",
                            conc_var = "Dilution_Percent",
                            conc_var_units = "%",
                            conc_var_interval = 50,
                            signal_var = "Area",
                            plot_first_half_lin_reg = FALSE)

  vdiffr::expect_doppelganger("A NA dilution plot", p)


})


test_that("Able to plot summary (numeric and characters) grid table for one dilution group", {

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
  table <- plot_summary_table(dilution_summary_grp)

  vdiffr::expect_doppelganger("complete summary table", grid::grid.draw(table))


})

test_that("Able to plot summary (only numeric) grid table for one dilution group", {

  r_corr <- c(0.951956)
  pra_linear <- c(65.78711)
  mandel_p_val <- c(2.899006e-07)
  concavity <- c(-4133.501328)



  dilution_summary_grp  <- data.frame(r_corr = r_corr,
                                      pra_linear = pra_linear,
                                      mandel_p_val = mandel_p_val,
                                      concavity = concavity)
  testthat::expect_null(plot_summary_table_char(dilution_summary_grp))

  # Test that it works even if there is no character or factor
  # or logical columns
  table <- plot_summary_table(dilution_summary_grp)

  vdiffr::expect_doppelganger("numeric summary table", grid::grid.draw(table))


})


test_that("Able to plot summary (only character) grid table for one dilution group", {

  wf1_group <- c("Poor Linearity")
  wf2_group <- c("Saturation")
  r_corr <- c(0.951956)
  pra_linear <- c(65.78711)
  mandel_p_val <- c(2.899006e-07)
  concavity <- c(-4133.501328)
  logical <- TRUE


  dilution_summary_grp  <- data.frame(wf1_group = wf1_group,
                                      wf2_group = wf2_group,
                                      logical = logical)

  testthat::expect_null(plot_summary_table_num(dilution_summary_grp))

  # Test that it works even if there is no numeric columns
  table <- plot_summary_table(dilution_summary_grp)

  vdiffr::expect_doppelganger("character summary table", grid::grid.draw(table))


})

test_that("Able to return NULL when there is no summary for one dilution group", {

  dilution_summary_grp <- data.frame()
  # Test that it gives NULL when both columns types are missing
  testthat::expect_null(plot_summary_table(dilution_summary_grp))
  testthat::expect_null(plot_summary_table(NA))
  testthat::expect_null(plot_summary_table(NULL))

})



