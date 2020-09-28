test_that("calculate pra", {
  dilution_percent <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  area <- c(5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(94.80, unname(pra_value), tolerance  = 0.01)

  dilution_data <- data.frame(Dilution_Percent = c(NA, NA), Area =  c(NA, NA))
  pra_value <- calculate_pra(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  dilution_data <- data.frame(Dilution_Percent = NA, Area = NA)
  pra_value <- calculate_pra(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  dilution_data <- NA
  pra_value <- calculate_pra(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

})

test_that("calculate concavity", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  concavity_value <- calculate_concavity(dilution_data,
                                         "Dilution_Percent", "Area")
  testthat::expect_equal(-4.089, unname(concavity_value), tolerance  = 0.01)

  dilution_data <- data.frame(Dilution_Percent = c(NA, NA), Area =  c(NA, NA))
  concavity_value <- calculate_concavity(dilution_data,
                                         "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(concavity_value))

  dilution_data <- data.frame(Dilution_Percent = NA, Area = NA)
  concavity_value <- calculate_concavity(dilution_data,
                                         "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(concavity_value))

  dilution_data <- NA
  concavity_value <- calculate_concavity(dilution_data,
                                         "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(concavity_value))


})

test_that("calculate Mandel", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(70.1, unname(mandel_result$mandel_stats),
                         tolerance  = 0.01)
  testthat::expect_equal(0.00357, unname(mandel_result$mandel_p_val),
                         tolerance  = 0.001)

  dilution_data <- data.frame(Dilution_Percent = c(NA, NA), Area =  c(NA, NA))
  mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA,unname(mandel_result$mandel_p_val))

  dilution_data <- data.frame(Dilution_Percent = NA, Area = NA)
  mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA,unname(mandel_result$mandel_p_val))

  dilution_data <- NA
  mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA,unname(mandel_result$mandel_p_val))

})

test_that("calculate dil linear GOF", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
                                             "Dilution_Percent", "Area")
  testthat::expect_equal(0.95575, unname(dil_linear_gof$r_corr),
                         tolerance  = 0.001)
  testthat::expect_equal(0.91346, unname(dil_linear_gof$r2_linear),
                         tolerance  = 0.001)
  testthat::expect_equal(0.89182, unname(dil_linear_gof$r2_adj_linear),
                         tolerance  = 0.001)
  testthat::expect_equal(119.743, unname(dil_linear_gof$bic_linear),
                         tolerance  = 0.01)
  testthat::expect_equal(0.00289, unname(dil_linear_gof$corr_p_val),
                         tolerance  = 0.001)


  dilution_data <- data.frame(Dilution_Percent = c(NA, NA), Area =  c(NA, NA))
  dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
                                             "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$bic_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$corr_p_val))

  dilution_data <- data.frame(Dilution_Percent = NA, Area = NA)
  dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
                                             "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$bic_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$corr_p_val))

  dilution_data <- NA
  dil_linear_gof <- calculate_dil_linear_gof(dilution_data,
                                             "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$bic_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$corr_p_val))



})
