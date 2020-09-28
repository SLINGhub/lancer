test_that("calculate pra test", {
  dilution_percent <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  area <- c(5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  #dilution_percent <- c(NA, NA)
  #area <- c(NA, NA)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(94.80, unname(pra_value), tolerance  = 0.01)
})

test_that("calculate Concavity", {
  dilution_percent <- c(10,20,40,60,80,100)
  area <- c(22561,31178,39981,48390,52171,53410)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  concavity_value <- calculate_concavity(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(-4.089, unname(concavity_value), tolerance  = 0.01)
})

test_that("calculate Mandel test", {
  dilution_percent <- c(10,20,40,60,80,100)
  area <- c(22561,31178,39981,48390,52171,53410)
  dilution_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  mandel_result <- calculate_mandel(dilution_data, "Dilution_Percent", "Area")
  testthat::expect_equal(70.1, unname(mandel_result$mandel_stats), tolerance  = 0.01)
  testthat::expect_equal(0.00357, unname(mandel_result$mandel_p_val), tolerance  = 0.001)

})
