test_that("calculate pra of linear model", {
  # Zero point will not be used to calculate the pra_value
  dilution_percent <- c(0, 10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  area <- c(0, 5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(94.80, unname(pra_value), tolerance = 0.01)

  dilution_percent <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  area <- c(5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(94.80, unname(pra_value), tolerance = 0.01)

  # Handle the case of a straight horizontal line input. Give NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(2, 2, 2, 2)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  # Handle the case of a straight vertical line input. Give NA
  dilution_percent <- c(10, 10, 10, 10)
  area <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  # Too little valid point (<= 3) will give an NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- data.frame(Dilution_Percent = c(NA, NA), Area = c(NA, NA))
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- data.frame(Dilution_Percent = NA, Area = NA)
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- NA
  pra_value <- calculate_pra_linear(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(pra_value))
})

test_that("calculate concavity", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(-4.089, unname(concavity_value), tolerance = 0.01)

  # Handle the case of a straight horizontal line input. Give NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(2, 2, 2, 2)
  curve_data <- data.frame(
    Dilution_Percent = dilution_percent,
    Area = area
  )
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent",
    "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  # Handle the case of a straight vertical line input. Give NA
  dilution_percent <- c(10, 10, 10, 10)
  area <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(
    Dilution_Percent = dilution_percent,
    Area = area
  )
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent",
    "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  # Too little valid point (<= 3) will give an NA
  dilution_percent <- c(0, 10, 8)
  area <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))


  curve_data <- data.frame(Dilution_Percent = c(NA, NA), Area = c(NA, NA))
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  curve_data <- data.frame(Dilution_Percent = NA, Area = NA)
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  curve_data <- NA
  concavity_value <- calculate_concavity(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(concavity_value))
})

test_that("calculate Mandel", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(70.1, unname(mandel_result$mandel_stats),
    tolerance = 0.01
  )
  testthat::expect_equal(0.00357, unname(mandel_result$mandel_p_val),
    tolerance = 0.001
  )

  # Handle the case of a straight horizontal line input. Give NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(2, 2, 2, 2)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  # Handle the case of a straight vertical line input. Give NA
  dilution_percent <- c(10, 10, 10, 10)
  area <- c(0, 5.23, 4.23, 6.35)
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  # Too little valid point (<= 3) will give an NA
  dilution_percent <- c(0, 10, 8)
  area <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- data.frame(Dilution_Percent = c(NA, NA), Area = c(NA, NA))
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- data.frame(Dilution_Percent = NA, Area = NA)
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- NA
  mandel_result <- calculate_mandel(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))
})

test_that("calculate dil linear GOF", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(0.95575, unname(dil_linear_gof$r_corr),
    tolerance = 0.001
  )
  testthat::expect_equal(0.91346, unname(dil_linear_gof$r2_linear),
    tolerance = 0.001
  )
  testthat::expect_equal(0.89182, unname(dil_linear_gof$r2_adj_linear),
    tolerance = 0.001
  )

  # Handle the case of a straight horizontal line input. Give NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(2, 2, 2, 2)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))

  # Handle the case of a straight vertical line input. Give NA
  dilution_percent <- c(10, 10, 10, 10)
  area <- c(0, 5.23, 4.23, 6.35)
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))

  # Too little valid point (<= 3) will give an NA
  dilution_percent <- c(0, 10, 8)
  area <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))

  curve_data <- data.frame(Dilution_Percent = c(NA, NA), Area = c(NA, NA))
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))

  curve_data <- data.frame(Dilution_Percent = NA, Area = NA)
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))

  curve_data <- NA
  dil_linear_gof <- calculate_gof_linear(
    curve_data,
    "Dilution_Percent", "Area"
  )
  testthat::expect_equal(NA, unname(dil_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(dil_linear_gof$r2_adj_linear))
})

test_that("calculate average deviation from linearity", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)

  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")

  # Handle the case of a straight horizontal line input. Give NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(2, 2, 2, 2)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))

  # Handle the case of a straight vertical line input. Give NA
  dilution_percent <- c(10, 10, 10, 10)
  area <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))

  # Too little valid point (<= 3) will give an NA
  dilution_percent <- c(0, 10, 8, 13)
  area <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Dilution_Percent = dilution_percent, Area = area)
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- data.frame(Dilution_Percent = c(NA, NA), Area = c(NA, NA))
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- data.frame(Dilution_Percent = NA, Area = NA)
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- NA
  adl_value <- calculate_adl(curve_data, "Dilution_Percent", "Area")
  testthat::expect_equal(NA, unname(adl_value))
})

test_that("calculate kroll linearity test using adl", {

  # Data from the paper
  solution_number <- c(
    1, 1, 2, 2, 3, 3, 4, 4,
    5, 5, 6, 6, 7, 7
  )
  result <- c(
    352, 348, 1009, 991, 1603, 1584, 3100, 3200,
    4482, 4390, 5101, 5046, 5669, 5516
  )

  curve_data <- data.frame(
    Solution_Number = solution_number,
    Result = result
  )

  adl_result <- calculate_adl_kroll_test(
    curve_data, "Solution_Number",
    "Result"
  )

  testthat::expect_equal(8.631137, unname(adl_result$adl_kroll),
    tolerance = 0.001
  )

  # Data in which best fit is cubic
  solution_number <- c(
    1, 1, 2, 2, 3, 3, 4, 4,
    5, 5
  )
  result <- c(
    1, 0.99, 1.6, 1.59, 2.5, 2.6, 4.36, 4.39,
    5.1, 5
  )

  # Data in which best fit is quadratic
  solution_number <- c(
    1, 1, 2, 2, 3, 3, 4, 4,
    5, 5
  )
  result <- c(
    4.7, 4.6, 7.8, 7.6, 10.4, 10.2, 13, 13.1,
    15.5, 15.3
  )
})


test_that("validate_curve_data", {
  dilution_percent <- c(10, 20, 40, 60, 80, 100)
  area <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Dilution_Percent = dilution_percent,
                              Area = area)
  testthat::expect_error(validate_curve_data(
    curve_data,
    "Dilution_Percen", "Area"
  ))
  testthat::expect_error(validate_curve_data(
    curve_data,
    "Dilution_Percent", "Are"
  ))
})
