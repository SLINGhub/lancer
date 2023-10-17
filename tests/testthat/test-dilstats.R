test_that("calculate pra of linear model", {
  # Zero point will not be used to calculate the pra_value
  concentration <- c(0, 10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  signal <- c(0, 5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(94.80, unname(pra_value), tolerance = 0.01)

  concentration <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
  signal <- c(5.23, 4.23, 6.35, 4.75, 5.65, 6.62, 3.03, 1.62, 6.03, 3.65, 2.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(94.80, unname(pra_value), tolerance = 0.01)

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(10, 10, 10, 10)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))

  curve_data <- NA
  pra_value <- calculate_pra_linear(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(pra_value))
})

test_that("calculate concavity", {
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(-4.089, unname(concavity_value), tolerance = 0.01)

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(
    Concentration = concentration,
    Signal = signal
  )
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration",
    "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(10, 10, 10, 10)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(
    Concentration = concentration,
    Signal = signal
  )
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration",
    "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(0, 10, 8)
  signal <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))


  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))

  curve_data <- NA
  concavity_value <- calculate_concavity(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(concavity_value))
})

test_that("calculate Mandel", {
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(70.1, unname(mandel_result$mandel_stats),
    tolerance = 0.01
  )
  testthat::expect_equal(0.00357, unname(mandel_result$mandel_p_val),
    tolerance = 0.001
  )

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(10, 10, 10, 10)
  signal <- c(0, 5.23, 4.23, 6.35)
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(0, 10, 8)
  signal <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))

  curve_data <- NA
  mandel_result <- calculate_mandel(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(mandel_result$mandel_stats))
  testthat::expect_equal(NA, unname(mandel_result$mandel_p_val))
})

test_that("calculate dil linear GOF", {
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(0.95575, unname(curve_linear_gof$r_corr),
    tolerance = 0.001
  )
  testthat::expect_equal(0.91346, unname(curve_linear_gof$r2_linear),
    tolerance = 0.001
  )
  testthat::expect_equal(0.89182, unname(curve_linear_gof$r2_adj_linear),
    tolerance = 0.001
  )

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(10, 10, 10, 10)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(0, 10, 8)
  signal <- c(0, 5.23, 4.23)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))

  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))

  curve_data <- NA
  curve_linear_gof <- calculate_gof_linear(
    curve_data,
    "Concentration", "Signal"
  )
  testthat::expect_equal(NA, unname(curve_linear_gof$r_corr))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_linear))
  testthat::expect_equal(NA, unname(curve_linear_gof$r2_adj_linear))
})

test_that("calculate average deviation from linearity", {

  # Linear curve example
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 44247, 85636, 137928, 170384, 224678)
  linear_curve_data <- data.frame(Concentration = concentration,
                                  Signal = signal)

  # Quadratic curve example
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)
  quad_curve_data <- data.frame(Concentration = concentration,
                                Signal = signal)

  # Sigmoid curve example
  concentration <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
  cubic_signal <- c(22561, 23561, 25981,
                    40390, 60384, 80294,
                    100171, 101410, 103893)

  cubic_curve_data <- data.frame(Concentration = concentration,
                                 Signal = cubic_signal)

  testthat::expect_silent(
    adl_value <- calculate_adl(
      linear_curve_data, "Concentration", "Signal")
  )

  testthat::expect_silent(
    adl_value <- calculate_adl(
      quad_curve_data, "Concentration", "Signal")
  )

  testthat::expect_silent(
    adl_value <- calculate_adl(
      cubic_curve_data, "Concentration", "Signal")
  )

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(10, 10, 10, 10)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(0, 5.23, 4.23, 6.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))

  curve_data <- NA
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  testthat::expect_equal(NA, unname(adl_value))
})

test_that("calculate kroll linearity test using adl", {

  # Linear curve example
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 44247, 85636, 137928, 170384, 224678)
  linear_curve_data <- data.frame(Concentration = concentration,
                                  Signal = signal)

  # Quadratic curve example
  concentration <- c(10, 20, 40, 60, 80, 100)
  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)
  quad_curve_data <- data.frame(Concentration = concentration,
                                Signal = signal)

  # Sigmoid curve example
  concentration <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
  cubic_signal <- c(22561, 23561, 25981,
                    40390, 60384, 80294,
                    100171, 101410, 103893)

  cubic_curve_data <- data.frame(Concentration = concentration,
                                 Signal = cubic_signal)

  testthat::expect_silent(
    adl_result <- calculate_adl_kroll_test(
      linear_curve_data,
      "Concentration",
      "Signal")
  )

  testthat::expect_silent(
    adl_result <- calculate_adl_kroll_test(
      quad_curve_data,
      "Concentration",
      "Signal")
  )

  testthat::expect_silent(
    adl_result <- calculate_adl_kroll_test(
      cubic_curve_data,
      "Concentration",
      "Signal")
  )

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
    curve_data,
    "Solution_Number",
    "Result"
  )

  testthat::expect_equal(8.631137, unname(adl_result$adl_kroll),
    tolerance = 0.001
  )

  # Handle the case of a straight horizontal line input. Give NA
  concentration <- c(0, 10, 8, 13)
  signal <- c(2, 2, 2, 2)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

  # Handle the case of a straight vertical line input. Give NA
  concentration <- c(8, 8, 8, 8)
  signal <- c(1, 2, 3, 4)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

  # Too little valid point (<= 3) will give an NA
  concentration <- c(10, 8, 13)
  signal <- c(5.23, 4.23, 6.35)
  curve_data <- data.frame(Concentration = concentration, Signal = signal)
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

  curve_data <- data.frame(Concentration = c(NA, NA), Signal = c(NA, NA))
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

  curve_data <- data.frame(Concentration = NA, Signal = NA)
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

  curve_data <- NA
  adl_value <- calculate_adl(curve_data, "Concentration", "Signal")
  adl_result <- calculate_adl_kroll_test(
    curve_data,
    "Concentration",
    "Signal")
  testthat::expect_equal(NA, unname(adl_result$adl_kroll))

})

test_that("validate_curve_data", {

  concentration <- c(10, 20, 40, 60, 80, 100)

  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)

  curve_data <- data.frame(Concentration = concentration,
                           Signal = signal)

  testthat::expect_error(validate_curve_data(
    curve_data,
    "Concentratio", "Signal"
  ))
  testthat::expect_error(validate_curve_data(
    curve_data,
    "Concentration", "Signa"
  ))
})

test_that("test summarise_curve_data when details = TRUE", {
  concentration <- c(10, 20, 40, 60, 80, 100)

  signal <- c(22561, 31178, 39981, 48390, 52171, 53410)

  curve_data <- data.frame(Concentration = concentration,
                           Signal = signal)

  testthat::expect_silent(
    curve_summary <- summarise_curve_data(
      curve_data,
      conc_var = "Concentration",
      signal_var = "Signal",
      details = TRUE
      )
  )

})

