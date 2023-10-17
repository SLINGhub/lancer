test_that("Test Linear Evaluation", {

  # Create curve summary data
  r_corr <- c(0.951956, 0.948683, 0.978057, 0.976462, 0.970618, 0.969348)
  pra_linear <- c(65.78711, 64.58687, 90.21257, 89.95473, 72.91220, 72.36528)
  mandel_p_val <- c(
    2.899006e-07, 7.922290e-07, 2.903365e-01, 3.082930e-01,
    3.195779e-08, 6.366588e-08
  )
  concavity <- c(
    -4133.501328, -4146.745747, -3.350942, -3.393617,
    0.3942824, 0.4012963
  )

  curve_summary <- data.frame(
    r_corr = r_corr, pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity) |>
    evaluate_linearity()

  first_group_fail <- data.frame(
    r_cor = r_corr, pra_linear = pra_linear,
    mandel_p_val = mandel_p_val,
    concavity = concavity
  )
  testthat::expect_warning(evaluate_linearity(first_group_fail))

  second_group_fail <- data.frame(
    r_corr = r_corr, pra_linear = pra_linear,
    mandel_p_va = mandel_p_val,
    concavity = concavity
  )
  testthat::expect_warning(evaluate_linearity(second_group_fail))
})

