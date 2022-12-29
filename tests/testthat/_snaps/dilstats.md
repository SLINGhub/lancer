# dilution_data argument is deprecated

    Code
      linear_model <- create_linear_model(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `evaluate_linearity()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      linear_model <- create_quad_model(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `create_quad_model()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      linear_model <- create_cubic_model(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `create_cubic_model()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      concavity_value <- calculate_concavity(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `calculate_concavity()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      curve_linear_gof <- calculate_gof_linear(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `calculate_gof_linear()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      mandel_result <- calculate_mandel(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `calculate_mandel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

---

    Code
      pra_value <- calculate_pra_linear(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `calculate_pra_linear()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.

# test deprecated functions

    Code
      validate_dilution_data(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      `validate_dilution_data()` was deprecated in lancer 0.0.6.9000.
      i Please use `validate_curve_data()` instead.

---

    Code
      curve_summary <- summarise_dilution_data(dilution_data = curve_data, conc_var = "Concentration",
        signal_var = "Signal")
    Condition
      Warning:
      `summarise_dilution_data()` was deprecated in lancer 0.0.6.9000.
      i Please use `summarise_curve_data()` instead.

