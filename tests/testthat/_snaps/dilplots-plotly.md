# dilution_data and dil_batch_var argument in plot_curve_plotly are deprecated

    Code
      p <- plot_curve_plotly(dilution_data = curve_data, title = "Curve_Saturated",
        pal = pal, sample_name_var = "Sample_Name", dil_batch_var = "Curve_Batch_Name",
        conc_var = "Concentration", conc_var_units = "%", conc_var_interval = 50,
        signal_var = "Signal", plot_first_half_lin_reg = TRUE,
        plot_last_half_lin_reg = TRUE)
    Condition
      Warning:
      The `dilution_data` argument of `plot_curve_plotly()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.
      Warning:
      The `dil_batch_var` argument of `plot_curve_plotly()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_var` argument instead.

# dilution_data, dilution_summary, dil_batch_var, dil_batch_col argument in add_plotly_panel are deprecated

    Code
      plotly_table <- add_plotly_panel(dilution_table = curve_table,
        dilution_summary = curve_summary, grouping_variable = c("Curve_Name",
          "Curve_Batch_Name"), dil_batch_var = "Curve_Batch_Name", dil_batch_col = c(
          "#377eb8"), conc_var = "Concentration", conc_var_units = "%",
        conc_var_interval = 50, signal_var = "Signal", have_plot_title = FALSE,
        plot_first_half_lin_reg = FALSE, plot_last_half_lin_reg = FALSE)
    Condition
      Warning:
      The `dilution_table` argument of `add_plotly_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_table` argument instead.
      Warning:
      The `dilution_summary` argument of `add_plotly_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.
      Warning:
      The `dil_batch_var` argument of `add_plotly_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_var` argument instead.
      Warning:
      The `dil_batch_col` argument of `add_plotly_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_col` argument instead.

