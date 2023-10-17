# dilution_summary, dilution_summary_grp argument in plot_summary_table related functions are deprecated

    Code
      table <- plot_summary_table_char(curve_summary_grp)

---

    Code
      table <- plot_summary_table_num(curve_summary_grp)

---

    Code
      table <- plot_summary_table(curve_summary_grp)

# dilution_data, dilution_summary_grp and dil_batch_var argument in plot_curve_ggplot are deprecated

    Code
      p <- plot_curve_ggplot(curve_data, curve_summary_grp, pal = pal, title = "Lipid_Saturated",
        conc_var = "Concentration", conc_var_units = "%", conc_var_interval = 50,
        signal_var = "Signal")

# dilution_data, dilution_summary, dil_batch_var, dil_batch_col argument in add_ggplot_panel are deprecated

    Code
      ggplot_table <- add_ggplot_panel(curve_table, curve_summary, grouping_variable = c(
        "Curve_Name", "Curve_Batch_Name"), conc_var = "Concentration",
      conc_var_units = "%", conc_var_interval = 50, signal_var = "Signal",
      have_plot_title = TRUE, plot_summary_table = TRUE, plot_first_half_lin_reg = FALSE,
      plot_last_half_lin_reg = FALSE)

