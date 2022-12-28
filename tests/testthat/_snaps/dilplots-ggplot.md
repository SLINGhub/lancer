# dilution_summary, dilution_summary_grp argument in plot_summary_table related functions are deprecated

    Code
      table <- plot_summary_table_char(dilution_summary_grp = curve_summary_grp)
    Condition
      Warning:
      The `dilution_summary_grp` argument of `plot_summary_table_char()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary_grp` argument instead.

---

    Code
      table <- plot_summary_table_num(dilution_summary_grp = curve_summary_grp)
    Condition
      Warning:
      The `dilution_summary_grp` argument of `plot_summary_table_num()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary_grp` argument instead.

---

    Code
      table <- plot_summary_table(dilution_summary_grp = curve_summary_grp)
    Condition
      Warning:
      The `dilution_summary_grp` argument of `plot_summary_table()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary_grp` argument instead.

# dilution_data, dilution_summary_grp and dil_batch_var argument in plot_curve_ggplot are deprecated

    Code
      p <- plot_curve_ggplot(dilution_data = curve_data, dilution_summary_grp = curve_summary_grp,
        pal = pal, title = "Lipid_Saturated", dil_batch_var = "Curve_Batch_Name",
        conc_var = "Concentration", conc_var_units = "%", conc_var_interval = 50,
        signal_var = "Signal")
    Condition
      Warning:
      The `dilution_data` argument of `plot_curve_ggplot()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_data` argument instead.
      Warning:
      The `dilution_summary_grp` argument of `plot_curve_ggplot()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary_grp` argument instead.
      Warning:
      The `dil_batch_var` argument of `plot_curve_ggplot()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_var` argument instead.

# dilution_data, dilution_summary, dil_batch_var, dil_batch_col argument in add_ggplot_panel are deprecated

    Code
      ggplot_table <- add_ggplot_panel(dilution_table = curve_table,
        dilution_summary = curve_summary, grouping_variable = c("Curve_Name",
          "Curve_Batch_Name"), dil_batch_var = "Curve_Batch_Name", dil_batch_col = c(
          "#377eb8"), conc_var = "Concentration", conc_var_units = "%",
        conc_var_interval = 50, signal_var = "Signal", have_plot_title = TRUE,
        plot_summary_table = TRUE, plot_first_half_lin_reg = FALSE,
        plot_last_half_lin_reg = FALSE)
    Condition
      Warning:
      The `dilution_table` argument of `add_ggplot_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_table` argument instead.
      Warning:
      The `dilution_summary` argument of `add_ggplot_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.
      Warning:
      The `dil_batch_var` argument of `add_ggplot_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_var` argument instead.
      Warning:
      The `dil_batch_col` argument of `add_ggplot_panel()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_batch_col` argument instead.

