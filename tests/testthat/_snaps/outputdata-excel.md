# Argument dilution summary is deprecated

    Code
      column_max_char <- calculate_column_max_char(dilution_summary = curve_summary)
    Condition
      Warning:
      The `dilution_summary` argument of `calculate_column_max_char()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      curve_summary <- mark_near_zero_columns(dilution_summary = curve_summary)
    Condition
      Warning:
      The `dilution_summary` argument of `mark_near_zero_columns()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      column_max_char <- mark_near_zero_columns(dilution_summary = curve_summary)
    Condition
      Warning:
      The `dilution_summary` argument of `mark_near_zero_columns()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      format_num_cell_style(dilution_summary = curve_summary, workbook = my_workbook,
        sheet = "Curve Summary")
    Condition
      Warning:
      The `dilution_summary` argument of `format_num_cell_style()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      format_char_cell_colour(dilution_summary = curve_summary, workbook = my_workbook,
        sheet = "Curve Summary", conditional_column = "wf1_group",
        pass_criteria_words = c("Good Linearity"))
    Condition
      Warning:
      The `dilution_summary` argument of `format_char_cell_colour()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      format_num_cell_colour(dilution_summary = curve_summary, workbook = my_workbook,
        sheet = "Curve Summary", conditional_column = "r_corr", threshold_value = "0.8",
        pass_criteria = "above")
    Condition
      Warning:
      The `dilution_summary` argument of `format_num_cell_colour()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

