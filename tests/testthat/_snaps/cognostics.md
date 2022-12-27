# dilution_summary argument in update_cog_manual is deprecated

    Code
      updated_summary <- update_cog_manual(dilution_summary = curve_summary, cog_df,
        col_name_vec = "col_name_vec", desc_vec = "desc_vec", type_vec = "type_vec")
    Condition
      Warning:
      The `dilution_summary` argument of `update_cog_manual()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      updated_summary <- update_cog_auto(dilution_summary = curve_summary)
    Condition
      Warning:
      The `dilution_summary` argument of `update_cog_auto()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

---

    Code
      updated_summary <- convert_to_cog(dilution_summary = curve_summary,
        grouping_variable = c("Curve_Name", "Curve_Batch_Name"))
    Condition
      Warning:
      The `dilution_summary` argument of `convert_to_cog()` is deprecated as of lancer 0.0.6.9000.
      i Please use the `curve_summary` argument instead.

