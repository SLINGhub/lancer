# dilution_summary argument is deprecated

    Code
      updated_summary <- update_cog_manual(curve_summary, cog_df, col_name_vec = "col_name_vec",
        desc_vec = "desc_vec", type_vec = "type_vec")

---

    Code
      updated_summary <- update_cog_auto(curve_summary)

---

    Code
      updated_summary <- convert_to_cog(curve_summary, grouping_variable = c(
        "Curve_Name", "Curve_Batch_Name"))

