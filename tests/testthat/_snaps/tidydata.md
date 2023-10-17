# test deprecated functions

    Code
      validate_dilution_annot(dilution_annot = curve_batch_annot, needed_column = c(
        "Sample_Name"))
    Condition
      Warning:
      `validate_dilution_annot()` was deprecated in lancer 0.0.6.9000.
      i Please use `validate_curve_batch_annot()` instead.

---

    Code
      validate_lipid_data_wide(lipid_data_wide = curve_data_wide, needed_column = c(
        "Sample_Name"))
    Condition
      Warning:
      `validate_lipid_data_wide()` was deprecated in lancer 0.0.6.9000.
      i Please use `validate_curve_data_wide()` instead.

---

    Code
      create_dilution_table(dilution_annot = curve_batch_annot, lipid_data_wide = curve_data_wide,
        common_column = c("Sample_Name"), signal_var = "Signal", column_group = "Curve_Name")
    Condition
      Warning:
      `create_dilution_table()` was deprecated in lancer 0.0.6.9000.
      i Please use `create_curve_table()` instead.
    Output
      # A tibble: 24 x 5
         Sample_Name Curve_Batch_Name Concentration Curve_Name  Signal
         <chr>       <chr>                    <dbl> <chr>        <dbl>
       1 Sample_010a B1                          10 Curve_1      22561
       2 Sample_010a B1                          10 Curve_2    2299075
       3 Sample_020a B1                          20 Curve_1      31178
       4 Sample_020a B1                          20 Curve_2    4136350
       5 Sample_040a B1                          40 Curve_1      39981
       6 Sample_040a B1                          40 Curve_2    7020062
       7 Sample_060a B1                          60 Curve_1      48390
       8 Sample_060a B1                          60 Curve_2    8922063
       9 Sample_080a B1                          80 Curve_1      52171
      10 Sample_080a B1                          80 Curve_2    9288742
      # i 14 more rows

---

    Code
      validate_dilution_table(dilution_table = curve_table, needed_column = c(
        "Curve_Name", "Curve_Batch_Name", "Concentration", "Signal"))
    Condition
      Warning:
      `validate_dilution_table()` was deprecated in lancer 0.0.6.9000.
      i Please use `validate_curve_table()` instead.

---

    Code
      summarise_dilution_table(dilution_table = curve_table, grouping_variable = c(
        "Curve_Name", "Curve_Batch_Name"), conc_var = "Concentration", signal_var = "Signal")
    Condition
      Warning:
      `summarise_dilution_table()` was deprecated in lancer 0.0.6.9000.
      i Please use `summarise_curve_table()` instead.
    Output
      # A tibble: 4 x 9
        Curve_Name Curve_Batch_Name r_corr r2_linear r2_adj_linear mandel_stats
        <chr>      <chr>             <dbl>     <dbl>         <dbl>        <dbl>
      1 Curve_1    B1                0.956     0.913         0.892        70.1 
      2 Curve_2    B1                0.972     0.946         0.932         5.67
      3 Curve_1    B2                0.956     0.913         0.892        70.1 
      4 Curve_2    B2                0.972     0.946         0.932         5.67
      # i 3 more variables: mandel_p_val <dbl>, pra_linear <dbl>, concavity <dbl>

