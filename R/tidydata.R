create_dil_table <- function(dilution_annot, lipid_data_wide,
                             common_column = c("Sample_Name"),
                             signal_var = "Area",
                             column_group = "Transition_Name") {

  dil_table <- dplyr::inner_join(dilution_annot,lipid_data_wide,
                                 by= common_column) %>%
    tidyr::pivot_longer(-tidyselect::any_of(colnames(dilution_annot)),
                        names_to = column_group, values_to = signal_var)

  return(dil_table)

}
