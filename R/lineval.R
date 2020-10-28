evaluate_linearity <- function(dilution_summary) {


  dilution_summary <- dilution_summary %>%
    dplyr::mutate(curve_group1 =
                    dplyr::case_when(
                      .data[["r_corr"]] < 0.8 ~ "Poor Linearity",
                      .data[["r_corr"]] >= 0.8 &
                        .data[["pra_linear"]] < 80 ~ "Poor Linearity",
                      .data[["r_corr"]] >= 0.8 &
                        .data[["pra_linear"]] >= 80 ~ "Good Linearity",
                    ),
                  curve_group2 =
                    dplyr::case_when(
                      .data[["curve_group1"]] == "Poor Linearity" &
                        .data[["r_corr"]] >= 0.8 &
                        .data[["mandel_p_val"]] < 0.05 &
                        .data[["concavity"]] >= 0 ~ "LOD",
                      .data[["curve_group1"]] == "Poor Linearity" &
                        .data[["r_corr"]] >= 0.8 &
                        .data[["mandel_p_val"]] < 0.05 &
                        .data[["concavity"]] < 0 ~ "Saturation",
                      TRUE ~ curve_group1

                  )
    )


  return(dilution_summary)

}
