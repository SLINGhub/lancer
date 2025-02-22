#' @title Validate Cognostics Data
#' @description Validate Cognostics Data
#' @param cog_df A data frame or tibble that contains cognostics information
#' @param needed_column A vector consisting of needed column names that
#' must be found in `cog_df`,
#' Default:
#' `c("col_name_vec", "desc_vec", "type_vec")`
#' @return An error if the things in `needed_column`
#' is not found in the Cognostics Data
#' @examples
#' # Create Cognostics Dataframe
#' col_name_vec <- c("Curve_Name", "Curve_Batch_Name")
#'
#' desc_vec <- c("Curve_Name", "Curve_Batch_Name")
#'
#' type_vec <- c("factor", "factor")
#'
#' cog_df <- data.frame(
#'   col_name_vec = col_name_vec,
#'   desc_vec = desc_vec,
#'   type_vec = type_vec
#' )
#'
#' validate_cog_df(cog_df)
#' @rdname validate_cog_df
#' @export
validate_cog_df <- function(cog_df,
                            needed_column = c(
                              "col_name_vec",
                              "desc_vec",
                              "type_vec"
                            )) {


  # Check if things in needed_column are in cog_df
  assertable::assert_colnames(cog_df, needed_column,
    only_colnames = FALSE, quiet = TRUE
  )
}


#' @title Create Default Cognostics Data Frame
#' @description Create default cognostics data frame to be used to
#' convert columns in `curve_summary` to class cognostics
#' @return A dataframe of default cognostics information
#' @details Internal function to create a dataframe of default cognostics
#' information to be used by the `trelliscopejs::cog`.
#' @examples
#' cog_df <- create_default_cog_df()
#' cog_df
#' @rdname create_default_cog_df
#' @export
create_default_cog_df <- function() {

  # More details in `trelliscopejs::cog` can be found in
  # <https://rdrr.io/cran/trelliscopejs/man/cog.html>

  col_name_vec <- c(
    "Curve_Name", "Curve_Batch_Name",
    "Curve_Class",
    "wf1_group", "wf2_group",
    "r_corr", "pra_linear", "mandel_p_val",
    "r2_linear", "r2_adj_linear", "mandel_stats",
    "adl_value"
  )

  desc_vec <- c(
    "Curve_Name", "Curve_Batch_Name",
    "Classes of Curves",
    "Group from workflow 1", "Group from workflow 2",
    "Pearson Correlation R values",
    "Linear Regression Percent Residual Accuracy",
    "P values for Mandel test",
    "Linear Regression R^2 Value",
    "Linear Regression Adjusted R^2 Value",
    "Test statistics for Mandel Test",
    "Average Deviation from Linearity"
  )

  type_vec <- c(
    "factor", "factor",
    "factor", "factor", "factor",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric"
  )

  cog_df <- data.frame(
    col_name_vec = col_name_vec,
    desc_vec = desc_vec,
    type_vec = type_vec
  )

  return(cog_df)
}

#' @title Update Cognostics Manually
#' @description Update cognostics on `curve_summary` based on
#' the cognostics parameters given by `cog_df`. We assume
#' `cog_df` is created manually
#' @param curve_summary The summary data frame or tibble generated
#' by function [summarise_curve_table()] and/or
#' [evaluate_linearity()].
#' @param dilution_summary `r lifecycle::badge("deprecated")`
#' `dilution_summary` was renamed to
#' `curve_summary`.
#' @param cog_df A data frame or tibble that contains cognostics information
#' If no input is given the cognostics information generated by function
#' [create_default_cog_df()] will be used.
#' Default: NULL
#' @param col_name_vec Column name in `cog_df` to indicate the columns
#' in `curve_summary` that needs to be converted to a cognostics.
#' Default: 'col_name_vec'
#' @param desc_vec Column name in `cog_df` to indicate the description
#' for each cognostics as define in `trelliscopejs::cog`.
#' Default: 'desc_vec'
#' @param type_vec Column name in `cog_df` to indicate the type
#' of each cognostics as define in `trelliscopejs::cog`.
#' Default: 'type_vec'
#' @return `curve_summary` with some columns converted
#' to type cog as defined in `trelliscopejs::cog`.
#' @details
#' More details in `trelliscopejs::cog` can be found in
#' <https://rdrr.io/cran/trelliscopejs/man/cog.html>.
#' @examples
#'
#' # Data Creation
#' concentration <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150,
#'   10, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#'
#' curve_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2",
#'   "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a",
#'   "Sample_010b", "Sample_025b",
#'   "Sample_040b", "Sample_050b", "Sample_060b",
#'   "Sample_075b", "Sample_080b", "Sample_100b",
#'   "Sample_125b", "Sample_150b"
#' )
#'
#' curve_1_saturation_regime <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060,
#'   5192648, 16594991, 32507833, 46499896,
#'   55388856, 62505210, 62778078, 72158161,
#'   78044338, 86158414
#' )
#'
#' curve_2_good_linearity <- c(
#'   31538, 53709, 69990, 101977, 146436, 180960,
#'   232881, 283780, 298289, 344519, 430432,
#'   25463, 63387, 90624, 131274, 138069,
#'   205353, 202407, 260205, 292257, 367924
#' )
#'
#' curve_3_noise_regime <- c(
#'   544, 397, 829, 1437, 1808, 2231,
#'   3343, 2915, 5268, 8031, 11045,
#'   500, 903, 1267, 2031, 2100,
#'   3563, 4500, 5300, 8500, 10430
#' )
#'
#' curve_4_poor_linearity <- c(
#'   380519, 485372, 478770, 474467, 531640, 576301,
#'   501068, 550201, 515110, 499543, 474745,
#'   197417, 322846, 478398, 423174, 418577,
#'   426089, 413292, 450190, 415309, 457618
#' )
#'
#' curve_batch_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Curve_Batch_Name = curve_batch_name,
#'   Concentration = concentration
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   `Curve_1` = curve_1_saturation_regime,
#'   `Curve_2` = curve_2_good_linearity,
#'   `Curve_3` = curve_3_noise_regime,
#'   `Curve_4` = curve_4_poor_linearity
#' )
#'
#' # Create curve table
#' curve_table <- create_curve_table(
#'   curve_batch_annot = curve_batch_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Signal",
#'   column_group = "Curve_Name"
#' )
#'
#' # Create curve statistical summary
#' curve_summary <- curve_table |>
#'   summarise_curve_table(
#'     grouping_variable = c(
#'       "Curve_Name",
#'       "Curve_Batch_Name"
#'     ),
#'     conc_var = "Concentration",
#'     signal_var = "Signal"
#'   ) |>
#'   dplyr::arrange(.data[["Curve_Name"]]) |>
#'   evaluate_linearity(grouping_variable = c(
#'     "Curve_Name",
#'     "Curve_Batch_Name"
#'   ))
#'
#' # Create our own cog_df
#' col_name_vec <- c(
#'   "Curve_Name", "Curve_Batch_Name",
#'   "Curve_Class"
#' )
#'
#' desc_vec <- c(
#'   "Curve_Name", "Curve_Batch_Name",
#'   "Curve_Class"
#' )
#'
#' type_vec <- c("factor", "factor", "factor")
#'
#' cog_df <- data.frame(
#'   col_name_vec = col_name_vec,
#'   desc_vec = desc_vec,
#'   type_vec = type_vec
#' )
#'
#' updated_summary <- update_cog_manual(
#'   curve_summary,
#'   cog_df,
#'   col_name_vec = "col_name_vec",
#'   desc_vec = "desc_vec",
#'   type_vec = "type_vec"
#' )
#'
#' # Observe that the first two columns has been converted
#' # to class cognostics
#' curve_summary
#'
#' updated_summary
#'
#' @rdname update_cog_manual
#' @export
update_cog_manual <- function(curve_summary,
                              dilution_summary = lifecycle::deprecated(),
                              cog_df = NULL,
                              col_name_vec = "col_name_vec",
                              desc_vec = "desc_vec",
                              type_vec = "type_vec") {

  if (lifecycle::is_present(dilution_summary)) {
    lifecycle::deprecate_warn(when = "0.0.6.9000",
                              what = "update_cog_manual(dilution_summary)",
                              with = "update_cog_manual(curve_summary)")
    curve_summary <- dilution_summary
  }

  if (is.null(cog_df)) {
    return(curve_summary)
  }

  # Check if cog_df is valid with the relevant columns
  validate_cog_df(cog_df,
    needed_column = c(
      col_name_vec,
      desc_vec,
      type_vec
    )
  )

  # See this webpage to learn how to mutate specific columns.
  # https://stackoverflow.com/questions/52998471/
  # dynamically-determine-if-a-dataframe-column-exists-
  # and-mutate-if-it-does

  for (colname in colnames(curve_summary)) {
    row_index <- which(cog_df$col_name_vec == colname)

    if (length(row_index) == 1) {
      curve_summary <- curve_summary |>
        dplyr::mutate(
          dplyr::across(
            dplyr::one_of(cog_df[[col_name_vec]][row_index]),
            trelliscopejs::cog,
            desc = cog_df[[desc_vec]][row_index],
            type = cog_df[[type_vec]][row_index]
          )
        )
    }
  }
  return(curve_summary)
}

#' Update Cognostics Automatically
#' @description Update cognostics on the `curve_summary` based on
#' the cognostics data frame given by the function `create_default_cog_df`.
#' @param curve_summary The summary table generated
#' by function [summarise_curve_table()] and/or
#' [evaluate_linearity()].
#' @param dilution_summary `r lifecycle::badge("deprecated")`
#' `dilution_summary` was renamed to
#' `curve_summary`.
#' @return `curve_summary` with some columns converted
#' to type cog as defined in `trelliscopejs::cog`.
#' @details
#' More details in `trelliscopejs::cog` can be found in
#' <https://rdrr.io/cran/trelliscopejs/man/cog.html>.
#' @examples
#'
#' # Data Creation
#' concentration <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150,
#'   10, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#'
#' curve_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2",
#'   "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a",
#'   "Sample_010b", "Sample_025b",
#'   "Sample_040b", "Sample_050b", "Sample_060b",
#'   "Sample_075b", "Sample_080b", "Sample_100b",
#'   "Sample_125b", "Sample_150b"
#' )
#'
#' curve_1_saturation_regime <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060,
#'   5192648, 16594991, 32507833, 46499896,
#'   55388856, 62505210, 62778078, 72158161,
#'   78044338, 86158414
#' )
#'
#' curve_2_good_linearity <- c(
#'   31538, 53709, 69990, 101977, 146436, 180960,
#'   232881, 283780, 298289, 344519, 430432,
#'   25463, 63387, 90624, 131274, 138069,
#'   205353, 202407, 260205, 292257, 367924
#' )
#'
#' curve_3_noise_regime <- c(
#'   544, 397, 829, 1437, 1808, 2231,
#'   3343, 2915, 5268, 8031, 11045,
#'   500, 903, 1267, 2031, 2100,
#'   3563, 4500, 5300, 8500, 10430
#' )
#'
#' curve_4_poor_linearity <- c(
#'   380519, 485372, 478770, 474467, 531640, 576301,
#'   501068, 550201, 515110, 499543, 474745,
#'   197417, 322846, 478398, 423174, 418577,
#'   426089, 413292, 450190, 415309, 457618
#' )
#'
#' curve_batch_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Curve_Batch_Name = curve_batch_name,
#'   Concentration = concentration
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   `Curve_1` = curve_1_saturation_regime,
#'   `Curve_2` = curve_2_good_linearity,
#'   `Curve_3` = curve_3_noise_regime,
#'   `Curve_4` = curve_4_poor_linearity
#' )
#'
#' # Create curve table
#' curve_table <- create_curve_table(
#'   curve_batch_annot = curve_batch_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Signal",
#'   column_group = "Curve_Name"
#' )
#'
#' # Create curve statistical summary
#' curve_summary <- curve_table |>
#'   summarise_curve_table(
#'     grouping_variable = c(
#'       "Curve_Name",
#'       "Curve_Batch_Name"
#'     ),
#'     conc_var = "Concentration",
#'     signal_var = "Signal"
#'   ) |>
#'   dplyr::arrange(.data[["Curve_Name"]]) |>
#'   evaluate_linearity(grouping_variable = c(
#'     "Curve_Name",
#'     "Curve_Batch_Name"
#'   ))
#'
#' updated_summary <- update_cog_auto(curve_summary)
#'
#' # Observe that the columns has been converted
#' # to class cognostics
#' curve_summary
#'
#' updated_summary
#'
#' @rdname update_cog_auto
#' @export
update_cog_auto <- function(
    curve_summary,
    dilution_summary = lifecycle::deprecated()) {

  if (lifecycle::is_present(dilution_summary)) {
    lifecycle::deprecate_warn(when = "0.0.6.9000",
                              what = "update_cog_auto(dilution_summary)",
                              with = "update_cog_auto(curve_summary)")
    curve_summary <- dilution_summary
  }

  # Create the default cognostics table
  cog_df <- create_default_cog_df()

  # Update the curve_summary columns and
  # convert the relevant to cognostics
  curve_summary <- curve_summary |>
    update_cog_manual(cog_df = cog_df)

  return(curve_summary)
}

#' @title Convert To Cognostics
#' @description Convert columns in `curve_summary` to
#' `trelliscopejs` cognostics for the `Trelliscope` display
#' @param curve_summary The summary data frame or tibble generated
#' by function [summarise_curve_table()] and/or
#' [evaluate_linearity()].
#' @param dilution_summary `r lifecycle::badge("deprecated")`
#' `dilution_summary` was renamed to
#' `curve_summary`.
#' @param cog_df A data frame or tibble that contains cognostics information
#' If no input is given the cognostics information generated by function
#' [create_default_cog_df()] will be used.
#' Default: NULL
#' @param grouping_variable A character vector of
#' column names in `curve_summary`to indicate how each curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report.
#' Default: c("Curve_Name", "Curve_Batch_Name")
#' @param panel_variable A column name in `curve_summary` to be converted
#' into a panel for the `Trelliscope` display.
#' Default: NULL
#' @param col_name_vec Column name in `cog_df` to indicate the columns
#' in `curve_summary` that needs to be converted to a cognostics.
#' Default: 'col_name_vec'
#' @param desc_vec Column name in `cog_df` to indicate the description
#' for each cognostics as define in `trelliscopejs::cog`.
#' Default: 'desc_vec'
#' @param type_vec Column name in `cog_df` to indicate the type
#' of each cognostics as define in `trelliscopejs::cog`.
#' Default: 'type_vec'
#' @return The curve summary table with `grouping variable` columns
#' converted to conditional cognostics.
#' other columns in `curve_summary` converted to cognostics
#' to be used in the in the `trelliscopejs` report.
#' @examples
#'
#' # Data Creation
#' concentration <- c(
#'   10, 20, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150,
#'   10, 25, 40, 50, 60,
#'   75, 80, 100, 125, 150
#' )
#'
#' curve_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1",
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2",
#'   "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a",
#'   "Sample_025a", "Sample_040a", "Sample_050a",
#'   "Sample_060a", "Sample_075a", "Sample_080a",
#'   "Sample_100a", "Sample_125a", "Sample_150a",
#'   "Sample_010b", "Sample_025b",
#'   "Sample_040b", "Sample_050b", "Sample_060b",
#'   "Sample_075b", "Sample_080b", "Sample_100b",
#'   "Sample_125b", "Sample_150b"
#' )
#'
#' curve_1_saturation_regime <- c(
#'   5748124, 16616414, 21702718, 36191617,
#'   49324541, 55618266, 66947588, 74964771,
#'   75438063, 91770737, 94692060,
#'   5192648, 16594991, 32507833, 46499896,
#'   55388856, 62505210, 62778078, 72158161,
#'   78044338, 86158414
#' )
#'
#' curve_2_good_linearity <- c(
#'   31538, 53709, 69990, 101977, 146436, 180960,
#'   232881, 283780, 298289, 344519, 430432,
#'   25463, 63387, 90624, 131274, 138069,
#'   205353, 202407, 260205, 292257, 367924
#' )
#'
#' curve_3_noise_regime <- c(
#'   544, 397, 829, 1437, 1808, 2231,
#'   3343, 2915, 5268, 8031, 11045,
#'   500, 903, 1267, 2031, 2100,
#'   3563, 4500, 5300, 8500, 10430
#' )
#'
#' curve_4_poor_linearity <- c(
#'   380519, 485372, 478770, 474467, 531640, 576301,
#'   501068, 550201, 515110, 499543, 474745,
#'   197417, 322846, 478398, 423174, 418577,
#'   426089, 413292, 450190, 415309, 457618
#' )
#'
#' curve_batch_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Curve_Batch_Name = curve_batch_name,
#'   Concentration = concentration
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   `Curve_1` = curve_1_saturation_regime,
#'   `Curve_2` = curve_2_good_linearity,
#'   `Curve_3` = curve_3_noise_regime,
#'   `Curve_4` = curve_4_poor_linearity
#' )
#'
#' # Create curve table
#' curve_table <- create_curve_table(
#'   curve_batch_annot = curve_batch_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Signal",
#'   column_group = "Curve_Name"
#' )
#'
#' # Create curve statistical summary
#' curve_summary <- curve_table |>
#'   summarise_curve_table(
#'     grouping_variable = c(
#'       "Curve_Name",
#'       "Curve_Batch_Name"
#'     ),
#'     conc_var = "Concentration",
#'     signal_var = "Signal"
#'   ) |>
#'   dplyr::arrange(.data[["Curve_Name"]]) |>
#'   evaluate_linearity(grouping_variable = c(
#'     "Curve_Name",
#'     "Curve_Batch_Name"
#'   ))
#'
#' updated_summary <- convert_to_cog(
#'   curve_summary,
#'   grouping_variable = c("Curve_Name",
#'                         "Curve_Batch_Name")
#'   )
#'
#' # Observe that the columns has been converted
#' # to class cognostics
#' curve_summary
#'
#' updated_summary
#'
#' @rdname convert_to_cog
#' @export
convert_to_cog <- function(curve_summary,
                           dilution_summary = lifecycle::deprecated(),
                           cog_df = NULL,
                           grouping_variable = c(
                             "Curve_Name",
                             "Curve_Batch_Name"
                           ),
                           panel_variable = NULL,
                           col_name_vec = "col_name_vec",
                           desc_vec = "desc_vec",
                           type_vec = "type_vec") {

  if (lifecycle::is_present(dilution_summary)) {
    lifecycle::deprecate_warn(when = "0.0.6.9000",
                              what = "convert_to_cog(dilution_summary)",
                              with = "convert_to_cog(curve_summary)")
    curve_summary <- dilution_summary
  }

  # Check if things in needed_column are in curve_summary
  assertable::assert_colnames(curve_summary, grouping_variable,
    only_colnames = FALSE, quiet = TRUE
  )

  # Check if panel_variable is also a grouping variable
  if (isTRUE(panel_variable %in% grouping_variable)) {
    stop(paste(
      "panel_variable", panel_variable,
      "cannot be a grouping_variable"
    ))
  }

  # Convert logical columns to characters
  curve_summary <- curve_summary |>
    dplyr::mutate_if(
      is.logical,
      ~ as.character(.x)
    )

  # Separate the panel variables if it is in curve_summary
  panel_df <- NULL
  if (!is.null(panel_variable)) {
    panel_df <- curve_summary |>
      dplyr::select(dplyr::any_of(c(
        grouping_variable,
        panel_variable
      )))

    curve_summary <- curve_summary |>
      dplyr::select(-dplyr::any_of(c(panel_variable)))
  }

  # Get cognostics for trellis report
  # First convert the columns based on default cognostics
  # from create_default_cog_df()
  # Next convert the columns based on user's input cognostics
  # Lastly, convert the rest of the columns based on class
  # Grouping variables must be the conditional columns
  curve_summary <- curve_summary |>
    update_cog_auto() |>
    update_cog_manual(
      cog_df = cog_df,
      col_name_vec = col_name_vec,
      desc_vec = desc_vec,
      type_vec = type_vec
    ) |>
    trelliscopejs::as_cognostics(
      cond_cols = grouping_variable,
      needs_cond = TRUE, needs_key = FALSE
    )


  # Convert panel variables if any to trelliscope_panels
  if (!is.null(panel_df) &&
    ncol(panel_df) != length(grouping_variable)) {

    # Ensure that the grouping variable is converted to
    # conditional columns
    # Ensure that the panel variable is converted to
    # trelliscope_panel
    panel_df <- panel_df |>
      dplyr::select(dplyr::all_of(c(grouping_variable))) |>
      trelliscopejs::as_cognostics(
        cond_cols = grouping_variable,
        needs_cond = TRUE,
        needs_key = FALSE
      ) |>
      dplyr::bind_cols(panel_df |>
        dplyr::select(dplyr::any_of(c(panel_variable)))) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(panel_variable),
        ~ structure(.x, class = c("trelliscope_panels", "list"))
      ))

    # Panel_df to do a left join with curve_summary
    # Move panel_variable to the end
    curve_summary <- panel_df |>
      dplyr::left_join(curve_summary, by = grouping_variable) |>
      dplyr::relocate(dplyr::any_of(c(panel_variable)),
        .after = dplyr::last_col()
      )
  }

  return(curve_summary)
}
