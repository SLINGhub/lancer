#' @title Validate Cognostics Data
#' @description Validate Cognostics Data
#' @param cog_df A data frame or tibble that contains cognostics information
#' @param needed_column A vector consisting of needed column names that
#' must be found in `cog_df`,
#' Default: c("col_name_vec", "desc_vec",
#'            "type_vec", "default_label_vec")
#' @return An error if the things in `needed_column`
#' is not found in the Cognostics Data
#' @rdname validate_cog_df
#' @export
validate_cog_df <- function(cog_df,
                            needed_column = c("col_name_vec",
                                              "desc_vec",
                                              "type_vec",
                                              "default_label_vec")) {


  # Check if things in needed_column are in cog_df
  assertable::assert_colnames(cog_df, needed_column,
                              only_colnames = FALSE, quiet = TRUE)


}


#' @title Create default cognostics for dilution plot
#' @description Create default cognostics for dilution plot
#' @return A dataframe of default cognostics information
#' @details Internal function to create a dataframe of default cognostics
#' information to be used by the `trelliscopejs::cog`.
#' @rdname create_dil_default_cog_df
#' @export
create_dil_default_cog_df <- function() {

  # More details in `trelliscopejs::cog` can be found in
  # <https://rdrr.io/cran/trelliscopejs/man/cog.html>

  col_name_vec <- c("Transition_Name", "Dilution_Batch","Transition_Name_Class",
                    "wf1_group", "wf2_group",
                    "r_corr", "pra_linear", "mandel_p_val",
                    "r2_linear", "r2_adj_linear", "mandel_stats")

  desc_vec <- c("Transition_Name", "Dilution_Batch", "Classes of Transitions",
                "Group from workflow 1","Group from workflow 2",
                "Pearson Correlation R values",
                "Linear Regression Percent Residual Accuracy", "P values for Mandel test",
                "Linear Regression R^2 Value", "Linear Regression Adjusted R^2 Value",
                "Test statistics for Mandel Test")

  type_vec <- c("factor","factor",
                "factor","factor","factor",
                "numeric","numeric","numeric","numeric","numeric","numeric")

  default_label_vec <- c(TRUE, TRUE,
                         FALSE, FALSE, FALSE,
                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

  cog_df <- data.frame(col_name_vec = col_name_vec,
                       desc_vec = desc_vec,
                       type_vec = type_vec,
                       default_label_vec = default_label_vec)

  return(cog_df)
}

#' @title Update cognostics
#' @description Update cognostics
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' @param cog_df  A data frame or tibble output
#' from the function `create_dil_default_cog_df`
#' @param col_name_vec Column name in `cog_df` to indicate the columns
#' in `dilution_summary` that needs to be converted to a cognostics,
#' Default: 'col_name_vec'
#' @param desc_vec Column name in `cog_df` to indicate the description
#' for each cognostics as define in `trelliscopejs::cog`,
#' Default: 'desc_vec'
#' @param type_vec Column name in `cog_df` to indicate the type
#' of each cognostics as define in `trelliscopejs::cog`,
#' Default: 'type_vec'
#' @param default_label_vec Column name in `cog_df` to indicate the if
#' the given cognostics is a panel label as define in `trelliscopejs::cog`,
#' Default: 'default_label_vec'
#' @return A data frame or tibble output with some columns converted
#' to type cog as defined in `trelliscopejs::cog`
#' @details
#' More details in `trelliscopejs::cog` can be found in
#' <https://rdrr.io/cran/trelliscopejs/man/cog.html>
#' @rdname update_cogs
#' @export
update_cogs <- function(dilution_summary, cog_df,
                        col_name_vec = "col_name_vec",
                        desc_vec = "desc_vec",
                        type_vec = "type_vec",
                        default_label_vec = "default_label_vec") {

  if (is.null(cog_df)) {
    return(dilution_summary)
  }

  #Check if cog_df is valid with the relevant columns
  validate_cog_df(cog_df,
                  needed_column = c(col_name_vec,
                                    desc_vec,
                                    type_vec,
                                    default_label_vec)
  )

  # See this webpage to learn how to mutate specific columns.
  # https://stackoverflow.com/questions/52998471/dynamically-determine-if-a-dataframe-column-exists-and-mutate-if-it-does

  for (colname in colnames(dilution_summary)) {
    row_index <- which(cog_df$col_name_vec == colname)

    if (length(row_index) == 1) {
      dilution_summary <- dilution_summary %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::one_of(cog_df[[col_name_vec]][row_index]),
            trelliscopejs::cog,
            desc = cog_df[[desc_vec]][row_index],
            type = cog_df[[type_vec]][row_index],
            default_label = cog_df[[default_label_vec]][row_index]
          )
        )
    }
  }
  return(dilution_summary)
}


#' Get dilution plots default cognostics
#' @description Function used to add the default cognostics defined in
#' `create_dil_default_cog_df` relevant to the dilution plot
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' @return Output a data frame with added cognostics
#' relevant to the dilution plot
#' @rdname get_dil_default_cognostics
#' @export
get_dil_default_cognostics <- function(dilution_summary) {

  # Create the default cognostics table
  cog_df <- create_dil_default_cog_df()

  # Update the dilution summary columns and
  # convert the relevant to cognostics
  dilution_summary <- dilution_summary %>%
    update_cogs(cog_df = cog_df)

  return(dilution_summary)

}



#' @title Convert to cognostics
#' @description Convert columns in `dilution_summary` to `trelliscopejs` cognostics
#' @param dilution_summary The summary table generated
#' by function `summarise_dilution_table` and/or `evaluate_linearity`
#' but it can also be any generic data frame or tibble
#' @param cog_df A data frame or tibble that contains cognostics information
#' If no input is given the cognostics information generated by function
#' `get_dil_default_cognostics` will be used.
#' Default: NULL
#' @param grouping_variable A character vector of
#' column names in `dilution_summary`to indicate how each dilution curve
#' should be grouped by. It is also going to be used as a conditional
#' cognostics in the `trelliscopejs` report,
#' Default: c("Transition_Name", "Dilution_Batch")
#' @param col_name_vec Column name in `cog_df` to indicate the columns
#' in `dilution_summary` that needs to be converted to a cognostics,
#' Default: 'col_name_vec'
#' @param desc_vec Column name in `cog_df` to indicate the description
#' for each cognostics as define in `trelliscopejs::cog`,
#' Default: 'desc_vec'
#' @param type_vec Column name in `cog_df` to indicate the type
#' of each cognostics as define in `trelliscopejs::cog`,
#' Default: 'type_vec'
#' @param default_label_vec Column name in `cog_df` to indicate the if
#' the given cognostics is a panel label as define in `trelliscopejs::cog`,
#' Default: 'default_label_vec'
#' @return The dilution summary table with `groupung variable` columns
#' converted to conditional cognostics,
#' other columns in `dilution_summary` converted to cognostics
#' to be used in the in the `trelliscopejs` report.
#' @rdname convert_to_cognostics
#' @export
convert_to_cognostics <- function(dilution_summary, cog_df = NULL,
                                  grouping_variable = c("Transition_Name",
                                                        "Dilution_Batch"),
                                  col_name_vec = "col_name_vec",
                                  desc_vec = "desc_vec",
                                  type_vec = "type_vec",
                                  default_label_vec = "default_label_vec") {

  # Check if things in needed_column are in dilution_summary
  assertable::assert_colnames(dilution_summary, grouping_variable,
                              only_colnames = FALSE, quiet = TRUE)

  # Get cognostics for trellis report
  # Grouping variables must be the conditional columns
  dilution_summary <- dilution_summary %>%
    get_dil_default_cognostics() %>%
    update_cogs(cog_df = cog_df,
                col_name_vec = col_name_vec,
                desc_vec = desc_vec,
                type_vec = type_vec,
                default_label_vec = default_label_vec) %>%
    trelliscopejs::as_cognostics(cond_cols = grouping_variable,
                                 needs_cond = TRUE, needs_key = FALSE)

  return(dilution_summary)

}
