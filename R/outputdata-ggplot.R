#' @title Create the page layout for the pdf document
#' @description Get the page layout for the pdf document based
#' on the number of plots to display and the number of
#' columns and rows.
#' @param number_of_plots Number of plots to be output into the
#' pdf document.
#' @param ncol Number of columns of plots allowed in one
#' page of the pdf document. Default: NULL
#' @param nrow Number of rows of plots allowed in one
#' page of the pdf document. Default: NULL
#' @return A list containing the number of rows and cols required
#' for one pdf page.
#' @details
#'  * If both `ncol` and `nrow` are NULL or only `ncol` is NULL,
#' the layout will have one column and the total number of rows
#' will be the number of plots it needs to produce.
#'  * If only `nrow` is NULL, the layout will have one row and
#' the total number of columns will be the number of plots
#' it needs to produce.
#' @examples
#' create_page_layout(
#'   number_of_plots = 4,
#'   ncol = 2,
#'   nrow = 2
#' )
#'
#' @rdname create_page_layout
#' @export
create_page_layout <- function(number_of_plots,
                               ncol = NULL, nrow = NULL) {
  if (number_of_plots < 1) {
    stop("Number of plots in the input list must be greater than 1")
  }
  if (!is.null(ncol) && !is.null(nrow)) {
  } else if (!is.null(ncol)) {
    if (ncol == 1) nrow <- number_of_plots
  } else if (!is.null(nrow)) {
    if (nrow == 1) ncol <- number_of_plots
  }
  list(ncol = ncol, nrow = nrow)
}

#' @title View `ggplot` Curve Plots As Pdf
#' @description View `ggplot` curve plots in a pdf file.
#' @param ggplot_list list of plots to be arranged into a grid
#' for each pdf page. The list of plot should can be created
#' via the function [add_ggplot_panel()]. The function will create
#' a data frame or tibble and the column name "panel" should contain
#' the list of plots.
#' @param filename File name of the pdf document.
#' Default: 'dilution_plot.pdf'
#' @param ncol_per_page Number of columns of plots allowed in one
#' page of the pdf document.
#' Default: 2
#' @param nrow_per_page Number of rows of plots allowed in one
#' page of the pdf document.
#' Default: 2
#' @param width The width of one pdf page. Default: 15
#' @param height The height of one pdf page. Default: 8
#' @param testing To indicate if we are running a test,
#' if so, no pdf report is given out.
#' Default: FALSE
#' @examples
#' sample_name <- c(
#'   "Sample_010a", "Sample_020a", "Sample_040a",
#'   "Sample_060a", "Sample_080a", "Sample_100a",
#'   "Sample_010b", "Sample_020b", "Sample_040b",
#'   "Sample_060b", "Sample_080b", "Sample_100b"
#' )
#'
#' lipid1_area <- c(
#'   22561, 31178, 39981, 48390, 52171, 53410,
#'   32561, 41178, 49981, 58390, 62171, 63410
#' )
#'
#' lipid2_area <- c(
#'   2299075, 4136350, 7020062, 8922063, 9288742, 11365710,
#'   2300075, 4137350, 7021062, 8923063, 9289742, 11366710
#' )
#'
#' curve_data <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Lipid1 = lipid1_area,
#'   Lipid2 = lipid2_area
#' )
#'
#' dilution_percent <- c(
#'   10, 20, 40, 60, 80, 100,
#'   10, 20, 40, 60, 80, 100
#' )
#'
#' dilution_batch_name <- c(
#'   "B1", "B1", "B1", "B1", "B1", "B1",
#'   "B2", "B2", "B2", "B2", "B2", "B2"
#' )
#'
#' curve_annot <- tibble::tibble(
#'   Sample_Name = sample_name,
#'   Dilution_Batch_Name = dilution_batch_name,
#'   Dilution_Percent = dilution_percent
#' )
#'
#' # Create dilution table
#' curve_table <- create_curve_table(
#'   curve_annot = curve_annot,
#'   curve_data_wide = curve_data,
#'   common_column = "Sample_Name",
#'   signal_var = "Area",
#'   column_group = "Transition_Name"
#' )
#'
#' # Create a ggplot table without dilution summary
#' ggplot_table_auto <- add_ggplot_panel(curve_table)
#'
#' # Get the list of ggplot list for each group
#' ggplot_list <- ggplot_table_auto$panel
#'
#' # Create a pdf report, set testing = FALSE to output results
#' pdf_page_list <- view_ggplot_pdf(ggplot_list, testing = TRUE)
#'
#' @rdname view_ggplot_pdf
#' @export
view_ggplot_pdf <- function(ggplot_list,
                            filename = "dilution_plot.pdf",
                            ncol_per_page = 2,
                            nrow_per_page = 2,
                            width = 15, height = 8,
                            testing = FALSE) {
  number_of_plots <- length(ggplot_list)

  # Get the layout for one page
  page_layout <- create_page_layout(
    number_of_plots = number_of_plots,
    ncol = ncol_per_page, nrow = nrow_per_page
  )
  # Get the number of plots per page
  # number_of_plots_per_page <- get_number_of_plots_per_page(page_layout$ncol,
  #                                                         page_layout$nrow)

  number_of_plots_per_page <- Inf

  if (!is.null(page_layout$ncol) && !is.null(page_layout$nrow)) {
    number_of_plots_per_page <- page_layout$ncol * page_layout$nrow
  } else if (!is.null(page_layout$ncol)) {
    number_of_plots_per_page <- page_layout$ncol
  } else if (!is.null(page_layout$nrow)) {
    number_of_plots_per_page <- page_layout$nrow
  }

  # Split plots over multiple pages
  if (number_of_plots > number_of_plots_per_page) {
    # Each element in the list is a list of ggplots for one pdf page
    ggplot_group_list <-
      split(
        ggplot_list,
        ceiling(seq_along(ggplot_list) / number_of_plots_per_page)
      )
  } else {
    # All plot in one pdf page
    ggplot_group_list <- list(ggplot_list)
  }

  # Each element in the list is a list of ggplots for one page plotted
  # in the layout defined in get_page_layout
  pdf_page_list <- purrr::map(ggplot_group_list, patchwork::wrap_plots,
    ncol = page_layout$ncol,
    nrow = page_layout$nrow
  )

  if (!testing) {

    # Create the setting needed to male a pdf file
    dev <- grDevices::pdf
    dev_opts <- list(
      file = filename,
      width = width,
      height = height
    )
    do.call(dev, dev_opts)

    # Output the plots to the pdf
    invisible(utils::capture.output(print(pdf_page_list)))

    # Close the pdf file
    invisible(utils::capture.output(grDevices::dev.off()))
  } else {
    return(pdf_page_list)
  }
}
