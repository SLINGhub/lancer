# DCVtestkit 0.0.3.9000 (development version)

## TODO

* Fix issue that `r_corr` from `calculate_gof_linear` gives a named numeric vector.

# DCVtestkit 0.0.3

* Add the hex sticker logo for this package.
* Correct README to ensure the `add_ggplot_panel` function is correctly used.
* Fix issue in `add_ggplot_panel` and `add_plotly_panel` function when users key in the `dil_batch_var` but is not also a `grouping variable`.
* Fix issue when `view_trellis_html` cannot display plots when there is input for `grouping variable` is a vector of length one.

# DCVtestkit 0.0.2

* Changed output excel sheet font to "Consolas" so that the number "0" and the letter "O" can be differentiated easily.
* In `cognostics.R`, function `convert_to_cog`, add code to ensure logical columns are converted to character columns as logical column will turn to NA when `trelliscopejs::as_cognostics` is applied.

# DCVtestkit 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Aim to create a tag version.
