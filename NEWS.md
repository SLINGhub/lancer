# lancer 0.0.6.9000 (development version)

## TODO

* Change `lancer` functions that has `dilution` to `curve`

## Completed

* `validate_dilution_annot` changed to `validate_curve_annot`.
* `validate_dilution_table` changed to `validate_curve_table`.
* `validate_dilution_data` changed to `validate_curve_data`.
* `validate_lipid_data_wide` changed to `validate_curve_data_wide`.
* `create_dilution_table` changed to `create_curve_table`.
* `summarise_dilution_data` changed to `summarise_curve_data`.

# lancer 0.0.6

* Change characterisation of LOD curves from LOD to Noise Regime.
* Change characterisation of Saturation curves from Saturation to Saturation Regime.

# lancer 0.0.5

* Change the R package name to lancer.
* Use `actions/checkout@v3` in the other GitHub Actions files.
* Add alternate text to the documentation.

# lancer 0.0.4

* Fix issue that `r_corr` from `calculate_gof_linear` gives a named numeric vector.
* Change code to suit the changes highlighted in [tidyselect 1.2.0](https://www.tidyverse.org/blog/2022/10/tidyselect-1-2-0/) .
* Use `actions/checkout@v3` and `actions/cache@v3` in `R-CMD-check.yaml` file 

# lancer 0.0.3

* Add the hex sticker logo for this package.
* Correct README to ensure the `add_ggplot_panel` function is correctly used.
* Fix issue in `add_ggplot_panel` and `add_plotly_panel` function when users key in the `dil_batch_var` but is not also a `grouping variable`.
* Fix issue when `view_trellis_html` cannot display plots when there is input for `grouping variable` is a vector of length one.

# lancer 0.0.2

* Changed output excel sheet font to "Consolas" so that the number "0" and the letter "O" can be differentiated easily.
* In `cognostics.R`, function `convert_to_cog`, add code to ensure logical columns are converted to character columns as logical column will turn to NA when `trelliscopejs::as_cognostics` is applied.

# lancer 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Aim to create a tag version.
