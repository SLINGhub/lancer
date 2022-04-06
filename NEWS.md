# DCVtestkit 0.0.2.9000 (development version)

## TODO

* Check if R package can work with the new version of R.
* Regarding the spurious cases of the dilution curves: flag curves every time they have a PRA value close to 0.8?
* Flag curves with negative correlation close to 1

## Completed

* Add the hex sticker logo for this package.
* Correct README to ensure the add_ggplot_panel function is correctly used.

# DCVtestkit 0.0.2

* Changed output excel sheet font to "Consolas" so that the number "0" and the letter "O" can be differentiated easily.
* In `cognostics.R`, function `convert_to_cog`, add code to ensure logical columns are converted to character columns as logical column will turn to NA when `trelliscopejs::as_cognostics` is applied

# DCVtestkit 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Aim to create a tag version.
