
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DCVtestkit (Dilution Curve Validation Testkit)

<!-- badges: start -->

<!-- badges: end -->

R package used to validate if a dilution curve is linear or not.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JauntyJJS/DCVtestkit")
```

## How it works

We try to categorise dilution curves based on the results of three
parameters

  - Correlation Coefficient (![R](https://latex.codecogs.com/png.latex?R
    "R"))
  - Mandel’s Fitting Test  
  - Percent Residual Accuracy
    (![PRA](https://latex.codecogs.com/png.latex?PRA "PRA"))

Correlation Coefficient (![R](https://latex.codecogs.com/png.latex?R
"R")) can be found in this
[paper](https://link.springer.com/article/10.1007/s00769-002-0487-6).
Equation (![1](https://latex.codecogs.com/png.latex?1 "1")) is used.

Mandel’s Fitting Test can be found in this
[paper](https://pubs.rsc.org/en/content/articlelanding/2013/ay/c2ay26400e#!divAbstract).
Equation (![5](https://latex.codecogs.com/png.latex?5 "5")) is used.

Percent Residual Accuracy
(![PRA](https://latex.codecogs.com/png.latex?PRA "PRA")) can be found in
this
[paper](https://www.sciencedirect.com/science/article/abs/pii/S0039914018307549).
Equation (![6](https://latex.codecogs.com/png.latex?6 "6")) is used.

Two methods are proposed to categorise the dilution curves.

### Workflow 1

Workflow 1 involves using ![R](https://latex.codecogs.com/png.latex?R
"R") and ![PRA](https://latex.codecogs.com/png.latex?PRA "PRA") to
categorise the dilution curves.

  - If ![R \< 0.8](https://latex.codecogs.com/png.latex?R%20%3C%200.8
    "R \< 0.8"), classify as poor linearity.  
  - If ![R
    \\ge{0.8}](https://latex.codecogs.com/png.latex?R%20%5Cge%7B0.8%7D
    "R \\ge{0.8}"), ![PRA
    \< 80](https://latex.codecogs.com/png.latex?PRA%20%3C%2080
    "PRA \< 80"), classify as poor linearity.  
  - If ![R
    \\ge{0.8}](https://latex.codecogs.com/png.latex?R%20%5Cge%7B0.8%7D
    "R \\ge{0.8}"), ![PRA
    \\ge 80](https://latex.codecogs.com/png.latex?PRA%20%5Cge%2080
    "PRA \\ge 80"), classify as good linearity.

### Workflow 2

Workflow 1 involves using ![R](https://latex.codecogs.com/png.latex?R
"R"), ![PRA](https://latex.codecogs.com/png.latex?PRA "PRA") and
Mandel’s Fitting Test to categorise the dilution curves.

  - If ![R \< 0.8](https://latex.codecogs.com/png.latex?R%20%3C%200.8
    "R \< 0.8"), classify as poor linearity.
  - If ![R
    \\ge{0.8}](https://latex.codecogs.com/png.latex?R%20%5Cge%7B0.8%7D
    "R \\ge{0.8}"), ![PRA
    \< 80](https://latex.codecogs.com/png.latex?PRA%20%3C%2080
    "PRA \< 80") and fit quadratic model and use Mandel’s Fitting Test
    to see it is better
      - If not better, classify as poor linearity
      - If better, check concavity of the quadratic model
          - If concavity is negative, classify as saturation
          - If concavity is positive, classify as limit of detection
            (LOD)
  - If ![R
    \\ge{0.8}](https://latex.codecogs.com/png.latex?R%20%5Cge%7B0.8%7D
    "R \\ge{0.8}"), ![PRA
    \\ge 80](https://latex.codecogs.com/png.latex?PRA%20%5Cge%2080
    "PRA \\ge 80"), classify as good linearity

## Usage

We first create our data set.

``` r
library(DCVtestkit)
# Data Creation
dilution_percent <- c(10, 20, 25, 40, 50, 60,
                      75, 80, 100, 125, 150,
                      10, 25, 40, 50, 60,
                      75, 80, 100, 125, 150)
dilution_batch <- c("B1", "B1", "B1", "B1", "B1",
                    "B1", "B1", "B1", "B1", "B1", "B1",
                    "B2", "B2", "B2", "B2", "B2",
                    "B2", "B2", "B2", "B2", "B2")
sample_name <- c("Sample_010a", "Sample_020a", "Sample_025a",
                 "Sample_040a", "Sample_050a", "Sample_060a",
                 "Sample_075a", "Sample_080a", "Sample_100a",
                 "Sample_125a", "Sample_150a",
                 "Sample_010a", "Sample_025a",
                 "Sample_040a", "Sample_050a", "Sample_060a",
                 "Sample_075a", "Sample_080a", "Sample_100a",
                 "Sample_125a", "Sample_150a")
lipid1_area_saturated <- c(5748124, 16616414, 21702718, 36191617,
                           49324541, 55618266, 66947588, 74964771,
                           75438063, 91770737, 94692060,
                           5192648, 16594991, 32507833, 46499896,
                           55388856, 62505210, 62778078, 72158161,
                           78044338, 86158414)
lipid2_area_linear <- c(31538, 53709, 69990, 101977, 146436, 180960,
                        232881, 283780, 298289, 344519, 430432,
                        25463, 63387, 90624, 131274, 138069,
                        205353, 202407, 260205, 292257, 367924)
lipid3_area_lod <- c(544, 397, 829, 1437, 1808, 2231,
                     3343, 2915, 5268, 8031, 11045,
                     500, 903, 1267, 2031, 2100,
                     3563, 4500, 5300, 8500, 10430)
lipid4_area_nonlinear <- c(380519, 485372, 478770, 474467, 531640, 576301,
                           501068, 550201, 515110, 499543, 474745,
                           197417, 322846, 478398, 423174, 418577,
                           426089, 413292, 450190, 415309, 457618)

dilution_annot <- tibble::tibble(Sample_Name = sample_name,
                                 Dilution_Batch = dilution_batch,
                                 Dilution_Percent = dilution_percent)
lipid_data <- tibble::tibble(Sample_Name = sample_name,
                             Lipid1 = lipid1_area_saturated,
                             Lipid2 = lipid2_area_linear,
                             Lipid3 = lipid3_area_lod,
                             Lipid4 = lipid4_area_nonlinear)
```

We merge the data together using `create_dilution_table` and summarise a
dilution curve for each transition and batch.

``` r
# Create dilution table and dilution statistical summary
dilution_summary <- create_dilution_table(dilution_annot, lipid_data,
                                          common_column = "Sample_Name",
                                          signal_var = "Area",
                                          column_group = "Transition_Name"
                                          ) %>% 
    summarise_dilution_table(grouping_variable = c("Transition_Name",
                                                   "Dilution_Batch"),
                             conc_var = "Dilution_Percent",
                             signal_var = "Area")

dilution_summary
#> # A tibble: 8 x 13
#>   Transition_Name Dilution_Batch curve_group1 curve_group2 r_corr pra_linear
#>   <chr>           <chr>          <chr>        <chr>         <dbl>      <dbl>
#> 1 Lipid1          B1             Poor Linear~ Saturation    0.952       65.8
#> 2 Lipid2          B1             Good Linear~ Good Linear~  0.978       90.2
#> 3 Lipid3          B1             Poor Linear~ LOD           0.971       72.9
#> 4 Lipid4          B1             Poor Linear~ Poor Linear~  0.344     -233. 
#> 5 Lipid1          B2             Poor Linear~ Saturation    0.949       64.6
#> 6 Lipid2          B2             Good Linear~ Good Linear~  0.976       90.0
#> 7 Lipid3          B2             Poor Linear~ LOD           0.969       72.4
#> 8 Lipid4          B2             Poor Linear~ Poor Linear~  0.384     -172. 
#> # ... with 7 more variables: mandel_p_val <dbl>, concavity <dbl>,
#> #   r2_linear <dbl>, r2_adj_linear <dbl>, bic_linear <dbl>, corr_p_val <dbl>,
#> #   mandel_stats <dbl>
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />
