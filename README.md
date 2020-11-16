<!-- README.md is generated from README.Rmd. Please edit that file -->

DCVtestkit (Dilution Curve Validation Testkit)
==============================================

<!-- badges: start -->

[![R-CMD-check](https://github.com/SLINGhub/DCVtestkit/workflows/R-CMD-check/badge.svg)](https://github.com/SLINGhub/DCVtestkit/actions)
[![Codecov test
coverage](https://codecov.io/gh/SLINGhub/DCVtestkit/branch/master/graph/badge.svg?token=RNlP8VlaL2)](https://codecov.io/gh/SLINGhub/DCVtestkit)
<!-- badges: end -->

R package used to validate if a dilution curve is linear or not.

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("SLINGhub/DCVtestkit")

If this repository is private, go to this
[link](https://maurolepore.netlify.app/2017/12/06/2017-12-06-best-prectice-for-installing-packages-from-private-repos/)
to learn how to set up your authorization token.

How it works
------------

We try to categorise dilution curves based on the results of three
parameters

-   Correlation Coefficient ( *R* )
-   Percent Residual Accuracy ( *P**R**A* )
-   Mandel’s Fitting Test

Correlation Coefficient ( *R* ) can be found in Van Loco, J., Elskens,
M., Croux, C. *et al.*, Linearity of calibration curves: use and misuse
of the correlation coefficient. *Accred Qual Assur* **7**, 281-285
(2002).
[10.1007/s00769-002-0487-6](https://doi.org/10.1007/s00769-002-0487-6).  
Equation ( 1 ) is used.

Mandel’s Fitting Test can be found in Andrade, J. M. and
Gómez-Carracedo, M. P., Notes on the use of Mandel’s test to check for
nonlinearity in laboratory calibrations. *Anal. Methods* **5**,
1145-1149 (2013).
[10.1039/C2AY26400E](https://dx.doi.org/10.1039/C2AY26400E).  
Equation ( 5 ) is used.

Percent Residual Accuracy ( *P**R**A* ) can be found in Logue, B. A. and
Manandhar, E., Percent residual accuracy for quantifying goodness-of-fit
of linear calibration curves. *Talanta* **189**, 527-533 (2018).
[10.1016/j.talanta.2018.07.046](https://doi.org/10.1016/j.talanta.2018.07.046).  
Equation ( 6 ) is used.

Two methods are proposed to categorise the dilution curves.

### Workflow 1 (Fit For Purpose)

Workflow 1 involves using *R* and *P**R**A* to categorise the dilution
curves.

-   If *R* &lt; 0.8, classify as poor linearity.  
-   If *R* ≥ 0.8, *P**R**A* &lt; 80, classify as poor linearity.  
-   If *R* ≥ 0.8, *P**R**A* ≥ 80, classify as good linearity.

### Workflow 2

Workflow 2 involves using *R*, *P**R**A* and Mandel’s Fitting Test to
categorise the dilution curves.

-   If *R* &lt; 0.8, classify as poor linearity.
-   If *R* ≥ 0.8, *P**R**A* &lt; 80, fit the quadratic model and use
    Mandel’s Fitting Test to see if the quadratic model is a better fit
    ( p value  &lt; 0.05 ).
    -   If not better, classify as poor linearity
    -   If better, check concavity of the quadratic model
        -   If concavity is negative, classify as saturation
        -   If concavity is positive, classify as limit of detection
            (LOD)
-   If *R* ≥ 0.8, *P**R**A* ≥ 80, classify as good linearity

Usage
-----

We first create our data set.

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
                     "Sample_010b", "Sample_025b",
                     "Sample_040b", "Sample_050b", "Sample_060b",
                     "Sample_075b", "Sample_080b", "Sample_100b",
                     "Sample_125b", "Sample_150b")
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

Merge the data together using `create_dilution_table`

    # Create dilution table
    dilution_table <- create_dilution_table(dilution_annot, lipid_data,
                                            common_column = "Sample_Name",
                                            signal_var = "Area",
                                            column_group = "Transition_Name"
                                            )

    print(dilution_table, width = 100)
    #> # A tibble: 84 x 5
    #>    Sample_Name Dilution_Batch Dilution_Percent Transition_Name     Area
    #>    <chr>       <chr>                     <dbl> <chr>              <dbl>
    #>  1 Sample_010a B1                           10 Lipid1           5748124
    #>  2 Sample_010a B1                           10 Lipid2             31538
    #>  3 Sample_010a B1                           10 Lipid3               544
    #>  4 Sample_010a B1                           10 Lipid4            380519
    #>  5 Sample_020a B1                           20 Lipid1          16616414
    #>  6 Sample_020a B1                           20 Lipid2             53709
    #>  7 Sample_020a B1                           20 Lipid3               397
    #>  8 Sample_020a B1                           20 Lipid4            485372
    #>  9 Sample_025a B1                           25 Lipid1          21702718
    #> 10 Sample_025a B1                           25 Lipid2             69990
    #> # … with 74 more rows

Summarise each dilution curve for each transition and batch with
`summarise_dilution_table`

    # Create dilution statistical summary
    dilution_summary <- summarise_dilution_table(dilution_table,
                                                 grouping_variable = c("Transition_Name",
                                                                        "Dilution_Batch"),
                                                 conc_var = "Dilution_Percent",
                                                 signal_var = "Area")

    print(dilution_summary, width = 100)
    #> # A tibble: 8 x 9
    #>   Transition_Name Dilution_Batch r_corr r2_linear r2_adj_linear mandel_stats
    #>   <chr>           <chr>           <dbl>     <dbl>         <dbl>        <dbl>
    #> 1 Lipid1          B1              0.963    0.928        0.920         71.2  
    #> 2 Lipid2          B1              0.990    0.980        0.978          2.53 
    #> 3 Lipid3          B1              0.964    0.930        0.922        106.   
    #> 4 Lipid4          B1              0.311    0.0970      -0.00333       13.2  
    #> 5 Lipid1          B2              0.950    0.903        0.890         52.9  
    #> 6 Lipid2          B2              0.995    0.990        0.988          0.868
    #> 7 Lipid3          B2              0.978    0.956        0.951         20.9  
    #> 8 Lipid4          B2              0.608    0.370        0.291          5.39 
    #>   mandel_p_val pra_linear concavity
    #>          <dbl>      <dbl>     <dbl>
    #> 1   0.0000297        70.5 -4174.   
    #> 2   0.150            92.8    -4.91 
    #> 3   0.00000678       71.2     0.468
    #> 4   0.00660        -251.    -20.5  
    #> 5   0.000166         62.3 -4137.   
    #> 6   0.382            94.3    -1.94 
    #> 7   0.00256          74.7     0.321
    #> 8   0.0533          -73.1   -22.9

Classify each dilution curve according to Workflow 1 and Workflow 2.  
`wf1_group1` gives the results of Workflow 1  
`wf2_group2` gives the results of Workflow 2

    dilution_classified <- evaluate_linearity(dilution_summary,
                                              grouping_variable = c("Transition_Name",
                                                                    "Dilution_Batch"))

    print(dilution_classified, width = 100)
    #> # A tibble: 8 x 11
    #>   Transition_Name Dilution_Batch wf1_group      wf2_group      r_corr pra_linear
    #>   <chr>           <chr>          <chr>          <chr>           <dbl>      <dbl>
    #> 1 Lipid1          B1             Poor Linearity Saturation      0.963       70.5
    #> 2 Lipid2          B1             Good Linearity Good Linearity  0.990       92.8
    #> 3 Lipid3          B1             Poor Linearity LOD             0.964       71.2
    #> 4 Lipid4          B1             Poor Linearity Poor Linearity  0.311     -251. 
    #> 5 Lipid1          B2             Poor Linearity Saturation      0.950       62.3
    #> 6 Lipid2          B2             Good Linearity Good Linearity  0.995       94.3
    #> 7 Lipid3          B2             Poor Linearity LOD             0.978       74.7
    #> 8 Lipid4          B2             Poor Linearity Poor Linearity  0.608      -73.1
    #>   mandel_p_val concavity r2_linear r2_adj_linear mandel_stats
    #>          <dbl>     <dbl>     <dbl>         <dbl>        <dbl>
    #> 1   0.0000297  -4174.       0.928        0.920         71.2  
    #> 2   0.150         -4.91     0.980        0.978          2.53 
    #> 3   0.00000678     0.468    0.930        0.922        106.   
    #> 4   0.00660      -20.5      0.0970      -0.00333       13.2  
    #> 5   0.000166   -4137.       0.903        0.890         52.9  
    #> 6   0.382         -1.94     0.990        0.988          0.868
    #> 7   0.00256        0.321    0.956        0.951         20.9  
    #> 8   0.0533       -22.9      0.370        0.291          5.39

Results can be exported to Excel via `create_excel_report`

    create_excel_report(dilution_classified, file_name = "dilution_summary.xlsx")

![Excel Report](man/figures/README-ExcelResults.png)
