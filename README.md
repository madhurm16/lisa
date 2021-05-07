# Inter-generational conflict and the declining labor share - Fabien Petit

<p align="justify"><b>Abstract</b>: The age structure of the population has received little attention as a determinant of the labor share. This paper argues that demographic dynamics in high-income countries affect the labor share in two different ways: directly through the labor supply and capital accumulation, but also indirectly through public policy. I use an OLG model in which a generational conflict arises because young and old individuals have different income sources and opposite objectives in terms of public policy. The youth face unemployment risk and use their political weight to raise the unemployment benefit, while the old favor health expenditures. This tension over the public budget allocation has consequences for wage bargaining and thus for the labor share. Numerical simulations for France and the United States indicate that the model can replicate the data and that boomers' cohorts have driven the observed decline of the labor share.</p>

# Data Availability Statements

All data used to support the findings of this study have been deposited in `_data/_raw/`.

# Dataset list

This list contains datasets used in the `main.Rmd` notebook. Original data are available in `_data/_raw/`. Original data in csv format are available in `_data/_csv/`. Transformation of data are performed with scripts in `_script/_treatment/`.

| DATA FILE               | SOURCE                    | NOTES               | PROVIDED |
| ----------------------- | ------------------------- | ------------------- | -------- |
| `_data/_final/cwed.csv`  | [CWED](http://cwed2.org/) | As per terms of use | Yes      |
| `_data/_final/demo.csv`  | [UN (WPP2017)](https://population.un.org/wpp/) | As per terms of use | Yes      |
| `_data/_final/oecd.csv`  | [OECD](https://data.oecd.org/) | As per terms of use | Yes      |
| `_data/_final/pwt.csv`   | [PWT 9.1](https://www.rug.nl/ggdc/productivity/pwt/) | As per terms of use | Yes      |
| `_data/_final/wpp.csv`   | [UN (WPP2017)](https://population.un.org/wpp/) | As per terms of use | Yes      |

# Computational requirements

## Software requirements (R Session info)

- R version 4.0.2 (2020-06-22)
- Platform: x86_64-w64-mingw32/x64 (64-bit)
- Running under: Windows 10 x64 (build 19042)

- Matrix products: default

- Attached base packages: grid, stats, graphics, grDevices, utils, datasets, methods, base     
- Other attached packages: knitr_1.29, kableExtra_1.1.0, texreg_1.37.5, gridExtra_2.3, ggpubr_0.4.0, RColorBrewer_1.1-2, ggrepel_0.8.2, ggplot2_3.3.2, zoo_1.8-8, lubridate_1.7.9, stringr_1.4.0, data.table_1.12.8, tidyselect_1.1.0, tidyr_1.1.0, reshape2_1.4.4, dplyr_1.0.2, haven_2.3.1, foreign_0.8-80    
- Loaded via a namespace (and not attached): xfun_0.15, purrr_0.3.4, lattice_0.20-41, carData_3.0-4, colorspace_1.4-1, vctrs_0.3.4, generics_0.1.0, viridisLite_0.3.0, htmltools_0.5.0, yaml_2.2.1, rlang_0.4.7, pillar_1.4.4, glue_1.4.2, withr_2.2.0, readxl_1.3.1, lifecycle_0.2.0, plyr_1.8.6, munsell_0.5.0, ggsignif_0.6.0, gtable_0.3.0, cellranger_1.1.0, rvest_0.3.5, zip_2.0.4, evaluate_0.14, rio_0.5.16, forcats_0.5.0, curl_4.3, broom_0.7.0, Rcpp_1.0.4.6, readr_1.3.1, scales_1.1.1, backports_1.1.8, webshot_0.5.2, abind_1.4-5, hms_0.5.3, digest_0.6.25, stringi_1.4.6, openxlsx_4.1.5, rstatix_0.6.0, tools_4.0.2, magrittr_1.5, tibble_3.0.3, crayon_1.3.4, car_3.0-8, pkgconfig_2.0.3, ellipsis_0.3.1, xml2_1.3.2, rmarkdown_2.6, httr_1.4.1, rstudioapi_0.11, R6_2.4.1, compiler_4.0.2

- the file `_script/init.R` will install all dependencies (latest version), and should be run once prior to running other programs.

## Descriptions of programs

- Programs in `_script/_treatment/` generates final datasets in `_data/_final/`.
- Notebook `main.Rmd` generates all graphs and tables of this study.

## Memory and Runtime Requirements

The code was last run on a 4-core Intel-based laptop with Windows version 10.20H2 and 16GB of RAM.

# Instructions

- Run `main.Rmd` to generates all graphs and tables of this study.

# List of directories and files

Each directory contains a README file describing each file and subdirectory.

| DATA FILE               | DESCRIPTION                                                    |
| ----------------------- | -------------------------------------------------------------- |
| `_data`                 | All the data (raw, intermediate and final datasets) |
| `_function`             | All the functions |
| `_graphic`              | All the graphics |
| `_script`               | All the scripts |
| `_tabular`              | All the tabulars |
| `lisa.Rproj`            | R Project settings |
| `main.md`               | Main notebook |
| `main.Rmd`              | Main notebook (R code) |
| `referencesLISA.bib`    | References in the paper|

# Acknowledgements

Structure of this file was copied from the [Template README and Guidance of The Review of Economic Studies](https://restud.github.io/data-editor/template-README/).
