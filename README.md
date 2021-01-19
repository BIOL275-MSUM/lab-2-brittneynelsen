Lab 2: Introduction to RStudio
================
brittneynelsen
2021-01-19

Welcome to my Lab 2 repository for Quantitative Biology BIOL 275 at
MSUM.

## Lab objectives

The objectives of this lab are:

  - learn how to use the console window to run commands as you perform
    basic arithmetic operations
  - learn the basics of using function
  - learn about creating objects using the assignment operator

## Repository contents

The repository contains the following scripts:

  - [calculator.R](calculator.R)
  - [calculator.Rmd](calculator.Rmd)

## Instructions for knitting

Your actual README document must be a Markdown file. This one is an R
Markdown file.

When you are done editing this document, click the Knit button in the
Source Pane. RStudio will then convert this into a .md file and give you
a preview in a separate window. Make sure everything looks okay and then
close the preview window.

## R Markdown

This README is an R Markdown document (.Rmd), which means you can use
markdown syntax and you can embed code chunks.

Here is a list of resources for helping you write R Markdown documents:

  - In the Go to the **Help** menu and select **Markdown Quick
    Reference**
  - [Basic Syntax](https://www.markdownguide.org/basic-syntax/) on the
    Markdown Guide website

## Session Info

Here is my session information:

``` r
sessioninfo::session_info(c("tidyverse"))
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 4.0.3 (2020-10-10)
    ##  os       macOS Big Sur 10.16         
    ##  system   x86_64, darwin17.0          
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       America/Chicago             
    ##  date     2021-01-19                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package      * version  date       lib source        
    ##  askpass        1.1      2019-01-13 [1] CRAN (R 4.0.2)
    ##  assertthat     0.2.1    2019-03-21 [1] CRAN (R 4.0.2)
    ##  backports      1.2.0    2020-11-02 [1] CRAN (R 4.0.2)
    ##  base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.0.2)
    ##  BH             1.72.0-3 2020-01-08 [1] CRAN (R 4.0.2)
    ##  blob           1.2.1    2020-01-20 [1] CRAN (R 4.0.2)
    ##  brio           1.1.0    2020-08-31 [1] CRAN (R 4.0.2)
    ##  broom          0.7.2    2020-10-20 [1] CRAN (R 4.0.2)
    ##  callr          3.5.1    2020-10-13 [1] CRAN (R 4.0.2)
    ##  cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.0.2)
    ##  cli            2.1.0    2020-10-12 [1] CRAN (R 4.0.2)
    ##  clipr          0.7.1    2020-10-08 [1] CRAN (R 4.0.2)
    ##  colorspace     1.4-1    2019-03-18 [1] CRAN (R 4.0.2)
    ##  cpp11          0.2.4    2020-11-05 [1] CRAN (R 4.0.2)
    ##  crayon         1.3.4    2017-09-16 [1] CRAN (R 4.0.2)
    ##  curl           4.3      2019-12-02 [1] CRAN (R 4.0.1)
    ##  DBI            1.1.0    2019-12-15 [1] CRAN (R 4.0.2)
    ##  dbplyr         2.0.0    2020-11-03 [1] CRAN (R 4.0.2)
    ##  desc           1.2.0    2018-05-01 [1] CRAN (R 4.0.2)
    ##  diffobj        0.3.2    2020-10-05 [1] CRAN (R 4.0.2)
    ##  digest         0.6.27   2020-10-24 [1] CRAN (R 4.0.2)
    ##  dplyr          1.0.2    2020-08-18 [1] CRAN (R 4.0.2)
    ##  ellipsis       0.3.1    2020-05-15 [1] CRAN (R 4.0.2)
    ##  evaluate       0.14     2019-05-28 [1] CRAN (R 4.0.1)
    ##  fansi          0.4.1    2020-01-08 [1] CRAN (R 4.0.2)
    ##  farver         2.0.3    2020-01-16 [1] CRAN (R 4.0.2)
    ##  forcats        0.5.0    2020-03-01 [1] CRAN (R 4.0.2)
    ##  fs             1.5.0    2020-07-31 [1] CRAN (R 4.0.2)
    ##  generics       0.1.0    2020-10-31 [1] CRAN (R 4.0.2)
    ##  ggplot2        3.3.2    2020-06-19 [1] CRAN (R 4.0.2)
    ##  glue           1.4.2    2020-08-27 [1] CRAN (R 4.0.2)
    ##  gtable         0.3.0    2019-03-25 [1] CRAN (R 4.0.2)
    ##  haven          2.3.1    2020-06-01 [1] CRAN (R 4.0.2)
    ##  highr          0.8      2019-03-20 [1] CRAN (R 4.0.2)
    ##  hms            0.5.3    2020-01-08 [1] CRAN (R 4.0.2)
    ##  htmltools      0.5.0    2020-06-16 [1] CRAN (R 4.0.2)
    ##  httr           1.4.2    2020-07-20 [1] CRAN (R 4.0.2)
    ##  isoband        0.2.2    2020-06-20 [1] CRAN (R 4.0.2)
    ##  jsonlite       1.7.1    2020-09-07 [1] CRAN (R 4.0.2)
    ##  knitr          1.30     2020-09-22 [1] CRAN (R 4.0.2)
    ##  labeling       0.4.2    2020-10-20 [1] CRAN (R 4.0.2)
    ##  lattice        0.20-41  2020-04-02 [1] CRAN (R 4.0.3)
    ##  lifecycle      0.2.0    2020-03-06 [1] CRAN (R 4.0.2)
    ##  lubridate      1.7.9    2020-06-08 [1] CRAN (R 4.0.2)
    ##  magrittr       1.5      2014-11-22 [1] CRAN (R 4.0.2)
    ##  markdown       1.1      2019-08-07 [1] CRAN (R 4.0.2)
    ##  MASS           7.3-53   2020-09-09 [1] CRAN (R 4.0.3)
    ##  Matrix         1.2-18   2019-11-27 [1] CRAN (R 4.0.3)
    ##  mgcv           1.8-33   2020-08-27 [1] CRAN (R 4.0.3)
    ##  mime           0.9      2020-02-04 [1] CRAN (R 4.0.2)
    ##  modelr         0.1.8    2020-05-19 [1] CRAN (R 4.0.2)
    ##  munsell        0.5.0    2018-06-12 [1] CRAN (R 4.0.2)
    ##  nlme           3.1-149  2020-08-23 [1] CRAN (R 4.0.3)
    ##  openssl        1.4.3    2020-09-18 [1] CRAN (R 4.0.2)
    ##  pillar         1.4.6    2020-07-10 [1] CRAN (R 4.0.2)
    ##  pkgbuild       1.1.0    2020-07-13 [1] CRAN (R 4.0.2)
    ##  pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.0.2)
    ##  pkgload        1.1.0    2020-05-29 [1] CRAN (R 4.0.2)
    ##  praise         1.0.0    2015-08-11 [1] CRAN (R 4.0.2)
    ##  prettyunits    1.1.1    2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx       3.4.4    2020-09-03 [1] CRAN (R 4.0.2)
    ##  progress       1.2.2    2019-05-16 [1] CRAN (R 4.0.2)
    ##  ps             1.4.0    2020-10-07 [1] CRAN (R 4.0.2)
    ##  purrr          0.3.4    2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6             2.5.0    2020-10-28 [1] CRAN (R 4.0.2)
    ##  RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 4.0.2)
    ##  Rcpp           1.0.5    2020-07-06 [1] CRAN (R 4.0.2)
    ##  readr          1.4.0    2020-10-05 [1] CRAN (R 4.0.2)
    ##  readxl         1.3.1    2019-03-13 [1] CRAN (R 4.0.2)
    ##  rematch        1.0.1    2016-04-21 [1] CRAN (R 4.0.2)
    ##  rematch2       2.1.2    2020-05-01 [1] CRAN (R 4.0.2)
    ##  reprex         0.3.0    2019-05-16 [1] CRAN (R 4.0.2)
    ##  rlang          0.4.8    2020-10-08 [1] CRAN (R 4.0.2)
    ##  rmarkdown      2.5      2020-10-21 [1] CRAN (R 4.0.3)
    ##  rprojroot      1.3-2    2018-01-03 [1] CRAN (R 4.0.2)
    ##  rstudioapi     0.11     2020-02-07 [1] CRAN (R 4.0.2)
    ##  rvest          0.3.6    2020-07-25 [1] CRAN (R 4.0.2)
    ##  scales         1.1.1    2020-05-11 [1] CRAN (R 4.0.2)
    ##  selectr        0.4-2    2019-11-20 [1] CRAN (R 4.0.2)
    ##  stringi        1.5.3    2020-09-09 [1] CRAN (R 4.0.2)
    ##  stringr        1.4.0    2019-02-10 [1] CRAN (R 4.0.2)
    ##  sys            3.4      2020-07-23 [1] CRAN (R 4.0.2)
    ##  testthat       3.0.0    2020-10-31 [1] CRAN (R 4.0.2)
    ##  tibble         3.0.4    2020-10-12 [1] CRAN (R 4.0.2)
    ##  tidyr          1.1.2    2020-08-27 [1] CRAN (R 4.0.2)
    ##  tidyselect     1.1.0    2020-05-11 [1] CRAN (R 4.0.2)
    ##  tidyverse      1.3.0    2019-11-21 [1] CRAN (R 4.0.2)
    ##  tinytex        0.27     2020-11-01 [1] CRAN (R 4.0.2)
    ##  utf8           1.1.4    2018-05-24 [1] CRAN (R 4.0.2)
    ##  vctrs          0.3.4    2020-08-29 [1] CRAN (R 4.0.2)
    ##  viridisLite    0.3.0    2018-02-01 [1] CRAN (R 4.0.1)
    ##  waldo          0.2.3    2020-11-09 [1] CRAN (R 4.0.3)
    ##  whisker        0.4      2019-08-28 [1] CRAN (R 4.0.2)
    ##  withr          2.3.0    2020-09-22 [1] CRAN (R 4.0.2)
    ##  xfun           0.19     2020-10-30 [1] CRAN (R 4.0.2)
    ##  xml2           1.3.2    2020-04-23 [1] CRAN (R 4.0.2)
    ##  yaml           2.2.1    2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
