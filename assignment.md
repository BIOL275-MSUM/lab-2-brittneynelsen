Lab 2 Assignment
================
brittneynelsen
2021-01-21

## Instructions

Replace any triple underscores "\_\_\_" with the appropriate text.

Copy the appropriate code from your R script
[calculator.R](calculator.R) and paste it into the code chunks below.

## Code Practice

The final section of the Lab 2 instructions asks me to imagine I have
collected a sample of data which consisted of the following values:

    `6.05 4.89 3.32 4.93 5.25 5.04 4.91 2.84 5.60 5.34`

First, I combine the values into a vector using the `c()` function,
assign it to a variable named `k`, and print `k`

``` r
c(k <- c(6.05, 4.89, 3.32, 4.93, 5.25, 5.04, 4.91, 2.84, 5.60, 5.34))
```

    ##  [1] 6.05 4.89 3.32 4.93 5.25 5.04 4.91 2.84 5.60 5.34

Here I calculate the sample size, mean, median, and standard deviation:

``` r
length(k)  # sample size
```

    ## [1] 10

``` r
median(k)  # median value
```

    ## [1] 4.985

``` r
mean(k)    # mean value
```

    ## [1] 4.817

``` r
sd(k)      # standard deviation
```

    ## [1] 0.9899725

``` r
n <- length(k) # sample size
n
```

    ## [1] 10

The standard error of the mean can be calculated by dividing the
standard deviation by the square root of the sample size:

``` r
sem <- sd(k) / sqrt(n) # standard error of the mean
sem
```

    ## [1] 0.3130568

And finally, I can see the 95% confidence interval ranges from a minimum
of 4.203409 to a maximum of 5.430591

``` r
mean(k) + 1.96 * sem                             # upper limit
```

    ## [1] 5.430591

``` r
mean(k) - 1.96 * sem                             # lower limit
```

    ## [1] 4.203409

``` r
c(mean(k) + 1.96 * sem, mean(k) - 1.96 * sem)    # both limits combined
```

    ## [1] 5.430591 4.203409

In this assignment I focused on using a single set of numbers, aka a
“vector”.

In the next lab we will begin using data frames, which are tables of
data consisting of multiple vectors.

As an example, imagine that the numbers above represent the weights of
chicks in an experiment. The first five numbers come from chicks that
were fed organic grain while the second were fed non-organic grain.

We could visualize the data as follows.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# check to make sure you did the part above correctly
# if not, generate some fake k data
if (!exists("k") | !is.numeric(k) | !length(k) == 10) k <- rep(1, 10)

# put the chick data into a data frame
chick_data <-
  tibble::tibble(
    diet = rep(c("Combined", "Combined", "Organic", "Non-Organic"), each = 5),
    weight = c(k, k)
  )

# summarize the chick weights by diet
chick_summary <-
  chick_data %>% 
  group_by(diet) %>% 
  summarize(
    mean = mean(weight),
    median = median(weight),
    sd = sd(weight),
    n = n(),
    sem = sd / sqrt(n),
    upper = mean + 1.96 * sem,   # upper confidence limit
    lower = mean - 1.96 * sem,   # lower confidence limit
    .groups = "drop"
  )

# plot chick weights
ggplot(chick_data, aes(x = diet, y = weight, color = diet, fill = diet)) +
  geom_jitter(size = 5, shape = 21, alpha = 0.5, width = 0.1) +
  geom_crossbar(
    mapping = aes(ymin = lower, ymax = upper, y = mean), 
    data = chick_summary,
    width = 0.2, alpha = 0.2
  ) +
  labs(
    title = "Weights of chicks fed organic and non-organic grain",
    k = "Diet",
    color = "Diet",
    fill = "Diet",
    y = "Weight (g)"
  )
```

![](assignment_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> \#
Session Info

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
    ##  date     2021-01-21                  
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
    ##  dplyr        * 1.0.2    2020-08-18 [1] CRAN (R 4.0.2)
    ##  ellipsis       0.3.1    2020-05-15 [1] CRAN (R 4.0.2)
    ##  evaluate       0.14     2019-05-28 [1] CRAN (R 4.0.1)
    ##  fansi          0.4.1    2020-01-08 [1] CRAN (R 4.0.2)
    ##  farver         2.0.3    2020-01-16 [1] CRAN (R 4.0.2)
    ##  forcats      * 0.5.0    2020-03-01 [1] CRAN (R 4.0.2)
    ##  fs             1.5.0    2020-07-31 [1] CRAN (R 4.0.2)
    ##  generics       0.1.0    2020-10-31 [1] CRAN (R 4.0.2)
    ##  ggplot2      * 3.3.2    2020-06-19 [1] CRAN (R 4.0.2)
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
    ##  purrr        * 0.3.4    2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6             2.5.0    2020-10-28 [1] CRAN (R 4.0.2)
    ##  RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 4.0.2)
    ##  Rcpp           1.0.5    2020-07-06 [1] CRAN (R 4.0.2)
    ##  readr        * 1.4.0    2020-10-05 [1] CRAN (R 4.0.2)
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
    ##  stringr      * 1.4.0    2019-02-10 [1] CRAN (R 4.0.2)
    ##  sys            3.4      2020-07-23 [1] CRAN (R 4.0.2)
    ##  testthat       3.0.0    2020-10-31 [1] CRAN (R 4.0.2)
    ##  tibble       * 3.0.4    2020-10-12 [1] CRAN (R 4.0.2)
    ##  tidyr        * 1.1.2    2020-08-27 [1] CRAN (R 4.0.2)
    ##  tidyselect     1.1.0    2020-05-11 [1] CRAN (R 4.0.2)
    ##  tidyverse    * 1.3.0    2019-11-21 [1] CRAN (R 4.0.2)
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
