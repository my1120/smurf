
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview of the package
=======================

This package allows you to select matched controls for an exposure variable under study in environmental epidemiology. Then you will be able to conduct a matched analysis to examine the health effects of the exposure variable.

Installation
------------

You can install smurf from github with:

``` r
# install.packages("devtools")
# devtools::install_github("my1120/smurf")
```

Example
-------

``` r
library(easypackages)
libraries("smurf", "dlnm", "eesim", "dplyr", "data.table")
```

A single city dataset is available from the package "dlnm".

``` r
data("chicagoNMMAPS")
chic <- chicagoNMMAPS
head(chic)
#>         date time year month doy      dow death cvd resp       temp   dptp
#> 1 1987-01-01    1 1987     1   1 Thursday   130  65   13 -0.2777778 31.500
#> 2 1987-01-02    2 1987     1   2   Friday   150  73   14  0.5555556 29.875
#> 3 1987-01-03    3 1987     1   3 Saturday   101  43   11  0.5555556 27.375
#> 4 1987-01-04    4 1987     1   4   Sunday   135  72    7 -1.6666667 28.625
#> 5 1987-01-05    5 1987     1   5   Monday   126  64   12  0.0000000 28.875
#> 6 1987-01-06    6 1987     1   6  Tuesday   130  63   12  4.4444444 35.125
#>     rhum     pm10       o3
#> 1 95.500 26.95607 4.376079
#> 2 88.250       NA 4.929803
#> 3 89.500 32.83869 3.751079
#> 4 84.500 39.95607 4.292746
#> 5 74.500       NA 4.751079
#> 6 77.375 40.95607 6.334412
```

Simulate a multi-city dataset with continuous exposure variable.

``` r
city_a <- create_sims(n_reps = 1, n = 365 * 5, exposure_type = "continuous",
                      exposure_trend = "cos1", central = 15, sd = 1,
                      exposure_amp = 0.6,
                      average_outcome = 40, outcome_trend = "cos1linear",
                      outcome_amp = 0.6,
                      rr = 1.2, start.date = "2000-01-01")
city_a <- do.call(rbind.data.frame, city_a)
city_a$city <- "A"

city_b <- create_sims(n_reps = 1, n = 365 * 5, exposure_type = "continuous",
                      exposure_trend = "cos1", central = 30, sd = 1,
                      exposure_amp = 0.6,
                      average_outcome = 40, outcome_trend = "cos1linear",
                      outcome_amp = 0.6,
                      rr = 1.4, start.date = "2000-01-01")
city_b <- do.call(rbind.data.frame, city_b)
city_b$city <- "B"

city_c <- create_sims(n_reps = 1, n = 365 * 5, exposure_type = "continuous",
                      exposure_trend = "cos1", central = 10, sd = 1,
                      exposure_amp = 0.6,
                      average_outcome = 40, outcome_trend = "cos1linear",
                      outcome_amp = 0.6,
                      rr = 1.02, start.date = "2000-01-01")
city_c <- do.call(rbind.data.frame, city_c)
city_c$city <- "C"

mcity <- rbind(city_a, city_b, city_c) %>%
  rename(exposure = x)
head(mcity)
#>         date exposure outcome city
#> 1 2000-01-01 25.60131     308    A
#> 2 2000-01-02 25.47738     285    A
#> 3 2000-01-03 24.75466     253    A
#> 4 2000-01-04 24.10179     230    A
#> 5 2000-01-05 24.10387     183    A
#> 6 2000-01-06 24.64990     246    A
```

#### findevent

Under some circumstances, an environmental exposure event is defined based on continuous measurements. For example, heatwave is typically defined as .... \[citation\]. "findevent" function can be used to find exposure event based on some conditions.

First, here is an example of finding exposure event for a single city.

``` r
chic_xmin <- data.table::data.table(byid = 1, xmin = 30)
chic_event <- findevents(date = chic$date, x = chic$temp, xmint = chic_xmin, 
                         mindays = 2)
subset(chic_event, event == TRUE)
#>     by       date        x above above_fromstart above_toend length event
#>  1: NA 1988-06-20 31.38889     1               1           2      2  TRUE
#>  2: NA 1988-06-21 30.27778     1               2           1      2  TRUE
#>  3: NA 1988-07-14 30.55556     1               1           2      2  TRUE
#>  4: NA 1988-07-15 30.27778     1               2           1      2  TRUE
#>  5: NA 1988-08-01 31.66667     1               1           4      4  TRUE
#>  6: NA 1988-08-02 31.94444     1               2           3      4  TRUE
#>  7: NA 1988-08-03 31.11111     1               3           2      4  TRUE
#>  8: NA 1988-08-04 30.55556     1               4           1      4  TRUE
#>  9: NA 1995-07-13 33.33333     1               1           2      2  TRUE
#> 10: NA 1995-07-14 33.05556     1               2           1      2  TRUE
#> 11: NA 1995-08-12 30.27778     1               1           3      3  TRUE
#> 12: NA 1995-08-13 30.55556     1               2           2      3  TRUE
#> 13: NA 1995-08-14 30.55556     1               3           1      3  TRUE
```

Second, if the datasets contains data for multiple cities, you would like to find events within each city. Here is an example using the simulated multicities dataset.

``` r
# find the tresholds within each city
m_xmin <- mcity %>%
  group_by(city) %>%
  summarise(xmin = quantile(exposure, probs = 0.8)) %>%
  mutate(byid = 1:length(city)) %>%
  select(byid, xmin) %>%
  as.data.frame()

mevent_1 <- findevents(date = mcity$date, x = mcity$exposure, 
                      xmint = m_xmin, 
                      mindays = 2, by = mcity$city)
mevent_1
```
