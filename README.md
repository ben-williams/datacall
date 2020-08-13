
# datacall

<!-- badges: start -->
<!-- badges: end -->

The goal of datacall is to create a clear workflow for pulling and cleaning data for fishery stock assessments.
You must be able to have a connection to the AFSC & AKFIN data servers (e.g., VPN if offsite).

## Installation

You can install the released version of datacall from [github](https://github.com/ben-williams/datacall) with:

``` r
# install.packages("devtools")
devtools::install_github("ben-williams/datacall")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(datacall)
raw_data("NORK", 2020, afsc_user, afsc_pwd, akfin_user, akfin_pwd)
```

