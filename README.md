
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
species = "NORK" # northern rockfish
year = 2020 # assessment year
afsc_user = "your_afsc_username"
afsc_pwd = "your_afsc_password"
akfin_user = "your_akfn_username"
akfin_pwd = "your_akfin_password"
admb_home = "C:/Program Files (x86)/ADMB-12.1" # location ADMB exists on your computer
TAC = c(3786, 3681, 4528) # last three years of TAC (year-3, year-2, year-1)
rec_age = 2 # recruitment age
plus_age = 45 # plus group age

# setup folders -----
modeldir(year)

# query databases ----
raw_data(species, year, afsc_user, afsc_pwd, akfin_user, akfin_pwd)

# catch and biomass data ----
clean_catch(year, TAC)

#note: must provide file for VAST estimates in the "user_input" folder, otherwise this will output the design-based biomass estimate
survey_biomass(year, "VAST_estimates.csv")


# biological data ----
allometric(year, admb_home, recage, plus_age)
fish_age_comp(year, rec_age, plus_age)
survey_age_comp(year, rec_age, plus_age)
fish_size_comp(year, rec_age)
survey_size_comp(year)
ageage(reader_tester = NULL, species, year, admb_home) # read_tester file is provide in the "user_input" folder
weight_length(year, admb_home, rec_age, plus_age)

# create dat file ----
concat_dat(rec_age, plus_age)
```
The initial output will be raw data pulls that will be placed in a folder `year/data/raw`. 
Processed data is placed in the `year/data/output` folder.

