
# datacall

<!-- badges: start -->
<!-- badges: end -->

The goal of datacall is to create a clear workflow for pulling and cleaning data for fishery stock assessments. 
It utilizes a "project oriented workflow" via RStudio. 
You must be able to have a connection to the AFSC & AKFIN data servers (e.g., VPN if offsite), and have usernames/passwords setup.

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
admb_home = "C:/Program Files (x86)/ADMB-12.1" # location ADMB exists on *my* computer
TAC = c(3786, 3681, 4528) # last three years of TAC (year-3, year-2, year-1)
rec_age = 2 # recruitment age
plus_age = 45 # plus group age

# setup folders -----
modeldir(year)

# query databases ----
raw_data(species, year, afsc_user, afsc_pwd, akfin_user, akfin_pwd)
```

The initial output will be raw data pulls that will be placed in the  `year/data/raw` folder. 

Then clean up the raw data which will be placed in the `year/data/output` folder.

```{r}
# catch and biomass data ----
clean_catch(year, TAC)

#note: must provide file for VAST estimates in the "user_input" folder, otherwise this will output the design-based biomass estimate
survey_biomass(year, "VAST_estimates.csv")


# biological data ----
clean_catch(year, TAC)
survey_biomass(year)
fish_age_comp(year, rec_age, plus_age)
survey_age_comp(year, rec_age, plus_age)
fish_size_comp(year, rec_age)
survey_size_comp(year)
size_at_age(year, admb_home, rec_age, plus_age)
weight_at_age(year, admb_home, rec_age, plus_age)
ageage(reader_tester = NULL, species, year, admb_home) # read_tester file is provide in the "user_input" folder

```

A .dat file will be placed in the model folder (users choice)

```{r}
# create dat file ----
concat_dat(year, model, rec_age, plus_age)
```

Run the ADMB model.  
Clean up model results.

```{r}
process_results(year, model, model_name, rec_age, plus_age)
plot_catch(year, model)
plot_survey(year, model)
plot_selex(year, model)
plot_params(year, model, model_name)
plot_phase(year, model, model_name)
plot_rec(year, model)
plot_rec_ssb(year, model, rec_age)
plot_swath(year, model)
plot_comps(year, model)
```






