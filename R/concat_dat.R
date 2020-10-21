#' Concatenate a .dat file
#'
#' @param year assessment year
#' @param model folder that the `.tpl` will be in
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param spawn_mo spawning month
#' @param n_ageagenumber of age error transmission matrices default is 1
#' @param n_sizeage number of size at age transmission matrices default is 1
#' @param lenbins set to base unless using alt in which case the file should be in the `user_input`` folder and the name needs to be provided e.g., `lengthbins.csv` - the column must be named `len_bin`
#' @retro not yet implemented
#' @return
#' @export concat_dat
#'
#' @examples concat_dat(year = 2020, model = "base", rec_age = 2, plus_age = 45)
#'
concat_dat <- function(year, model, rec_age, plus_age, spawn_mo = 5, n_ageage = NULL, n_sizeage = NULL, lenbins = NULL, retro = NULL){

  if(is.null(lenbins)){
    lenbins = read.csv(here::here(year, "data/user_input/len_bin_labels.csv"))$len_bins
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }



  catch = read.csv(here::here(year, "data", "output", "catch.csv"))
  waa = read.csv(here::here(year, "data", "output", "waa.csv"))
  saa = read.csv(here::here(year, "data", "output", "SaA.csv"))
  ae = read.csv(here::here(year, "data", "output", "ae_model.csv"))
  fishac = read.csv(here::here(year, "data", "output", "fish_age_comp.csv"))
  fishsc = read.csv(here::here(year, "data", "output", "fish_size_comp.csv"))
  srvac = read.csv(here::here(year, "data", "output", "survey_age_comp.csv"))
  tssc = read.csv(here::here(year, "data", "output", "survey_size_comp.csv"))
  tsb = read.csv(here::here(year, "data", "output", "survey_biomass.csv"))
  names(tsb) <- c("year", "biomass", "se", "lci", "uci")
  m_nages = nrow(ae)
  nages = length(rec_age:plus_age)

  # get length bin info
  lbin = as.numeric(gsub("[^0-9.]", "",  colnames(tssc)))
  lbin = lbin[!is.na(lbin)]
  nlenbins = length(lbin)

  if (!dir.exists(here::here(year, model))){
    dir.create(here::here(year, model), recursive=TRUE)
  }

  if(is.null(n_ageage)){
    n_ageage = 1
  }

  if(is.null(n_sizeage)){
    n_sizeage = 1
  }

  sep = "# -------------------------------------------------------------------"

  # header ----
  header = c(sep,
             "# GOA Northern Rockfish .dat file for ADMB optimization",
             paste0 ("# New data provided on, ", read.table(file = here::here(year, "data/raw/data_called.txt"),
                                                             sep = "\t")[2,1]),
             "# Notes:",
             "#   ~ Total catch prior to 1992 frozen",
             "#   ~ Total catch from 1993 on uses catch downloaded from AKFIN",
             "#   ~ Weight-at-age and length-age transition matrix automatically updated",
             "#   ~ Formatted to conduct automated retrospective analysis",
             "#   ~ Does not use most recent years fishery size data",
             "#   ~ Does not use fishery size data in years when ages are expected",
             sep,
             "#",
             "#")

  # model inputs ----
  mipv <- c(sep,
            "# Model input parameters/vectors",
            sep,
            "# Start and end years, recruitment age, number of age and length bins",
            "# Model start year (styr):",
            as.character(min(catch$Year)),
            "# Model end year (endyr): #!",
            as.character(year),
            "# Age at recruitment (rec_age): #-",
            as.character(rec_age),
            "# Number of ages in data (nages_D):",
            as.character(nages),
            "# Number of ages in model (nages_M):",
            as.character(m_nages),
            "# Number of length bins (nlenbins):",
            as.character(nlenbins),
            "# Number of age-age transition matrices (n_ageage_mat):",
            as.character(n_ageage),
            "# Number of size-age transition matrices (n_sizeage_mat):",
            as.character(n_sizeage),
            "# Length bin labels (len_bin_labels):",
            paste(lbin, collapse=" "),
            "# Spawn month (spawn_fract):",
            as.character(spawn_mo),
            "# Weight-at-age (wt):",
            paste(waa$x, collapse=" "),
            "#",
            "#")
  # fishery catch ----
  fishery_catch = c(sep,
                    "# Fishery catch (mt): obs_catch(styr,endyr)",
                    sep,
                    paste0("#! ", paste(min(catch$Year):year, collapse=" ")),
                    paste(catch$Catch, collapse=" "),
                    "#-",
                    "",
                    "")
  # cpue ----
  # not currently used for northern rockfish
  cpue = c(sep,
           "# CPUE Data",
           sep,
           "# Number of CPUE years",
           "0",
           "# CPUE observations (leave blank if 0)",
           "",
           "")

  # trawl biomass ----
  trawl_biomass = c(sep,
                    "# Trawl Survey Biomass",
                    sep,
                    "#! Number of trawl surveys: nyrs_srv1",
                    as.character(nrow(tsb)),
                    "#- Trawl survey years: yrs_srv1(1,nyrs_srv1) #!",
                    paste(tsb$year, collapse=" "),
                    "#- Observed trawl survey biomass (mt): obs_srv1_biom(1,nyrs_srv1) #!",
                    paste(tsb$biomass, collapse=" "),
                    "#- SE of observed trawl survey biomass: obs_srv1_se(1,nyrs_srv1) #!",
                    paste(tsb$se, collapse=" "),
                    "#- Lower CI, 1.96*SE #!",
                    paste(tsb$lci, collapse=" "),
                    "#- Upper CI, 1.96*SE #!",
                    paste(tsb$uci, collapse=" "),
                    "#-",
                    "",
                    "")
  # long line survey biomass ----
  # not currently used for northern rockfish

  ll_biomass = c(
    sep,
    "# Longline Survey Biomass",
    sep,
    "# Number of longline surveys: nyrs_srv2",
    "1",
    "# Longline survey years: yrs_srv2(1,nyrs_srv2)",
    "1999",
    "# Observed longline survey biomass (mt): obs_srv2_biom(1,nyrs_srv2)",
    "1000",
    "# SE of observed longline survey biomass: obs_srv2_se(1,nyrs_srv2)",
    "100",
    "# Lower CI, 1.96*SE",
    "10",
    "# Upper CI, 1.96*SE",
    "10000",
    "",
    "")

  # fishery age comp ----
  fac <- c(
    sep,
    "# Fishery Age Composition",
    sep,
    "#! Number of years: nyrs_fish_age",
    as.character(nrow(fishac)),
    "#- Fishery age comp years: yrs_fish_age #!",
    paste(fishac$year, collapse=" "),
    "#- Number of samples: nsamples_fish_age(1,nyrs_fish_age) #!",
    paste(fishac$n_s, collapse=" "),
    "#- Number of hauls: nhauls_fish_age(1,nyrs_fish_age) #!",
    paste(fishac$n_h, collapse=" "),
    "#- Index for age-age error matrix #!",
    paste(fishac$AA_Index, collapse=" "),
    "#- Observed fishery age compositions (proportions at age): oac_fish(1,nyrs_fish_age,1,nages) #!",
    collapse_row(dplyr::select(fishac, -year, -n_s, -n_h, -AA_Index)),
    "#-",
    "",
    "")

  # trawl survey age comp ----

  tsac <- c(sep,
            "# Trawl Survey Age Composition",
            sep,
            "#! Number of years: nyrs_srv1_age",
            as.character(nrow(srvac)),
            "#- Trawl Survey age comp years: yrs_srv1_age #!",
            paste(srvac$year, collapse=" "),
            "#- Number of samples: nsamples_srv1_age(1,nyrs_srv1_age) #!",
            paste(srvac$n_s, collapse=" "),
            "#- Number of hauls: nhauls_srv1_age(1,nyrs_srv1_age) #!",
            paste(srvac$n_h, collapse=" "),
            "#- Index for age-age error matrix #!",
            paste(srvac$AA_Index, collapse=" "),
            "#- Observed trawl survey age compositions (proportions at age): oac_srv1(1,nyrs_srv1_age,1,nages) #!",
            collapse_row(dplyr::select(srvac, -year, -n_s, -n_h, -AA_Index)),
            "#-",
            "",
            "")

  # fishery size comp ----
  fsc <- c(
    sep,
    "# Fishery Size Composition",
    sep,
    "#! Number of years:",
    as.character(nrow(fishsc)),
    "#- Fishery size comp years: #!",
    paste(fishsc$year, collapse=" "),
    "#- Number of samples:  #!",
    paste(fishsc$n_s, collapse=" "),
    "#- Number of hauls:  #!",
    paste(fishsc$n_h, collapse=" "),
    "#- Index for size-age error matrix #!",
    paste(fishsc$SA_Index, collapse=" "),
    "#- Observed fishery size compositions (proportions at age)#!",
    collapse_row(dplyr::select(fishsc, -year, -n_s, -n_h, -SA_Index)),
    "#-",
    "",
    "")

  # trawl survey size comp ----
  tssc <- c(
    sep,
    "# Trawl Survey Size Composition",
    sep,
    "#! Number of years:",
    as.character(nrow(tssc)),
    "#- Survey Years: #!",
    paste(tssc$year, collapse=" "),
    "#- Number of samples:#!",
    paste(tssc$n_s, collapse=" "),
    "#- Number of hauls: #!",
    paste(tssc$n_h, collapse=" "),
    "#- Index for size-age error matrix #!",
    paste(tssc$SA_Index, collapse=" "),
    "#- Observed survey size compositions (proportions at age): oac_fish(1,nyrs_fish_age,1,nages) #!",
    collapse_row(dplyr::select(tssc, -year, -n_s, -n_h, -SA_Index)),
    "#-",
    "",
    "")

  # longline survey size comp ----
  # not used for northern rockfish
  llsc <- c(sep,
            "# Longline Survey Size Composition, NOT USED IN MODEL, include one year of fake data",
            sep,
            "# Number of years: nyrs_srv2_size",
            "1",
            "# Longline Survey size comp years: yrs_srv1_size",
            "1999",
            "# Number of samples: nsamples_srv2_size(1,nyrs_srv2_size)",
            "99",
            "# Number of hauls: nhauls_srv2_size(1,nyrs_srv2_size)",
            "99",
            "# Index for size-age error matrix",
            "1",
            "# Observed longline survey size compositions (proportions at length): osc_srv2(1,nyrs_srv2_size,1,nlenbins)",
            paste(seq(1/nlenbins, 1/nlenbins, length.out=nlenbins), collapse=" "),
            "",
            "")

  # size-age transition matrix ----
  sizeage <- c(sep,
               "# Size-age transition matrix: proportion at size given age: ",
               sep,
               collapse_row(dplyr::select(saa, -age)),
               "#",
               "",
               "")

  # age error matrix ----
  aa <- c(sep,
          "# age error transition matrix: ",
          sep,
          collapse_row(ae),
          "#",
          "",
          "")

  # eof ----
  eof <- c(sep,
           "# end of file marker",
           sep,
           "42",
           "#!")

  # Compile DAT file for ADMB ----

  dat <- c(header,
           mipv,
           fishery_catch,
           cpue,
           trawl_biomass,
           ll_biomass,
           fac,
           tsac,
           fsc,
           tssc,
           llsc,
           sizeage,
           aa,
           eof)


  write.table(dat, file = here::here(year, model, paste0("goa_nr_", year, ".dat")) ,
              quote=FALSE, row.names=FALSE, col.names=FALSE)
}
