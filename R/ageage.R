#' age error model
#'
#' @param read_tester_loc_file
#' @param species
#' @param admb_home
#' @param recage
#' @param plus_age
#' @param max_age
#' @param ...
#'
#' @return
#' @export ageage
#'
#' @examples
ageage <- function(read_tester_loc_file, species, admb_home = NULL, recage = 2, plus_age = 45, max_age = 100, ...){

  if(species == "NORK"){
    norpac_species = 303
    region = "GOA"
    nages = length(recage:plus_age) + 1
  }

  if (!dir.exists("output")){
    dir.create("output", recursive=TRUE)
  }

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  read.csv(here::here(read_tester_loc_file)) %>%
    filter(Species == norpac_species,
           Region == region,
           Read_Age > 0,
           Test_Age > 0,
           Final_Age > 0) %>%
    group_by(Test_Age, Read_Age) %>%
    summarise(freq = n()) -> dat


  left_join(expand.grid(Test_Age = unique(dat$Test_Age),
                        Read_Age = unique(dat$Read_Age)),
            dat) %>%
    replace_na(list(freq = 0)) %>%
    group_by(Test_Age) %>%
    mutate(num = case_when(Test_Age == Read_Age ~ freq)) %>%
    summarise(num = mean(num, na.rm = TRUE),
              den = sum(freq)) %>%
    mutate(ape = 1 - (1 - num / den),
           ape = ifelse(is.nan(ape), 0, ape)) %>%
    dplyr::select(age = Test_Age, ape, ss = den) %>%
    left_join(data.frame(age = min(dat$Test_Age):max(dat$Test_Age)), .) %>%
    replace_na(list(ape = -9, ss = -9)) -> dats

  c("# Number of obs", nrow(dats),
    "# Age vector", dats$age,
    "# Percent agreement vector", dats$ape,
    "# Sample size vector", dats$ss) %>%
    write.table(here::here("data/models/ageage/AGEAGE.DAT"), sep="", quote=F, row.names=F, col.names=F)

  setwd(here::here("data/models/ageage"))
  R2admb::compile_admb("AGEAGE", verbose = TRUE)
  R2admb::run_admb("AGEAGE", verbose=TRUE)

  setwd(here::here())
  read.delim("data/models/ageage/AGEAGE.STD", sep="") %>%
    filter(grepl("_a", name)) %>%
    bind_cols(dats) %>%
    dplyr::select(age, value) -> sds

  fit = lm(value ~ age, data = sds)

  # fit out to age 100 (aka: max_age)
  data.frame(age = recage:max_age) %>%
    mutate(ages_sd = predict(fit, .)) -> fits

  ages = fits$age
  ages_sd = cbind(ages, sds$value[3:(length(ages) + 2)])

  mtx100 = matrix(nrow = nrow(fits), ncol = nrow(fits))
  colnames(mtx100) = rownames(mtx100) = ages

  for(j in 1:length(ages)){
    mtx100[j,1] <- pnorm(ages[1] + 0.5, ages[j], ages_sd[which(ages_sd[,1] == ages[j]), 2])}


  for(i in 2:(length(ages) - 1)){
    for(j in 1:length(ages)){
      mtx100[j,i] <- pnorm(ages[i] + 0.5, ages[j], ages_sd[which(ages_sd[,1] == ages[j]), 2]) -
        pnorm(ages[i-1] + 0.5, ages[j], ages_sd[which(ages_sd[,1] == ages[j]), 2])
    }
  }

  for(j in 1:length(ages)){
    mtx100[j,length(ages)] <- 1 - sum(mtx100[j, 1:(length(ages) - 1)])
  }

  write.csv(mtx100, paste0("output/ae_mtx_", max_age, ".csv"))
  write.csv(sds,  here::here("output/ae_SD.csv"))

  # Compute ageing error matrix for model
  ae_Mdl <- matrix(nrow=length(ages), ncol=nages)
  ae_Mdl[, 1:(nages-1)] <- as.matrix(mtx100[, 1:(nages-1)])
  ae_Mdl[, nages] <- rowSums(mtx100[, nages:length(ages)])
  ae_Mdl <- round(ae_Mdl, digits=4)
  r <- which(ae_Mdl[, nages]>=0.999)[1]
  ae_Mdl <- ae_Mdl[1:r,]

  write.csv(ae_Mdl,  here::here("output/ae_model.csv"))
  ae_Mdl

}
