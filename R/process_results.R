#' Process model results for tables and figs
#'
#' @param year  assessment year
#' @param model   model being evaluated (folder name)
#' @param model_name   name of the model e.g., updated_nr
#' @param dat_name name of dat file e.g., goa_nr_2020
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param mcmc number of mcmcs run
#' @param mcsave the number of mcmcs saved
#' @param survey file name (stored in the "user_input" folder) if NULL it pulls the design-based estimate from AKFIN
#' @param ... future functions
#'
#' @return
#' @export process_results
#'
#' @examples process_results (year = 2020, model = m18.2, model_name = "goa_nr", dat_name = "goa_nr_2020", rec_age = 2, plus_age = 45, mcmc = 1e+07, mcsave = 2000, survey = "VAST_GAP.csv")
#'
process_results <- function(year, model, model_name, dat_name,
                            rec_age, plus_age, mcmc, mcsave, survey = NULL,...){

  # setup
  if (!dir.exists(here::here(year, model, "processed"))){
    dir.create(here::here(year, model, "processed"), recursive=TRUE)
  }

  if (!dir.exists(here::here(year, model, "figs"))){
    dir.create(here::here(year, model, "figs"), recursive=TRUE)
  }

  if (!dir.exists(here::here(year, model, "tables"))){
    dir.create(here::here(year, model, "tables"), recursive=TRUE)
  }

  # helper functions
  rep_item <- function(name){
    t <- strsplit(REP[grep(name, REP)]," ")
    t <- subset(t[[1]], t[[1]]!="")
    if(t[[1]][1] == "TWL"){
      as.numeric(t[3:length(t)])
    } else {
      as.numeric(t[2:length(t)])
    }
  }


  # read in rep and ctl files
  REP <- readLines(here::here(year, model, paste0(model_name, ".rep")))
  CTL <- readLines(here::here(year, model, paste0(dat_name, ".ctl")))
  PSV <- file(here::here(year, model, paste0(model_name, ".psv")), "rb")
  STD <- read.delim(here::here(year, model, paste0(model_name, ".std")), sep="", header = TRUE)
  mceval <- read.delim(here::here(year, model, "evalout.prj"), sep="", header=FALSE)

  # clean rep file
  suppressWarnings(data.frame(year = unlist(strsplit(REP[grep("Year", REP)[1]]," "))) %>%
                     dplyr::mutate(year = as.numeric(year)) %>%
                     tidyr::drop_na() %>%
                     dplyr::pull(year)) -> yrs

  suppressWarnings(data.frame(age = unlist(strsplit(REP[grep("Age", REP)[1]]," "))) %>%
                     dplyr::mutate(age = as.numeric(age)) %>%
                     tidyr::drop_na() %>%
                     dplyr::pull(age)) -> ages

  styr_rec <- yrs[1] - length(ages) + 2

  suppressWarnings(as.data.frame(cbind(yrs = yrs, ages = ages, styr_rec = styr_rec)) %>%
                     dplyr::mutate(ages = replace(ages, duplicated(ages), NA),
                                   styr_rec = replace(styr_rec, duplicated(styr_rec), NA))) %>%
    write.csv(here::here(year, model, "processed", "ages_yrs.csv"), row.names = FALSE)

  # MCMC parameters ----

  npar = readBin(PSV, what = integer(), n=1)
  mcmcs = readBin(PSV, what = numeric(), n = (npar * mcmc / mcsave))
  close(PSV)
  mcmc_params = matrix(mcmcs, byrow=TRUE, ncol=npar)
  mcmc_params = mcmc_params[501:nrow(mcmc_params),]
  colnames(mcmc_params) = STD$name[1:ncol(mcmc_params)]
  write.csv(mcmc_params, here::here(year, model, "processed", "mcmc.csv"), row.names = FALSE)

  # mceval phase output ----

  #Curry's Change
  mceval = mceval[501:nrow(mceval),]

  #Length colnames = 286
  # columns mcmc_other = 271

  #1-8: Through objective function value

  colnames(mceval) = c("sigr", "q_srv1", "q_srv2", "F40", "natmort", "spawn_biom_proj",
                       "ABC", "obj_fun",
                       paste0("tot_biom_", yrs),
                       paste0("log_rec_dev_", seq(styr_rec, yrs[length(yrs)])),
                       paste0("spawn_biom_", yrs),
                       "log_mean_rec",
                       paste0("spawn_biom_proj_", max(yrs) + 1:15),
                       paste0("pred_catch_proj_", max(yrs) + 1:15),
                       paste0("rec_proj_", max(yrs) + 1:10),
                       paste0("tot_biom_proj_", max(yrs) + 1:15))
  write.csv(mceval, here::here(year, model, "processed", "mceval.csv"), row.names = FALSE)

  # catch data ----

  pred = strsplit(REP[grep("Pred_Catch", REP)], " ")
  r1 = which(pred[[1]] == "Pred_Catch")
  r2 = which(pred[[1]] == "Pred_catch_later")
  r3 = which(pred[[1]] == "")
  pred = as.numeric(pred[[1]][-c(r1, r2, r3)])

  obs = strsplit(REP[grep("Obs_Catch", REP)], " ")
  r1 = which(obs[[1]] == "Obs_Catch")
  r2 = which(obs[[1]] == "Obs_Catch_Later")
  r3 = which(obs[[1]] == "")
  obs = as.numeric(obs[[1]][-c(r1, r2, r3)])

  data.frame(obs = obs, pred = pred) %>%
    write.csv(here::here(year, model, "processed/catch.csv"))



  # survey data ----
  if(is.null(survey)){
    dat = read.csv(here::here(year, "data", "output", "survey_biomass.csv")) %>%
      dplyr::rename_all(tolower)
  } else {
    dat = read.csv(here::here(year, "data", "user_input", survey)) %>%
      dplyr::rename_all(tolower)
  }


  pred = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][3]
  pred = strsplit(pred," ")
  pred = subset(pred[[1]], pred[[1]]!="")
  pred = as.numeric(pred[2:length(pred)])

  dat %>%
    dplyr::bind_cols(pred = pred) %>%
    write.csv(here::here(year, model, "processed", "survey.csv"), row.names = FALSE)


  # recruitment ----

  N = REP[grep("Numbers", REP):(grep("Obs_P_fish_age", REP)-2)]
  t = NA
  for(i in 1:length(yrs)){
    ts = as.numeric(strsplit(N[i+1]," ")[[1]][3])
    t = c(t, ts)}
  pred_rec = t[!is.na(t)]

  # biomass & F & recruits ----
  data.frame(year = yrs,
             tot_biom = rep_item("Tot_biom"),
             sp_biom = rep_item("SpBiom"),
             F = rep_item("Fully_selected_F"),
             recruits = pred_rec) %>%
    write.csv(here::here(year, model, "processed", "bio_rec_f.csv"), row.names = FALSE)


  # selectivity ----
  data.frame(age = ages,
             fish = rep_item("Fishery_Selectivity"),
             srv1 = rep_item("TWL Survey_Selectivity")) %>%
    write.csv(here::here(year, model, "processed", "selex.csv"), row.names = FALSE)

  # yield ratio B40 & B35----

  data.frame(B40 = STD$value[which(STD$name=="B40")],
    B35 = as.numeric(REP[(grep("B_35",REP)+1):(grep("F_40",REP)[1]-1)]),
    yld_rat = as.numeric(unlist(strsplit(CTL[grep("yieldratio", CTL)], "\t"))[1])) %>%
    write.csv(here::here(year, model, "processed", "b35_b40_yld.csv"), row.names = FALSE)

  # size comps ----

  #! this will need a switch for multiple surveys

  obs = REP[grep("Obs_P_fish_age",REP):(grep("Pred_P_fish_age",REP)-2)]
  pred = REP[grep("Pred_P_fish_age",REP):(grep("Obs_P_fish_size",REP)-2)]

  obs_l = REP[grep("Obs_P_fish_size",REP):(grep("Pred_P_fish_size",REP)-2)]
  pred_l = REP[grep("Pred_P_fish_size",REP):(grep("Obs_P_srv1_age",REP)-2)]

  s_obs = REP[grep("Obs_P_srv1_age",REP):(grep("Pred_P_srv1_age",REP)-2)]
  s_pred = REP[grep("Pred_P_srv1_age",REP):(grep("Obs_P_srv1_size",REP)-2)]

  s_obs_l = REP[grep("Obs_P_srv1_size",REP):(grep("Pred_P_srv1_size",REP)-2)]

  purrit(obs, pred, rec_age, plus_age, comp = "age") %>%
    write.csv(here::here(year, model, "processed", "fac.csv"))

  purrit(obs_l, pred_l, rec_age, plus_age, comp = "length") %>%
    write.csv(here::here(year, model, "processed", "fsc.csv"))

  purrit(s_obs, s_pred, rec_age, plus_age, comp = "age") %>%
    write.csv(here::here(year, model, "processed", "sac.csv"))

  purrit(s_obs_l, pred = NULL, rec_age, plus_age, comp = "length") %>%
    write.csv(here::here(year, model, "processed", "ssc.csv"))


}
