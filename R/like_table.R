#' Likelihood table to generate table 11 in the SAFE
#'
#' @param year model assessment year
#' @param model the folder the model is in e.g., m18.2
#' @param model_name name of the tpl file (don't include the .tpl extension)
#'
#' @return
#' @export like_table
#'
#' @examples like_table(year = 2020, model = "m18.2", model_name = "updated_nr")
like_table <- function(year, model, model_name){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating tables")
  }

  REP <- readLines(here::here(year, model, paste0(model_name, ".rep")))

  suppressWarnings(data.frame(year = unlist(strsplit(REP[grep("Year", REP)[1]]," "))) %>%
                     dplyr::mutate(year = as.numeric(year)) %>%
                     tidyr::drop_na() %>%
                     dplyr::pull(year)) -> yrs

  # helper functions ----

  rep_item <- function(name, type = NULL){

    if(is.null(type)){
      as.numeric(strsplit(REP[grep(name, REP)], " ")[[1]][2])
    } else if(type == 1){
      as.numeric(strsplit(REP[grep(name, REP) + 1], " ")[[1]][1])
    } else if(type == 2){
      as.numeric(strsplit(REP[grep(name, REP)[1]+1], " ")[[1]][2])
    } else if(type == 3){
      as.numeric(strsplit(REP[grep(paste(name, max(yrs)+1), REP)[1]+1], " ")[[1]][1])
    } else if(type == 4){
      as.numeric(strsplit(REP[grep(paste(name, max(yrs)+1), REP)[2]+1], " ")[[1]][1])
    }

  }


  # table 10-11 part a
  data.frame(catch = rep_item('SSQ_Catch_Likelihood'),
             survey_bio = rep_item('TWL Survey_Abundance_Index_Likelihood'),
             fish_age  = rep_item('Fishery_Age_Composition_Likelihood'),
             survey_age = rep_item('Survey_Age_Composition_Likelihood'),
             fish_size = rep_item('Fishery_Size_Composition_Likelihood'),
             maturity = rep_item('L mat like'),
             data = rep_item('data likelihood'),
             rec_devs = rep_item('Recruitment_Deviations_Likelihood'),
             f_mort = rep_item('Fishing_Mortality_Regularity_Penalty'),
             survey_q = rep_item('priors q TWL survey'),
             prior_m = rep_item('priors M'),
             obj_fun = rep_item('obj_fun')) %>%
    dplyr::mutate(maturity = obj_fun - rowSums(.[1:5]) - rowSums(.[8:11]),
                  data = rowSums(.[1:5]) + maturity,
                  id = 1:dplyr::n()) %>%
    tidyr::pivot_longer(-id, names_to = "item", values_to = model) %>%
    dplyr::select(-id) -> part_a


  # table 10-11 part b

  data.frame(item = c('params', 'q', 'mort', 'sigma_r', 'log_mean_rec', 'exp_log_mean_rec',
                      'mean_recruit','f40', 'tot_bio', 'ssb',
                      'b0','b40', 'abc', 'f35', 'ofl'),
             value = c(rep_item("Number parameters estimated", type = 1),
                       rep_item("q_trawl", type = 1),
                       rep_item("nat_mort", type = 1),
                       rep_item("sigr", type = 1),
                       rep_item("log_mean_rec", type = 1),
                       exp(rep_item("log_mean_rec", type = 1)),
                       rep_item("N_at_age projected", type = 2),
                       rep_item("F_40", type = 1),
                       rep_item("TotalBiomass for", type = 3),
                       rep_item("Female_Spawning Biomass for", type = 3),
                       rep_item("B_zero", type = 1),
                       rep_item("B_40", type = 1),
                       rep_item("ABC for", type = 4),
                       rep_item("F_35", type = 1),
                       rep_item("OFL for", type = 4))) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    dplyr::mutate(!!model := value) %>%
    dplyr::select(-value) -> part_b

  dplyr::bind_rows(part_a, part_b) %>%
    write.csv(here::here(year, model, "tables", "tbl_10_11.csv"), row.names = FALSE)


}
