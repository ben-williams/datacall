#' recruit table to generate table 14 in the SAFE
#'
#' @param year model year
#' @param model folder the model is in
#' @param model_name name of tpl file
#' @param rec_age recruitment age
#'
#' @return
#' @export recruit_table
#'
#' @examples recruit_table(year, model, model_name, rec_age)
recruit_table <- function(year, model, model_name, rec_age){

  # hard to filter year with year so change the name
  mod_year = year

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating tables")
  }

  # read in data
  REP <- readLines(here::here(year, model, paste0(model_name, ".rep")))
  STD <- read.delim(here::here(year, model, paste0(model_name, ".std")), sep="", header = TRUE)

  suppressWarnings(data.frame(year = unlist(strsplit(REP[grep("Year", REP)[1]]," "))) %>%
                     dplyr::mutate(year = as.numeric(year)) %>%
                     tidyr::drop_na() %>%
                     dplyr::pull(year)) -> yrs

  bio_rec = read.csv(here::here(year, model, "processed", "bio_rec_f.csv")) %>%
    dplyr::select(-F) %>%
    dplyr::mutate(recruits = round(recruits * 1000))

  bio_rec %>%
    dplyr::filter(year %in% (1977 + rec_age):(mod_year - rec_age)) %>%
    dplyr::summarise(recruits = round(mean(recruits))) -> pred_rec

  data.frame(year = mod_year + 1:2,
             tot_biom = STD$value[which(STD$name=="tot_biom_proj")][1:2],
             sp_biom = STD$value[which(STD$name=="spawn_biom_proj")][1:2],
             recruits = pred_rec$recruits) -> std_data

  values = dplyr::bind_rows(bio_rec, std_data) %>%
    dplyr::mutate_all(dplyr::funs(round(.)))



  # get mcmc data - clean it, calculate annual uci and lci
  read.csv(here::here(year, model, "processed", "mceval.csv")) %>%
    dplyr::select(dplyr::starts_with(c( "tot_biom", "spawn_biom", "log_rec_dev", "rec_proj"))) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::mutate_if(is.character, dplyr::funs(as.numeric(gsub(",", "", .)))) %>%
    tidyr::pivot_longer(-id) %>%
    dplyr::mutate(value = ifelse(grepl("log", name), exp(value), value),
                  year =  as.numeric(stringr::str_extract(name, "[[:digit:]]+")),
                  name = dplyr::case_when(stringr::str_detect(name, "tot_biom") ~ "tot_biom",
                                          stringr::str_detect(name, "spawn_biom") ~ "sp_biom",
                                          stringr::str_detect(name, "log_rec") ~ "recruits",
                                          stringr::str_detect(name, "rec_proj") ~ "recruits")) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    dplyr::filter(year >= yrs[1] & year <= mod_year + rec_age) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(tot_lci = round(quantile(tot_biom, 0.025)),
                     tot_uci = round(quantile(tot_biom, 0.975)),
                     sp_lci = round(quantile(sp_biom, 0.025)),
                     sp_uci = round(quantile(sp_biom, 0.975)),
                     rec_lci = round(quantile(recruits, 0.025) * 1000),
                     rec_uci = round(quantile(recruits, 0.975) * 1000))  %>%
    dplyr::left_join(values, .) %>%
    dplyr::select(year,
                  recruits, rec_lci, rec_uci,
                  tot_biom, tot_lci, tot_uci,
                  sp_biom, sp_lci, sp_uci) %>%
  write.csv(here::here(year, model, "tables", "tbl_10_14.csv"), row.names = FALSE)

}
