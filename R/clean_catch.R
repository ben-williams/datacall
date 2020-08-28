#' clean up catch data
#'
#' @param old_catch - if catch is frozen place the file in user_input folder
#'
#' @return
#' @export clean_catch
#'
#' @examples
clean_catch <- function(year, TAC, old_catch = NULL){

  if(!is.null(old_catch)){
    old_catch = read.csv(here::here(year, "data/user_input", old_catch))
  }

  # Fishery catch data ----
  read.csv(here::here(year, "data/raw/fishery_catch_data.csv")) -> catch_data
  read.csv(here::here(year, "data/raw/fishery_obs_data.csv")) -> obs_data

    # Estimate catch ratio in final year to end of year
  obs_data %>%
    dplyr::filter(YEAR %in% (year-3):(year-1)) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(tot_catch = sum(EXTRAPOLATED_WEIGHT),
           test_date = paste0(YEAR, substr(max(as.Date(catch_data$WEEK_END_DATE)),5,10))) %>%
    dplyr::filter(HAUL_DATE <= test_date) %>%
    dplyr::summarise(oct_catch = sum(EXTRAPOLATED_WEIGHT),
              tot_catch = mean(tot_catch)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(ratio = 1 + (sum(tot_catch) - sum(oct_catch)) / sum(oct_catch)) %>%
    dplyr::pull(ratio) -> ratio

  # Compute catch
  catch_data %>%
    dplyr::select(Year = YEAR, catch = WEIGHT_POSTED) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(Catch = sum(catch)) %>%
    dplyr::bind_rows(old_catch) %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(Catch = ifelse(Year==year, Catch * ratio, Catch)) -> catch
    write.csv(catch, here::here(year, "data/output/catch.csv"))

  # estimate yield ratio of previous 3 years relative to TAC
  catch %>%
    dplyr::filter(Year %in% (year-3):(year-1)) %>%
    dplyr::bind_cols(tac = TAC) %>%
    dplyr::mutate(yld = Catch / tac) %>%
    dplyr::summarise(yld = mean(yld)) %>%
    write.csv(here::here(year, "data/output/yld_rat.csv"))
}
