#' Survey size composition
#'
#' @param year  = the year of assessment
#' @param lenbins = length bin file
#'
#' @return
#' @export survey_size_comp
#'
#' @examples
survey_size_comp <- function(year, lenbins = NULL){

  if(is.null(lenbins)){
    lenbins = read.csv(here::here(year, "data", "user_input", "len_bin_labels.csv"))$len_bins
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }

  read.csv(here::here(year, "data", "raw", "srv_size_freq.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(length)) %>%
    dplyr::mutate(length = length / 10) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_s = sum(frequency),
                     n_h = length(unique(hauljoin))) %>%
    dplyr::ungroup() -> dat


  read.csv(here::here(year, "data", "raw", "srv_size_comp.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(length = length / 10,
                  length = ifelse(length >= max(lenbins), max(lenbins), length)) %>%
    dplyr::filter(length %in% lenbins) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tot = sum(total)) %>%
    dplyr::group_by(year, length) %>%
    dplyr::summarise(prop = sum(total) / mean(tot)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(expand.grid(year = unique(.$year), length = lenbins), .) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::left_join(dat) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(SA_Index = 1,
                  n_s = mean(n_s, na.rm = T),
                  n_h = mean(n_h, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = length, values_from = prop) -> size_comp

  readr::write_csv(size_comp, here::here(year, "data", "output", "survey_size_comp.csv"))

  size_comp
}
