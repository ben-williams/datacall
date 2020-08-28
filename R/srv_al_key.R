#' AGe0length key for survey data
#'
#' @return
#' @export srv_al_key
#'
#' @examples
srv_al_key <- function(year){

  read.csv(here::here(year, "data/raw/srv_saa_age.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(year, age, length) %>%
    dplyr::filter(year>=1990, !is.na(age))  %>%
    dplyr::select(-year) %>%
    dplyr::group_by(age) %>%
    dplyr::filter(dplyr::n()>1) %>%
    dplyr::group_by(length) %>%
    dplyr::mutate(n_l = dplyr::n()) %>%
    dplyr::arrange(age, length) %>%
    dplyr::group_by(age) %>%
    dplyr::mutate(sample_size =  dplyr::n()) -> inter

  read.csv(here::here(year, "data/raw/srv_saa_length.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(year>1990, !is.na(length)) %>%
    dplyr::select(frequency, length) %>%
    dplyr::group_by(length) %>%
    dplyr::summarise(tot = sum(frequency)) %>%
    dplyr::left_join(inter, .) %>%
    dplyr::group_by(age, length) %>%
    dplyr::mutate(prop =  dplyr::n() / n_l * tot )

}
