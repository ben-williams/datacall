#' Survey age comp
#'
#' @param year
#' @param recage
#' @param plus_age
#'
#' @return
#' @export survey_age_comp
#'
#' @examples
survey_age_comp <- function(year, recage, plus_age){

  read.csv(here::here(year, "data/raw/srv_age_specimens.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(age)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_s = dplyr::n(),
                     n_h = length(unique(hauljoin))) -> dat1


  read.csv(here::here(year, "data/raw/srv_age_comp.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(year = survey_year) %>%
    dplyr::filter(age >= recage) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tot = sum(agepop),
                  age = ifelse(age < plus_age, age, plus_age)) %>%
    dplyr::group_by(age, year) %>%
    dplyr::summarise(prop = sum(agepop) / mean(tot)) %>%
    dplyr::left_join(dat1) %>%
    dplyr::left_join(expand.grid(year = unique(.$year),
                                 age = recage:plus_age), .) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(AA_Index = 1,
                  n_s = mean(n_s, na.rm = T),
                  n_h = mean(n_h, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = age, values_from = prop) %>%
    dplyr::arrange(year) -> age_comp

  write.csv(age_comp, here::here(year, "data/output/survey_age_comp.csv"))

  age_comp

}
