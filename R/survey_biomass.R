#' Get the survey biomass
#'
#' @param year of interest
#' @param type: if not using the design-based abundance, the file name must be stated (e.g. "GAP_VAST.csv")
#'
#' @return
#' @export survey_biomass
#'
#' @examples
#'
survey_biomass <- function(year, file = NULL){

  if(is.null(file)){

    read.csv(here::here(year, "data", "raw", "srv_biomass.csv")) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(biomass = total_biomass,
                       se = sqrt(biomass_var),
                       lci = biomass - 1.96 * se,
                       uci = biomass + 1.96 * se) %>%
      dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
      dplyr::mutate_if(is.double, round) -> sb
  } else {
      read.csv(here::here(year, "data", "user_input", file)) -> sb
    }

  write.csv(sb, here::here(year, "data", "output", "survey_biomass.csv"), row.names = FALSE)

  sb
}
