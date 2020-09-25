#' clean up all the raw data
#'
#' @param year model year
#' @param species e.g., "NORK"
#' @param TAC past three years TAC c(year-3, year-2, year-1)
#' @param admb_home location of admb on your computer e.g., "C:/Program Files (x86)/ADMB-12.1"
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param reader_tester if there is an alternate reader_tester (age error) file
#'
#' @return
#' @export clean_raw
#'
#' @examples
clean_raw <- function(year, species, TAC, admb_home, rec_age, plus_age, reader_tester = NULL){

  # catch and biomass data ----
  clean_catch(year, TAC)
  # note: must provide file for VAST estimates
  survey_biomass(year)

  # biological data ----
  allometric(year, admb_home, recage, plus_age)
  fish_age_comp(year, recage, plus_age)
  survey_age_comp(year, recage, plus_age)
  fish_size_comp(year, recage)
  survey_size_comp(year)
  ageage(reader_tester = NULL, species, year, admb_home)
  weight_length(year, admb_home, rec_age, plus_age)
}
