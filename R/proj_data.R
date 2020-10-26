#' project data for "Best F"
#'
#' @param year assessment year
#' @param model folder the model is in
#'
#' @return
#' @export proj_data
#'
#' @examples
proj_data <- function(year, model){

  proj = readLines(here::here(year, model, "proj.dat"), warn=FALSE)

  age = strsplit(proj[grep("#_Natural_Mortality", proj)], " ")[[1]]
  age = as.numeric(age[2:length(age)])

  naa = strsplit(proj[grep("#_Numbers_at_age_end_year", proj) + 1], " ")[[1]]
  naa = as.numeric(naa[2:length(naa)])

  waa = strsplit(proj[grep("#_Wt_at_age_spawners", proj) + 1], " ")[[1]]
  waa = as.numeric(waa[2:length(waa)])

  saa = strsplit(proj[grep("#_Selectivity_fishery_scaled_to_max_at_one", proj) + 1], " ")[[1]]
  saa = as.numeric(saa[2:length(saa)])
  m = as.numeric(strsplit(proj[grep("#_Natural_Mortality", proj) + 1], " ")[[1]][[2]])

  list(best = data.frame(age, naa, waa, saa), m = m)
}
