#' project data for "Best F"
#'
#' @param year assessment year
#' @param model folder the model is in
#' @param model_name name of the .tpl file (no extension)
#'
#' @return
#' @export proj_data
#'
#' @examples
proj_data <- function(year, model, model_name){

  REP = readLines(here::here(year, "base", paste0(model_name, ".rep")))

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
  ofl = as.numeric(strsplit(REP[grep(paste0("OFL for ", year-1), REP) + 1] [[2]]," "))

  list(best = data.frame(age, naa, waa, saa), m = m, last_ofl = ofl)
}
