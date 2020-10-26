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

  # data files
  REP = readLines(here::here(year, "base", paste0(model_name, ".rep")))
  proj = readLines(here::here(year, model, "proj.dat"), warn=FALSE)

  # helper function
  split_item <- function(name, one = NULL){

    if(is.null(one)){
    x = strsplit(proj[grep(name, proj)], " ")[[1]]
    as.numeric(x[2:length(x)])
    } else {
      x = strsplit(proj[grep(name, proj) + 1], " ")[[1]]
      as.numeric(x[2:length(x)])
    }
  }

  # pull data
  age = split_item("#_Natural_Mortality")
  naa = split_item("#_Numbers_at_age_end_year", 1)
  waa = split_item("#_Wt_at_age_spawners", 1)
  saa = split_item("#_Selectivity_fishery_scaled_to_max_at_one", 1)

  m = as.numeric(strsplit(proj[grep("#_Natural_Mortality", proj) + 1], " ")[[1]][[2]])
  ofl = as.numeric(strsplit(REP[grep(paste0("OFL for ", year-1), REP) + 1] [[2]]," "))

  list(best = data.frame(age, naa, waa, saa), m = m, last_ofl = ofl)
}
