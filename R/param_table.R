#' param_table to generate table 15 in the SAFE
#'
#' @param year assessment year
#' @param model folder the model is in
#' @param model_name name of the tpl file
#'
#' @return
#' @export param_table
#'
#' @examples param_table(year = 2020, model = "m18.2b", model_name = "updated_nr")
param_table <- function(year, model, model_name){

  STD = read.delim(here::here(year, model, paste0(model_name, ".std")), sep="", header = TRUE)
  mceval = read.csv(here::here(year, model, "processed", "mceval.csv"))

  params <- function(year, STD, mceval, param){

    # doh - use the same names!!!
    param2 = ifelse(param == "nattymort", "natmort", param)

    std = STD %>%
      dplyr::filter(name == !!param)

    if(param == "spawn_biom_proj") {
      param2 = paste0("spawn_biom_proj_", year + 1)
      std = STD %>%
        dplyr::filter(name == !! param) %>%
        dplyr::slice_head(1)
    }

    mceval %>%
      dplyr::select(param2) %>%
      dplyr::summarise(mean = mean(!!dplyr::sym(param2)),
                       median = median(!!dplyr::sym(param2)),
                       sd = sd(!!dplyr::sym(param2)),
                       lci = quantile(!!dplyr::sym(param2), 0.025),
                       uci = quantile(!!dplyr::sym(param2), 0.975)) %>%
      dplyr::bind_cols(std) %>%
      dplyr::mutate(name := !! param) %>%
      dplyr::select(name, value, mean, median, std.dev, sd, lci, uci)


  }

  dplyr::bind_rows(params(year, STD, mceval, param = "q_srv1") %>%
                     dplyr::mutate_if(is.numeric, format, digits = 2),
                   params(year, STD, mceval, param = "nattymort") %>%
                     dplyr::mutate_if(is.numeric, format, digits = 4),
                   params(year, STD, mceval, param = "F40") %>%
                     dplyr::mutate_if(is.numeric, format, digits = 4),
                   params(year, STD, mceval, param = "spawn_biom_proj") %>%
                     dplyr::mutate_if(is.numeric, format, digits = 0),
                   params(year, STD, mceval, param = "ABC") %>%
                     dplyr::mutate_if(is.numeric, format, digits = 0)) %>%
    write.csv(here::here(year, model, "tables", "tbl_10_15.csv"), row.names = FALSE)
}
