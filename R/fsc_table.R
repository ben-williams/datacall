#' fishery size comp table
#'
#' @param year assessment year
#' @param model folder the model is in
#'
#' @return
#' @export fsc_table
#'
#' @examples fsc_table(year, model)
fsc_table <- function(year, model){

  option(scipen = 999)
  fsc = read.csv(here::here(year, "data", "output", "fish_size_comp.csv"))

  fsc %>%
    dplyr::select(n_s, n_h) %>%
    t(.) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name") -> samps

  fsc %>%
    dplyr::select(-n_s, -n_h, -SA_Index) %>%
    tidyr::pivot_longer(-c(year)) %>%
    tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, digits = 4) %>%
    dplyr::mutate(name = gsub("X", "", name),
                  name = ifelse(dplyr::row_number() == dplyr::n(), paste0(name, "+"), name )) %>%
    dplyr::rename_all(~stringr::str_replace(., "y", "")) -> comp

  names(samps) <- names(comp)

  dplyr::bind_rows(comp, samps) %>%
    write.csv(here::here(year, model, "tables", "tbl_10_06.csv"), row.names = FALSE)

}
