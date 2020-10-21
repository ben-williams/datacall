#' Plot MCMC estimated biomass
#'
#' @param year model year
#' @param model folder the model is in
#'
#' @return
#' @export plot_biomass
#'
#' @examples
plot_biomass <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  yrs = read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs
  bio = read.csv(here::here(year, model, "processed", "bio_rec_f.csv"))

  read.csv(here::here(year, model, "processed", "mceval.csv"))  %>%
    dplyr::select(paste0("tot_biom_", yrs)) %>%
    dplyr::mutate(group = 1:dplyr::n()) %>%
    tidyr::pivot_longer(-group) %>%
    dplyr::mutate(year = as.numeric(gsub("tot_biom_", "", name)),
                  name = "Total biomass") %>%
    dplyr::bind_rows( read.csv(here::here(year, model, "processed", "mceval.csv")) %>%
                        dplyr::select(paste0("spawn_biom_", yrs)) %>%
                        dplyr::mutate(group = 1) %>%
                        tidyr::pivot_longer(-group) %>%
                        dplyr::mutate(year = as.numeric(gsub("spawn_biom_", "", name)),
                                      name = "Spawning biomass")) %>%
    dplyr::mutate(name = factor(name, levels = c("Total biomass", "Spawning biomass"))) %>%
    dplyr::group_by(year, name) %>%
    dplyr::summarise(median = median(value) / 1000,
                     lci = quantile(value, 0.025) / 1000,
                     uci = quantile(value, 0.975) / 1000) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(data.frame(year = yrs,
                                tot = bio$tot_biom / 1000,
                                bio = bio$sp_biom / 1000)) %>%
    dplyr::mutate(biomass = ifelse(name == "Total biomass", tot, bio)) %>%
    dplyr::select(-tot, -bio) -> dat

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, biomass)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.1) +
    ggplot2::facet_wrap(~name, dir = "v", scales = "free_y") +
    ggplot2::scale_y_continuous(name = "Biomass (kt)\n", labels = scales::comma) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(name = "\nYear",
                                breaks = funcr::tickr(dat, year, 10, start = 1960)$breaks,
                                labels = funcr::tickr(dat, year, 10, start = 1960)$labels) +
    funcr::theme_report()

  ggplot2::ggsave(here::here(year, model, "figs", "est_biomass.png"), width = 6.5, height = 6.5, units = "in", dpi = 200)
}
