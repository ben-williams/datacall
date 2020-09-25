
#' Swath plot
#'
#' @param year
#' @param model   the folder with the model in it
#'
#' @return
#' @export plot_swath
#'
#' @examples
plot_swath <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  # set view
  ggplot2::theme_set(funcr::theme_report())

  # read in data
  read.csv(here::here(year, model, "processed/ages_yrs.csv"))$yrs -> yrs
  read.csv(here::here(year, model, "processed/b35_b40_yld.csv")) -> bby

  read.csv(here::here(year, model, "processed/mceval.csv")) %>%
    dplyr::select(paste0("spawn_biom_", yrs),
                  paste0("spawn_biom_proj_", (max(yrs)+1):(max(yrs+15)))) %>%
    dplyr::mutate(group = 1:dplyr::n()) %>%
    tidyr::pivot_longer(c(-group), values_to = "biomass") %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(name, "[[:digit:]]+"))) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(lci = quantile(biomass, 0.025) / 1000,
                     lci2 = quantile(biomass, 0.1) / 1000,
                     median = quantile(biomass, 0.5) / 1000,
                     uci = quantile(biomass, 0.8) / 1000,
                     uci2 = quantile(biomass, 0.975) / 1000) -> dat

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, median)) +
    ggplot2::geom_ribbon(data = dplyr::filter(dat, year<=2020), ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.3) +
    ggplot2::geom_ribbon(data = dplyr::filter(dat, year>=2020), ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.2) +
    ggplot2::geom_line(data = dplyr::filter(dat, year<=2020), ggplot2::aes(year, median)) +
    ggplot2::geom_line(data = dplyr::filter(dat, year>=2020), ggplot2::aes(year, median), lty = 2) +
    ggplot2::geom_hline(yintercept = c(bby$B40/1000, bby$B35/1000),
                        lty = c(3, 1), color = c(2, 1)) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, year, 10, start = 1960)$breaks,
                                labels = funcr::tickr(dat, year, 10, start = 1960)$labels) +
    ggplot2::ylab("Spawning biomass (kt)\n") +
    ggplot2::xlab("\nYear") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(here::here(year, model, "figs/swath.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
