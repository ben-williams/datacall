
#' Swath plot
#'
#' @param year assessment year
#' @param model   the folder with the model in it
#'
#' @return
#' @export plot_swath
#'
#' @examples plot_swath(year, model)
plot_swath <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  # set view
  ggplot2::theme_set(funcr::theme_report())

  # establish quantiles
  q_name <- purrr::map_chr(seq(.025,.975,.05), ~ paste0("q", .x*100))
  q_fun <- purrr::map(seq(.025,.975,.05), ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
    purrr::set_names(nm = q_name)

  # read in data calculate quantiles/median and plot
  read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs -> yrs
  read.csv(here::here(year, model, "processed", "b35_b40_yld.csv")) -> bby

  read.csv(here::here(year, model, "processed", "mceval.csv")) %>%
    dplyr::select(paste0("spawn_biom_", yrs),
                  paste0("spawn_biom_proj_", (max(yrs)+1):(max(yrs+15)))) %>%
    dplyr::mutate(group = 1:dplyr::n()) %>%
    tidyr::pivot_longer(c(-group), values_to = "biomass") %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(name, "[[:digit:]]+")),
                  biomass = biomass / 1000) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise_at(dplyr::vars(biomass), tibble::lst(!!!q_fun, median)) %>%
    tidyr::pivot_longer(-c(year, median)) %>%
    dplyr::mutate(grouping = dplyr::case_when(name == q_name[1] | name == q_name[20] ~ 1,
                                              name == q_name[2] | name == q_name[19] ~ 2,
                                              name == q_name[3] | name == q_name[18] ~ 3,
                                              name == q_name[4] | name == q_name[17] ~ 4,
                                              name == q_name[5] | name == q_name[16] ~ 5,
                                              name == q_name[6] | name == q_name[15] ~ 6,
                                              name == q_name[7] | name == q_name[14] ~ 7,
                                              name == q_name[8] | name == q_name[13] ~ 8,
                                              name == q_name[9] | name == q_name[12] ~ 9,
                                              name == q_name[10] | name == q_name[11] ~ 10)) %>%
    dplyr::group_by(year, grouping) %>%
    dplyr::mutate(min = min(value),
           max = max(value)) %>%
    dplyr::ungroup() -> dat

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, group = grouping)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max), alpha = 0.07) +
    ggplot2::geom_line(ggplot2::aes(y = median)) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, year, 10, start = 1960)$breaks,
                                labels = funcr::tickr(dat, year, 10, start = 1960)$labels) +
    ggplot2::ylab("Spawning biomass (kt)\n") +
    ggplot2::xlab("\nYear") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_hline(yintercept = c(bby$B40/1000, bby$B35/1000),
                        lty = c(3, 1)) +
    scico::scale_fill_scico()

  ggplot2::ggsave(here::here(year, model, "figs", "swath.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
