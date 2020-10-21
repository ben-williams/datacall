#' Plot survey biomass
#'
#' @param year model year
#' @param model  folder model is in
#'
#' @return
#' @export plot_survey
#'
#' @examples plot_survey(year, model)
plot_survey <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  read.csv(here::here(year, model, "processed", "survey.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(year = starts_with("y"),
                  Observed = starts_with("bio"),
                  Predicted = pred,
                  se, lci, uci) %>%
    tidyr::pivot_longer(-c(year, se, uci, lci)) %>%
    dplyr::mutate(value = value / 1000,
                  uci = uci / 1000,
                  lci = lci / 1000) -> dat

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, value, color = name, linetype = name)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lci, ymax = uci), color = "gray", width = 0.4) +
    ggplot2::scale_color_grey(name = "",
                              start = 0.8, end = 0.2) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c(0,1)) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, year)$breaks,
                                labels = funcr::tickr(dat, year)$labels) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::xlab("\nYear") +
    ggplot2::ylab("Survey biomass (kt)\n") +
    ggplot2::expand_limits(y = 0) +
    funcr::theme_report() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = 0))) +
    ggplot2::theme(legend.justification=c(1,0),
                   legend.position=c(0.98,0.8))

  ggplot2::ggsave(here::here(year, model, "figs", "srv1_biomass.png"),
         width = 6.5, height = 6.5, units = "in", dpi = 200)
}
