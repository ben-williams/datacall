#' Plot selectivity at age
#'
#' @param year model year
#' @param model folder model is in
#'
#' @return
#' @export plot_selex
#'
#' @examples plot_selex(year, model)
plot_selex <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }


  read.csv(here::here(year, model, "processed", "selex.csv")) %>%
    dplyr::select(age, Fishery = fish, Survey = srv1) -> dat

  dat %>%
    tidyr::pivot_longer(-age) %>%
    ggplot2::ggplot(ggplot2::aes(age, value, color = name)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_grey(name = "")+
    ggplot2::ylab("\nSelectivity") +
    ggplot2::scale_x_continuous(name = "\nYear",
                                breaks = funcr::tickr(dat, age, 10, start = 0)$breaks,
                                labels = funcr::tickr(dat, age, 10, start = 0)$labels) +
    funcr::theme_report() +
    ggplot2::theme(legend.justification=c(1,0), legend.position=c(0.9,0.2))

  ggplot2::ggsave(here::here(year, model, "figs", "selex.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
