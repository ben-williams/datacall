#' Plot catch
#'
#' @param year model year
#' @param model folder name model is in
#'
#' @return
#' @export plot_catch
#'
#' @examples
plot_catch <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs -> yrs
  read.csv(here::here(year, model, "processed", "catch.csv")) %>%
    dplyr::bind_cols(year = yrs) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(Observed = obs / 1000,
                     Estimated = pred / 1000,
                     years = "All years") -> dat

  dplyr::filter(dat, year %in% (max(yrs) - 20):max(yrs)) %>%
    dplyr::mutate(years = "Recent years") %>%
    dplyr::bind_rows(dat) %>%
    tidyr::pivot_longer(c(-year, -years)) %>%
    ggplot2::ggplot(ggplot2::aes(year, value, color = name, lty = name)) +
    ggplot2::geom_line() +
    scico::scale_color_scico_d(name = "", palette = "roma") +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c(1,1)) +
    ggplot2::facet_wrap(~years, scales = "free",
                        dir = "v") +
    ggplot2::ylab("Catch (kt)\n") +
    ggplot2::xlab("\nYear") +
    ggplot2::expand_limits(y = 0) +
    funcr::theme_report() +
    ggplot2::theme(legend.justification=c(1,0),
                   legend.position=c(0.98,0.8))

  ggplot2::ggsave(here::here(year, model, "figs", "catch.png"),
         width = 6.5, height = 6.5, units = "in", dpi = 200)

}
