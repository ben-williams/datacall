
#' Recruitment/SSB plot
#'
#' @param year model year
#' @param model folder model is in
#' @param rec_age age at first recruitment
#'
#' @return
#' @export plot_rec_ssb
#'
#' @examples plot_rec_ssb(year, model, rec_age)
plot_rec_ssb <- function(year, model, rec_age){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs -> yrs
read.csv(here::here(year, model, "processed", "bio_rec_f.csv")) %>%
  dplyr::select(sp_biom, recruits) %>%
  dplyr::bind_cols(year = yrs) -> dat

data.frame(ssb = dat$sp_biom[1:(length(yrs) - rec_age)] / 1000,
           rec = dat$recruits[(rec_age + 1):length(yrs)],
           year = yrs[1:(length(yrs)-2)]) %>%
  dplyr::mutate(label = stringr::str_sub(year, 3),
                decade = (floor(year / 10) * 10)) %>%
  ggplot2::ggplot(ggplot2::aes(ssb, rec)) +
  ggplot2::geom_label(ggplot2::aes(label=label, color = decade),
                      label.size = 0, show.legend = FALSE, size = 3, family="Times", alpha = 0.85) +
  ggplot2::expand_limits(x = 0, y = 0) +
  scico::scale_color_scico(palette = "romaO") +
  ggplot2::xlab("\nSSB (kt)") +
  ggplot2::ylab("Recruitment (millions)\n") +
  funcr::theme_report()

ggplot2::ggsave(here::here(year, model, "figs", "recr-ssb.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
