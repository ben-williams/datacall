
#' Plot recruitment estimates
#'
#' @param year assessment year
#' @param model folder the model is in
#'
#' @return
#' @export plot_rec
#'
#' @examples
plot_rec <- function(year, model){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs -> yrs
  read.csv(here::here(year, model, "processed", "bio_rec_f.csv")) %>%
  dplyr::select(recruits) %>%
  dplyr::bind_cols(read.csv(here::here(year, model, "processed", "mceval.csv")) %>%
                     dplyr::select(log_mean_rec, paste0("log_rec_dev_", yrs)) %>%
                     dplyr::mutate(group = 1:dplyr::n()) %>%
                     tidyr::pivot_longer(c(-group, -log_mean_rec), values_to = "rec_dev") %>%
                     dplyr::mutate(year = as.numeric(gsub("log_rec_dev_", "", name)),
                                   rec_dev = exp(log_mean_rec + rec_dev)) %>%
                     dplyr::group_by(year) %>%
                     dplyr::summarise(lci = quantile(rec_dev, 0.025),
                                      uci = quantile(rec_dev, 0.975))) -> dat

dat %>%
  ggplot2::ggplot(ggplot2::aes(year, recruits)) +
  ggplot2::geom_col(width = 0.8, alpha = 0.6) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lci, ymax = uci), width = 0.5) +
  ggplot2::scale_x_continuous(name = "\nYear",
                              breaks = funcr::tickr(dat, year, 10, start = 1960)$breaks,
                              labels = funcr::tickr(dat, year, 10, start = 1960)$labels) +
  ggplot2::ylab("Age-2 Recruitment (millions)") +
  funcr::theme_report()

ggplot2::ggsave(here::here(year, model, "figs", "recruits.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
