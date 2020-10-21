#' Plot parameter histograms
#'
#' @param year model year
#' @param model foler the model is in
#' @param model_name e.g., goa_nr_2020
#'
#' @return
#' @export plot_params
#'
#' @examples
plot_params <- function(year, model, model_name){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs -> yrs

  ggplot2::theme_set(funcr::theme_report())

 read.delim(here::here(year, model, paste0(model_name, ".std")), sep="", header = TRUE) %>%
    dplyr::filter(name %in% c("q_srv1", "ABC", "nattymort", "tot_biom",
                              "F40","spawn_biom")) %>%
    dplyr::group_by(name) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = dplyr::case_when(name =="q_srv1" ~ "q_srv",
                                          name =="nattymort" ~ "natmort",
                                          name == "F40" ~ "F",
                                          name == "spawn_biom" ~ "spawn_biom_",
                                          name == "tot_biom" ~ "tot_biom_",
                                          TRUE ~ name),
                  value = dplyr::case_when(name == "ABC" ~ value / 1000,
                                           name == "spawn_biom_" ~ value / 1000,
                                           name == "tot_biom_" ~ value / 1000,
                                           TRUE ~ value),
                  name = factor(name, levels = c("q_srv", "natmort", "F", "ABC", "tot_biom_", "spawn_biom_"))) -> fits

   read.csv(here::here(year, model, "processed", "mceval.csv"))  %>%
     dplyr::select(q_srv1, ABC, natmort, paste0("tot_biom_", yrs),
                   F40, paste0("spawn_biom_", yrs)) %>%
     dplyr::mutate(group = 1:dplyr::n()) %>%
     tidyr::pivot_longer(-group) %>%
     dplyr::mutate(years = as.numeric(gsub('\\D+','', name)),
                   name = gsub('[[:digit:]]+', '', name),
                   value = dplyr::case_when(name=="spawn_biom_" ~ value / 1000,
                                            name=="tot_biom_" ~ value / 1000,
                                            name=="ABC" ~ value / 1000,
                                            TRUE ~ value),
                   name = factor(name, levels = c("q_srv", "natmort", "F", "ABC", "tot_biom_", "spawn_biom_"))) -> dat

   p1 = dat %>%
     dplyr::filter(name == "q_srv") %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 60) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::scale_x_continuous(breaks = seq(0,2.5,0.5)) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "q_srv"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
     ggplot2::xlab(expression("Trawl survey catchability ("*italic(q)*")")) +
     ggplot2::theme(legend.position = "none")


   p2 = dat %>%
     dplyr::filter(name == "natmort") %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 60) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "natmort"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::ylab("Probability density\n") +
     ggplot2::xlab(expression("Natural mortality ("*italic(M)*")")) +
     ggplot2::theme(legend.position = "none")

   p3 = dat %>%
     dplyr::filter(name == "F") %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 60) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "F"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
     ggplot2::xlab(expression(italic(F)["40%"])) +
      ggplot2::theme(legend.position = "none")

   p4 = dat %>%
     dplyr::filter(name == "ABC") %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 60) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "ABC"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
     ggplot2::xlab("ABC (kt)") +
      ggplot2::theme(legend.position = "none")


   p5 = dat %>%
     dplyr::filter(name == "tot_biom_", years == year) %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 37) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "tot_biom_"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
     ggplot2::xlab("Current total biomass (kt)") +
      ggplot2::theme(legend.position = "none")

   p6 = dat %>%
     dplyr::filter(name == "spawn_biom_", years == year) %>%
     ggplot2::ggplot(ggplot2::aes(value)) +
     # facet_wrap(~name, scales = "free", dir = "v") +
     ggplot2::geom_histogram(ggplot2::aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill = ..density..),
                             color = NA, bins = 37) +
     scico::scale_fill_scico(palette = "devon", direction = -1) +
     ggplot2::geom_segment(data = dplyr::filter(fits, name == "spawn_biom_"),
                           mapping = ggplot2::aes(x = value, xend = value, y = 0, yend = Inf), size = 2, color = "darkgray") +
     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
     ggplot2::xlab("Current spawning biomass (kt)") +
      ggplot2::theme(legend.position = "none")

   p7 <- cowplot::plot_grid(p1, p4, p2, p5, p3, p6, align = "v", ncol = 2, rel_heights = c(0.5, 0.5))

   # cowplot::save_plot(here::here(year, model, "figs", "hists.png"), p7, nrow = 3, ncol = 2)
   ggplot2::ggsave(here::here(year, model, "figs", "hists.png"), p7, width = 6.5, height = 8.5, units = "in", dpi = 200)

}
