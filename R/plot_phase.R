#' Phase plot
#'
#' @param year model year
#' @param model  folder model is in
#' @param model_name e.g. goa_nr_2020
#'
#' @return
#' @export plot_phase
#'
#' @examples
plot_phase <- function(year, model, model_name){

  if (!dir.exists(here::here(year, model, "processed"))){
    stop("must run 'process_results' before creating figures")
  }

  STD <- read.delim(here::here(year, model, paste0(model_name, ".std")), sep="", header = TRUE)
  bio = read.csv(here::here(year, model, "processed", "bio_rec_f.csv"))
  bby = read.csv(here::here(year, model, "processed", "b35_b40_yld.csv"))
  yrs = read.csv(here::here(year, model, "processed", "ages_yrs.csv"))$yrs
  B35 = bby$B35
  yield = bby$yld_rat

  Fabc <- 0.35/0.4

  segs = data.frame(x1 = rep(c(0.05, 0.4/0.35), 2),
                    x2 = rep(c(0.4/0.35, 2.8), 2),
                    y1 = c(0, 1, 0, Fabc),
                    y2 = c(1, 1, Fabc, Fabc),
                    group = factor(c("ofl", "ofl", "abc", "abc"),
                                   levels = c("ofl", "abc")))

  data.frame(year = min(yrs):(max(yrs) + 2),
             x = c(bio$sp_biom, STD$value[which(STD$name=="spawn_biom_proj")][1],
                   STD$value[which(STD$name=="spawn_biom_proj")][2]) / B35,
             y = c(bio$F, STD$value[which(STD$name=="F40")] * yield,
                   STD$value[which(STD$name=="F40")] * yield) /
               STD$value[which(STD$name=="F35")]) %>%
    dplyr::mutate(label = stringr::str_sub(year, 3),
                  decade = (floor(year / 10) * 10))  %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_path(color = "darkgray", show.legend = FALSE) +
    ggplot2::geom_label(ggplot2::aes(label=label, color = decade), label.size = 0,
                        show.legend = FALSE, size = 3, family="Times", alpha = 0.5) +
    ggplot2::geom_segment(data = segs, ggplot2::aes(x1, y1, xend = x2, yend = y2, linetype = group)) +
    ggplot2::scale_linetype_manual(values = c(1, 3),
                                   labels = c(expression(italic(F[OFL])),
                                              expression(italic(F[ABC]))),
                                   name = "") +
    scico::scale_color_scico(palette = "roma") +
    ggplot2::ylab(expression(italic(F/F["35%"]))) +
    ggplot2::xlab(expression(italic(SSB/B["35%"]))) +
    funcr::theme_report() +
    ggplot2::theme(legend.justification=c(1,0),
                   legend.position=c(0.9,0.85))

  # Can create a zoomed in figure with this code
  # library(ggforce)
  # facet_zoom(xlim = c(0, 2.8),
  #            ylim = c(0, 1),
  #            zoom.data = z,
  #            horizontal = FALSE) +
  # ggplot2::theme(zoom.y = element_blank(), validate = FALSE)


  ggplot2::ggsave(here::here(year, model, "figs", "phase_plane.png"),
         width = 6.5, height = 6.5, units = "in", dpi = 200)

}
