#' Plot age and length compositions
#'
#' @param year = assessment year
#' @param model = folder the model lives in
#' @return
#' @export plot_comps
#'
#' @examples
#' plot_comps(year, model)
plot_comps <- function(year, model){

  fac = read.csv(here::here(year, model, "processed", "fac.csv"))
  fsc = read.csv(here::here(year, model, "processed", "fsc.csv"))
  sac = read.csv(here::here(year, model, "processed", "sac.csv"))
  ssc = read.csv(here::here(year, model, "processed", "ssc.csv"))

  variable = "age"
  fleet = "fishery"
  dat = fac
  dat %>%
    dplyr::mutate(Age = factor(Age),
                  Year = factor(Year)) %>%
    dplyr::filter(groups == "obs") %>%
    ggplot2::ggplot(ggplot2::aes(age, value)) +
    ggplot2::geom_col(ggplot2::aes(fill = Age), width = 1, color = 1) +
    ggplot2::facet_wrap(~Year, strip.position="right",
                        dir = "v",
                        ncol = 1) +
    ggplot2::geom_line(data = dplyr::filter(dat, groups == "pred"),
                       ggplot2::aes(age, value, group = 1)) +
    ggplot2::geom_point(data = dplyr::filter(dat, groups == "pred"),
                        ggplot2::aes(age, value, group = 1)) +
    ggplot2::theme(panel.spacing.y = grid::unit(0, "mm")) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5)) +
    ggplot2::xlab(paste0("\n", Hmisc::capitalize(variable))) +
    ggplot2::ylab(paste0(Hmisc::capitalize(fleet)," ", variable,  " composition\n")) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, age, start = 0, min = 5)$breaks,
                                labels = funcr::tickr(dat, age, start = 0, min = 5)$labels) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(here::here(year, model, "figs", "fishery_age_comp.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)

  variable = "length"
  fleet = "fishery"
  dat = fsc
  dat %>%
    dplyr::mutate(Length = factor(length),
                  Year = factor(Year)) %>%
    dplyr::filter(groups == "obs") %>%
    ggplot2::ggplot(ggplot2::aes(length, value)) +
    ggplot2::geom_col(ggplot2::aes(fill = Length), width = 1, color = 1) +
    ggplot2::facet_wrap(~Year, strip.position="right",
                        dir = "v",
                        ncol = 1) +
    ggplot2::geom_line(data = dplyr::filter(dat, groups == "pred"),
                       ggplot2::aes(length, value, group = 1)) +
    ggplot2::geom_point(data = dplyr::filter(dat, groups == "pred"),
                        ggplot2::aes(length, value, group = 1)) +
    ggplot2::theme(panel.spacing.y = grid::unit(0, "mm")) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5)) +
    ggplot2::xlab(paste0("\n", Hmisc::capitalize(variable))) +
    ggplot2::ylab(paste0(Hmisc::capitalize(fleet)," ", variable,  " composition\n")) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, length, start = 0, min = 5)$breaks,
                                labels = funcr::tickr(dat, length, start = 0, min = 5)$labels) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(here::here(year, model, "figs", "fishery_length_comp.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)

  # survey ----

  variable = "age"
  fleet = "survey"
  dat = sac
  dat %>%
    dplyr::mutate(Age = factor(Age),
                  Year = factor(Year)) %>%
    dplyr::filter(groups == "obs") %>%
    ggplot2::ggplot(ggplot2::aes(age, value)) +
    ggplot2::geom_col(ggplot2::aes(fill = Age), width = 1, color = 1) +
    ggplot2::facet_wrap(~Year, strip.position="right",
                        dir = "v",
                        ncol = 1) +
    ggplot2::geom_line(data = dplyr::filter(dat, groups == "pred"),
                       ggplot2::aes(age, value, group = 1)) +
    ggplot2::geom_point(data = dplyr::filter(dat, groups == "pred"),
                        ggplot2::aes(age, value, group = 1)) +
    ggplot2::theme(panel.spacing.y = grid::unit(0, "mm")) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5)) +
    ggplot2::xlab(paste0("\n", Hmisc::capitalize(variable))) +
    ggplot2::ylab(paste0(Hmisc::capitalize(fleet)," ", variable,  " composition\n")) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, age, start = 0, min = 5)$breaks,
                                labels = funcr::tickr(dat, age, start = 0, min = 5)$labels) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(here::here(year, model, "figs", "survey_age_comp.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)

  variable = "length"
  fleet = "survey"
  dat = ssc
  dat %>%
    dplyr::mutate(Length = factor(length),
                  Year = factor(Year)) %>%
    dplyr::filter(groups == "obs") %>%
    ggplot2::ggplot(ggplot2::aes(length, value, fill = Length)) +
    ggplot2::geom_col(width = 1, color = 1) +
    ggplot2::facet_wrap(~Year, strip.position="right",
                        dir = "v",
                        ncol = 1) +
    # ggplot2::geom_line(data = dplyr::filter(dat, groups == "pred"),
    #                    ggplot2::aes(length, value, group = 1)) +
    # ggplot2::geom_point(data = dplyr::filter(dat, groups == "pred"),
    #                     ggplot2::aes(length, value, group = 1)) +
    ggplot2::theme(panel.spacing.y = grid::unit(0, "mm")) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5)) +
    ggplot2::xlab(paste0("\n", Hmisc::capitalize(variable))) +
    ggplot2::ylab(paste0(Hmisc::capitalize(fleet)," ", variable,  " composition\n")) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, length, start = 0, min = 5)$breaks,
                                labels = funcr::tickr(dat, length, start = 0, min = 5)$labels) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(here::here(year, model, "figs", "survey_length_comp.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)


}


