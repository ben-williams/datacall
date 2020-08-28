#' compute length at age and length sd
#'
#' @param year= analysis year
#'
#' @return
#' @export weight_length
#'
#' @examples
weight_length <- function(year, admb_home, recage, plus_age){

  if (!dir.exists(here::here(year, "/data/output"))){
    dir.create(here::here(year, "/data/output"), recursive=TRUE)
  }

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  srv_al_key(year) %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(sample_size = mean(sample_size),
              Lbar = sum(prop * length) / sum(prop) * 0.1,
              SD_Lbar = sqrt(1 / (sum(prop) - 1) * sum(prop * (length / 10 - Lbar)^2))) %>%
    dplyr::filter(SD_Lbar>=0.01) -> laa_stats

  write.csv(laa_stats, here::here(year, "data/output/laa_stats.csv"))

  laa_stats


  # run models ----

  setwd(here::here(year, "data/models/VBL"))
  # Estimate mean length
  c("# Data file for LVB model of mean length",
    "# Number of ages (nages)",
    nrow(laa_stats),
    "# Ages with observed mean length (ages)",
    paste(laa_stats$age, collapse=" "),
    "# Observed mean length (Lbar_obs)",
    paste(laa_stats$Lbar, collapse=" "),
    "# SD in Observed mean length (Lbar_obs)",
    paste(laa_stats$SD_Lbar, collapse=" ")) %>%
    write.table("VBL.dat", quote=FALSE, row.names=FALSE, col.names=FALSE)

  R2admb::compile_admb("VBL", verbose = TRUE)
  R2admb::run_admb("VBL", verbose = TRUE)

  # retrieve output

  REP <- readLines("VBL.REP", warn=FALSE)
  Linf = as.numeric(sub(".*? ", "", REP[1]))
  k = as.numeric(sub(".*? ", "", REP[2]))
  t0 = as.numeric(sub(".*? ", "", REP[3]))


  # run model 2
  setwd(here::here(year, "data/models/length_sd"))

  c("# Data file for LVB model of mean length",
    "# Number of ages (nages)",
    nrow(laa_stats),
    "# Ages with observed mean length (ages)",
    paste(laa_stats$age, collapse=" "),
    "# Observed mean length (Lbar_obs)",
    paste(laa_stats$Lbar, collapse=" "),
    "# SD in Observed mean length (Lbar_obs)",
    paste(laa_stats$SD_Lbar, collapse=" "),
    "# Sample size vector",
    paste(laa_stats$sample_size, collapse=" ")) %>%
    write.table("lengthSD.dat", quote=FALSE, row.names=FALSE, col.names=FALSE)


  R2admb::compile_admb("lengthSD", verbose = TRUE)
  R2admb::run_admb("lengthSD", verbose = TRUE)
  STD <- read.delim("lengthSD.STD", sep="")
  a <- STD$value[1]
  b <- STD$value[2]
  (params <- cbind(Linf, k, t0, a, b))

  write.csv(params, here::here(year, "data/output/lbar_params.csv"))


  # Compute Sz@A transition matrix
  lenbins = read.csv(here::here(year, "data/user_input/len_bin_labels.csv"))$len_bins

  expand.grid(age = recage:plus_age,
              length = lenbins) %>%
    dplyr::mutate(Lbar = Linf * (1 - exp(-k * (age - t0))),
           Lbar = ifelse(age == plus_age, 0.5 * (Lbar + Linf), Lbar),
           SD_Lbar = a * log(age) + b,
           prob = ifelse(length == min(length),
                         pnorm(length + 0.5, Lbar, SD_Lbar),
                         pnorm(length + 0.5, Lbar, SD_Lbar) -
                           pnorm(length -0.5, Lbar, SD_Lbar)),
           prob = round(prob, digits = 4)) %>%
    dplyr::select(age, length, prob) %>%
    tidyr::pivot_wider(names_from = length, values_from = prob) -> saa

    write.csv(saa, here::here(year, "data/output/SaA.csv"))
    saa

}
