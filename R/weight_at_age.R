#' estimate allometric relationship and weight-at-age
#'
#' @param year model year
#' @param admb_home = location admb exists on your computer
#' @param rec_age recruitment age
#' @return
#' @export weight_at_age
#'
#' @examples weight_at_age(year = 2020, admb_home = "C:/Program Files (x86)/ADMB-12.1", rec_age = 2)
weight_at_age <- function(year, admb_home, rec_age){

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  if (!file.exists(here::here(year,"data", "output", "ae_model.csv"))){
    stop("You must first run the age-error function 'ageage()")
  } else {
  nages_m = nrow(read.csv(here::here(year, "data", "output", "ae_model.csv")))
  ages_m = rec_age:(rec_age + nages_m - 1)
  }


  # data ----
  read.csv(here::here(year, "data", "raw", "srv_saa_length.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(year >= 1990, !is.na(length)) -> length_data_raw


  read.csv(here::here(year, "data", "raw", "srv_saa_age.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(year, age, length, weight) %>%
    dplyr::filter(year >= 1990, !is.na(age))  %>%
    dplyr::select(-year) -> age_data_raw


  # Get parameters
  ages = sort(unique(age_data_raw$age))
  nages = length(ages)
  lengths = sort(unique(age_data_raw$length))
  nlengths = length(lengths)

  # Subset to ages with >1 obs
  n_a = table(age_data_raw$age)
  r = which(n_a<2)
  if(length(r)>0){
    n_a = n_a[-r]
  }
  ages = as.numeric(names(n_a))
  nages = length(ages)
  age_data_1 = NULL
  for(a in 1:nages){
    t = subset(age_data_raw, age_data_raw$age==ages[a])
    age_data_1 = rbind(age_data_1,t)
  }
  # Get Age-length key together
  n_al = table(age_data_1$age, age_data_1$length)
  n_l = colSums(n_al)
  r = which(n_l<2)

  if(length(r)>0){
    n_l = n_l[-r]
    n_al = n_al[,-r]
  }

  lengths = as.numeric(names(n_l))
  nlengths = length(lengths)
  N_l = matrix(nrow=nlengths)
  rownames(N_l) = lengths

  for(l in 1:nlengths){
    N_l[l,1] = sum(subset(length_data_raw$frequency, length_data_raw$length==lengths[l]))
  }

  N_al = matrix(0, nrow=nages, ncol=nlengths)
  rownames(N_al) = ages
  colnames(N_al) = lengths

  for(l in 1:nlengths){
    N_al[,l] = n_al[,l] / n_l[l] * N_l[l]
    }

  # Get mean weight and r age-length key together
  Wbar_la = matrix(NA,nrow=nages,ncol=nlengths)
  rownames(Wbar_la) = ages
  colnames(Wbar_la) = lengths

  r_la = matrix(NA, nrow=nages, ncol=nlengths)
  rownames(r_la) = ages
  colnames(r_la) = lengths

  V_Wbar_la = matrix(NA, nrow=nages, ncol=nlengths)
  rownames(V_Wbar_la) = ages
  colnames(V_Wbar_la) = lengths

  V_r_la = matrix(NA, nrow=nages, ncol=nlengths)
  rownames(V_r_la) = ages
  colnames(V_r_la) = lengths

  theta_la = matrix(NA, nrow=nages, ncol=nlengths)
  rownames(theta_la) = ages
  colnames(theta_la) = lengths

  theta_a = vector(length=nages)
  alpha_l = vector(length=nlengths)

  for(a in 1:nages){
    for(l in 1:nlengths){
      awl_data = subset(age_data_1,
                        age_data_1$age == ages[a] &
                          age_data_1$length == lengths[l])

      if(length(awl_data$weight) > 0){
        Wbar_la[a,l] = mean(awl_data$weight, na.rm=TRUE)

        if(length(awl_data$weight)>1){
          V_Wbar_la[a,l] = var(awl_data$weight, na.rm=TRUE) / length(awl_data$weight)
        }
      }

      alpha_l[l] = N_l[l] / sum(N_l)
      theta_la[a,l] = n_al[a,l] / sum(n_al[,l])
      r_la[a,l] = alpha_l[l] * theta_la[a,l]
    }
    theta_a[a] = sum(r_la[a,])
  }

  L = sum(N_l)
  A_l = colSums(n_al)

  for(a in 1:nages){
    for(l in 1:nlengths){
      V_r_la[a,l] = alpha_l[l]^2 * theta_la[a,l] *
        (1 - theta_la[a,l]) / (A_l[l] - 1) + alpha_l[l] *
        (theta_la[a,l] - theta_a[a])^2 / L
    }
  }

  # Get/compile weight-at-age statistics
  Age = ages
  SS = vector(length = nages)
  Wbar = vector(length = nages)
  SD_Wbar = vector(length = nages)
  for(a in 1:nages){
    SS[a] = length(subset(age_data_1$weight, age_data_1$age == ages[a]))
    Wbar[a] = sum(r_la[a,] * Wbar_la[a,], na.rm=TRUE) / sum(r_la[a,])

    SD_Wbar[a] = sqrt(sum(r_la[a,]^2 * V_Wbar_la[a,] +
                           (Wbar_la[a,] - Wbar[a])^2 * V_r_la[a,], na.rm=TRUE) /
                       theta_a[a]^2) *
      sqrt(length(subset(age_data_1$weight, age_data_1$age == ages[a])))
  }
  WaA_stats = as.data.frame(cbind(Age,SS,Wbar,SD_Wbar))
  r = which(WaA_stats$SD_Wbar == 0)
  WaA_stats = WaA_stats[-r,]
  r = which(WaA_stats$SS < 30)
  WaA_stats = WaA_stats[-r,]

  # Write data
  write.csv(WaA_stats,
            here::here(year, "data", "output", "waa_stats.csv"), row.names = FALSE)

  # Get/compile weight-at-length statistics
  lw_data = cbind(age_data_raw$length, age_data_raw$weight)
  r = which(is.na(lw_data[,2]) == TRUE)
  if(length(r)>0){
    lw_data = lw_data[-r,]
  }
  lengths = sort(unique(lw_data[,1]))
  lw_mdl_data = matrix(nrow=length(lengths), ncol=3)
  colnames(lw_mdl_data) = c("Length","Wbar","SD_Wbar")
  lw_mdl_data[,1] = lengths

  for(l in 1:length(lengths)){
    q = subset(lw_data[,2], lw_data[,1] == lengths[l])
    lw_mdl_data[l,2] = mean(q)
    lw_mdl_data[l,3] = sd(q)
    }
  r = which(is.na(lw_mdl_data[,3]) == TRUE)
  if(length(r)>0){
    lw_mdl_data = lw_mdl_data[-r,]
    }
  r = -which(lw_mdl_data[,3] == 0)
  if(length(r)>0){
    lw_mdl_data = lw_mdl_data[-r,]
    }

  lw_mdl_data = as.data.frame(lw_mdl_data)

  # Write data
  write.csv(lw_mdl_data, here::here(year, "data", "output", "wal_stats.csv"),
            row.names = FALSE)


  # Run allometric model ----
  DAT = c("# Data file for allometric model of mean weight by length",
          "# Number of lengths (nlengths)",
          nrow(lw_mdl_data),
          "# Lengths with observed mean weight (lengths)",
          paste(lw_mdl_data$Length, collapse=" "),
          "# Observed mean weight (Wbar_obs)",
          paste(lw_mdl_data$Wbar, collapse=" "),
          "# SD in Observed mean weight (SD_Wbar)",
          paste(lw_mdl_data$SD_Wbar, collapse=" "))

  setwd(here::here(year, "data", "models", "allometric"))
  write.table(DAT, "allometric.dat", quote=FALSE,
              row.names=FALSE, col.names=FALSE)

  # run model

  R2admb::compile_admb("allometric", verbose = TRUE)
  R2admb::run_admb("allometric", verbose = TRUE)
  par = readLines("allometric.par", warn = FALSE)
  alpha_lw = as.numeric(strsplit(par[grep("alpha", par) + 1]," ")[[1]])
  beta_lw = as.numeric(strsplit(par[grep("beta", par) + 1]," ")[[1]])

  setwd(here::here())

  allo = data.frame(alpha_lw = alpha_lw, beta_lw = beta_lw)
  write.csv(allo, here::here(year, "data", "output", "alpha_beta_lw.csv"))



  setwd(here::here(year, "data", "models", "wVBL"))

  # Run LVBmodel and estimate mean weight
  PIN <- c("# Parameter starting values for LVB model of mean weight",
           "# Winf", "800",
           "# k", "0.1",
           "# t0", "0",
           "# beta", as.character(beta_lw))

  write.table(PIN, "wVBL.PIN", quote=FALSE, row.names=FALSE, col.names=FALSE)

  WaA_stats = data.frame(WaA_stats)
  DAT<-c("# Data file for LVB model of mean weight",
         "# Number of ages (nages)",
         length(WaA_stats$Age),
         "# Ages with observed mean weight (ages)",
         paste(WaA_stats$Age, collapse=" "),
         "# Observed mean weight (Wbar_obs)",
         paste(WaA_stats$Wbar, collapse=" "),
         "# SD in Observed mean weight (Wbar_obs)",
         paste(WaA_stats$SD_Wbar, collapse=" "))

  write.table(DAT, file="wVBL.DAT", quote=FALSE, row.names=FALSE, col.names=FALSE)

  # run model

  R2admb::compile_admb("wVBL", verbose = TRUE)
  R2admb::run_admb("wVBL", verbose = TRUE)

  REP <- readLines("wVBL.REP", warn=FALSE)

  setwd(here::here())

  Winf = as.numeric(strsplit(REP[grep("Winf", REP)[1]], " ")[[1]][2])
  k = as.numeric(strsplit(REP[grep("k", REP)[1]], " ")[[1]][2])
  t0 = as.numeric(strsplit(REP[grep("t0", REP)[1]], " ")[[1]][2])

  Wbar = Winf * (1 - exp(-k * (ages_m - t0)))^beta_lw
  Wbar[nages_m] = 0.5 * (Wbar[nages_m] + Winf)
  Wbar = round(Wbar, digits=1)
  Wbar_params = cbind(Winf, k, t0, beta_lw)

  write.csv(Wbar_params, here::here(year, "data", "output", "Wbar_params.csv"), row.names = FALSE)
  write.csv(Wbar, here::here(year, "data", "output", "waa.csv"), row.names = FALSE)

  Wbar}


