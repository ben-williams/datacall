
#' Title
#'
#' @param year
#'
#' @return
#' @export allometric
#'
#' @examples
allometric <- function(year, admb_home = NULL, rec_age, plus_age){



  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }


  # weight at length statistics ----
  read.csv(here::here(year, "data/raw/srv_saa_age.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(length, weight) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(length) %>%
    dplyr::summarise(Wbar = mean(weight),
              SD_Wbar = sd(weight)) %>%
    tidyr::drop_na() -> wal

  if (!dir.exists(here::here(paste0(year, "/data/output")))){
    dir.create(here::here(paste0(year, "/data/output")), recursive=TRUE)
  }

  write.csv(wal, here::here(year, "data/output/WaL_stats.csv"))


  # Run allometric model
  DAT <- c("# Data file for allometric model of mean weight by length",
           "# Number of lengths (nlengths)",
           nrow(wal),
           "# Lengths with observed mean weight (lengths)",
           paste(wal$length, collapse=" "),
           "# Observed mean weight (Wbar_obs)",
           paste(wal$Wbar, collapse=" "),
           "# SD in Observed mean weight (SD_Wbar)",
           paste(wal$SD_Wbar, collapse=" "))

  setwd(here::here(year, "data/models/allometric"))
  write.table(DAT, "allometric.dat", quote=FALSE, row.names=FALSE, col.names=FALSE)

  # run model

  R2admb::compile_admb("allometric", verbose = TRUE)
  R2admb::run_admb("allometric", verbose = TRUE)
  par <- readLines("allometric.par", warn = FALSE)
  alpha_lw <- as.numeric(strsplit(par[grep("alpha", par) + 1]," ")[[1]])
  beta_lw <- as.numeric(strsplit(par[grep("beta", par) + 1]," ")[[1]])

  setwd(here::here())

  allo = data.frame(alpha_lw = alpha_lw, beta_lw = beta_lw)
  write.csv(allo, here::here(year, "data/output/alpha_beta_lw.csv"))
  allo

  read.csv(here::here( year, "data/raw/srv_saa_age.csv")) %>%
    # dplyr::rename_all(tolower) %>%
    dplyr::select(YEAR, AGE, LENGTH, WEIGHT) %>%
    dplyr::filter(!is.na(AGE))  %>%
    dplyr::select(-YEAR) %>%
    dplyr::group_by(AGE) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(n>1) %>%
    dplyr::ungroup() -> age_data_1


  # Length data
  read.csv(here::here(year, "data/raw/srv_saa_length.csv")) -> length_data_raw

  # Get parameters
  ages<-sort(unique(age_data_1$AGE))
  nages<-length(ages)
  lengths<-sort(unique(age_data_1$LENGTH))
  nlengths<-length(lengths)


  # Get Age-length key together
  n_al<-table(age_data_1$AGE, age_data_1$LENGTH)
  n_l<-colSums(n_al)
  r<-which(n_l<2)
  if(length(r)>0){
    n_l<-n_l[-r]
    n_al<-n_al[,-r]}
  lengths<-as.numeric(names(n_l))
  nlengths<-length(lengths)
  N_l<-matrix(nrow=nlengths)
  rownames(N_l)<-lengths
  for(l in 1:nlengths){
    N_l[l,1] <- sum(subset(length_data_raw$FREQUENCY, length_data_raw$LENGTH==lengths[l]))}
  N_al<-matrix(0,nrow=nages,ncol=nlengths)
  rownames(N_al)<-ages
  colnames(N_al)<-lengths
  for(l in 1:nlengths){
    N_al[,l]<-n_al[,l]/n_l[l]*N_l[l]}

  # Get mean weight and r age-length key together
  Wbar_la<-matrix(NA,nrow=nages,ncol=nlengths)
  rownames(Wbar_la)<-ages
  colnames(Wbar_la)<-lengths
  r_la<-matrix(NA,nrow=nages,ncol=nlengths)
  rownames(r_la)<-ages
  colnames(r_la)<-lengths
  V_Wbar_la<-matrix(NA,nrow=nages,ncol=nlengths)
  rownames(V_Wbar_la)<-ages
  colnames(V_Wbar_la)<-lengths
  V_r_la<-matrix(NA,nrow=nages,ncol=nlengths)
  rownames(V_r_la)<-ages
  colnames(V_r_la)<-lengths
  theta_la<-matrix(NA,nrow=nages,ncol=nlengths)
  rownames(theta_la)<-ages
  colnames(theta_la)<-lengths
  theta_a<-vector(length=nages)
  alpha_l<-vector(length=nlengths)
  for(a in 1:nages){
    for(l in 1:nlengths){
      awl_data<-subset(age_data_1, age_data_1$AGE==ages[a] & age_data_1$LENGTH==lengths[l])
      if(length(awl_data$WEIGHT)>0){
        Wbar_la[a,l]<-mean(awl_data$WEIGHT,na.rm=TRUE)
        if(length(awl_data$WEIGHT)>1){
          V_Wbar_la[a,l]<-var(awl_data$WEIGHT,na.rm=TRUE) / length(awl_data$WEIGHT)}}
      alpha_l[l]<-N_l[l]/sum(N_l)
      theta_la[a,l]<-n_al[a,l]/sum(n_al[,l])
      r_la[a,l]<-alpha_l[l]*theta_la[a,l]}
    theta_a[a]<-sum(r_la[a,])}
  L<-sum(N_l)
  A_l<-colSums(n_al)
  for(a in 1:nages){
    for(l in 1:nlengths){
      V_r_la[a,l]<-alpha_l[l]^2*theta_la[a,l]*(1-theta_la[a,l])/(A_l[l]-1)+alpha_l[l]*(theta_la[a,l]-theta_a[a])^2/L}}

  # Get/compile weight-at-age statistics

  Age<-ages
  SS<-vector(length=nages)
  Wbar<-vector(length=nages)
  SD_Wbar<-vector(length=nages)
  for(a in 1:nages){
    SS[a]<-length(subset(age_data_1$WEIGHT,age_data_1$AGE==ages[a]))
    Wbar[a]<-sum(r_la[a,]*Wbar_la[a,],na.rm=TRUE)/sum(r_la[a,])
    SD_Wbar[a]<-sqrt(sum(r_la[a,]^2*V_Wbar_la[a,]+(Wbar_la[a,]-Wbar[a])^2*V_r_la[a,],na.rm=TRUE)/theta_a[a]^2)*sqrt(length(subset(age_data_1$WEIGHT,age_data_1$AGE==ages[a])))}
  WaA_stats<-as.data.frame(cbind(Age,SS,Wbar,SD_Wbar))
  r<-which(WaA_stats$SD_Wbar==0)
  WaA_stats<-WaA_stats[-r,]
  r<-which(WaA_stats$SS<30)
  WaA_stats<-WaA_stats[-r,]

  # Write data
  write.csv(WaA_stats, here::here(year, "data/output/WaA_stats.csv"))

  setwd(here::here(year, "data/models/wVBL"))

    # Run LVBmodel and estimate mean weight
  PIN <- c("# Parameter starting values for LVB model of mean weight",
           "# Winf","800","# k","0.1","# t0","0","# beta", as.character(beta_lw))

  write.table(PIN, "wVBL.PIN", quote=FALSE, row.names=FALSE, col.names=FALSE)

  DAT<-c("# Data file for LVB model of mean weight",
         "# Number of ages (nages)",
         length(WaA_stats$Age),
         "# Ages with observed mean weight (ages)",
         paste(WaA_stats$Age, collapse=" "),
         "# Observed mean weight (Wbar_obs)",
         paste(WaA_stats$Wbar, collapse=" "),
         "# SD in Observed mean weight (Wbar_obs)",
         paste(WaA_stats$SD_Wbar,collapse=" "))

  write.table(DAT, file="wVBL.DAT", quote=FALSE, row.names=FALSE, col.names=FALSE)

  # run model

  R2admb::compile_admb("wVBL", verbose = TRUE)
  R2admb::run_admb("wVBL", verbose = TRUE)

  REP <- readLines("wVBL.REP", warn=FALSE)

  setwd(here::here())

  Winf = as.numeric(sub(".*? ", "", REP[1]))
  k = as.numeric(sub(".*? ", "", REP[2]))
  t0 = as.numeric(sub(".*? ", "", REP[3]))

  ages_M = rec_age:plus_age
  nages_M = length(ages_M)
  Wbar = Winf * (1 - exp(-k * (ages_M - t0)))^beta_lw
  Wbar[nages_M] = 0.5 * (Wbar[nages_M] + Winf)
  Wbar = round(Wbar, digits=1)
  Wbar_params = cbind(Winf, k, t0, beta_lw)

  write.csv(Wbar_params, here::here(year, "data/output/Wbar_params.csv"))
  write.csv(Wbar, here::here(year, "data/output/waa.csv"))

  Wbar
}



