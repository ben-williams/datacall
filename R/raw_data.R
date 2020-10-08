#' Raw data call from AFSC and AKFIN servers
#'
#' @param species is the species of interest e.g., "NORK", "DUSK"
#' @param year is the year you would like data through
#' @param afsc_user AFSC server username
#' @param afsc_pwd AFSC server password
#' @param akfin_user AKFIN server username
#' @param akfin_pwd AKFIN server password
#' @param ...
#'
#' @return
#' @importFrom dplyr %>%
#' @export raw_data
#'
#' @examples
#'raw_data("NORK", 2020, afsc_user, afsc_pwd, akfin_user, akfin_pwd)
#'
raw_data <- function(species, year, afsc_user, afsc_pwd, akfin_user, akfin_pwd, ...){

  if(species == "NORK"){
    afsc_species = 30420
    norpac_species = 303
  }

  if(species == "DUSK"){
     afsc_species =  30150
     afsc_species2 = 30152
     norpac_species = 330
  }

  region = "GOA"

  if (!dir.exists(here::here(year, "data", "raw"))){
  dir.create(here::here(year, "data", "raw"), recursive=TRUE)
  }

  akfin = RODBC::odbcConnect("akfin", uid=akfin_user, pwd=akfin_pwd)


  # fishery catch data ----
  RODBC::sqlQuery(akfin,
           paste0("SELECT COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,
         COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_NAME,
         COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,
         COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,
         COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA,
         COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,
         COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,
         COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED
         FROM COUNCIL.COMPREHENSIVE_BLEND_CA
         WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA = 'GOA'
         AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ", year,"
                AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = '", species,"'"),
           believeNRows=FALSE) %>%
    write.csv(here::here(year, "data", "raw", "fishery_catch_data.csv"), row.names = FALSE)



  RODBC::sqlQuery(akfin,
           paste0("SELECT NORPAC.DEBRIEFED_SPCOMP_MV.YEAR,
                NORPAC.DEBRIEFED_SPCOMP_MV.HAUL_DATE,
                NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES,
                NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA,
                NORPAC.DEBRIEFED_SPCOMP_MV.EXTRAPOLATED_WEIGHT
                FROM NORPAC.DEBRIEFED_SPCOMP_MV
                INNER JOIN NORPAC.DEBRIEFED_HAUL_MV
                ON NORPAC.DEBRIEFED_SPCOMP_MV.JOIN_KEY = NORPAC.DEBRIEFED_HAUL_MV.JOIN_KEY
                WHERE NORPAC.DEBRIEFED_SPCOMP_MV.YEAR BETWEEN ", year - 3,"
                AND ", year - 1,"
                AND NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA = 'GOA'
                AND NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES = ", norpac_species),
           believeNRows=FALSE) %>%
    write.csv(here::here(year, "data", "raw", "fishery_obs_data.csv"), row.names = FALSE)

  # fishery age comp ----
  age_data <- RODBC::sqlQuery(akfin,
                       paste0("SELECT NORPAC.DEBRIEFED_AGE_MV.YEAR,
                            NORPAC.DEBRIEFED_AGE_MV.FMP_AREA,
                            NORPAC.DEBRIEFED_AGE_MV.SPECIES,
                            NORPAC.DEBRIEFED_AGE_MV.LENGTH,
                            NORPAC.DEBRIEFED_AGE_MV.WEIGHT,
                            NORPAC.DEBRIEFED_AGE_MV.AGE,
                            NORPAC.DEBRIEFED_AGE_MV.SPECIMEN_TYPE,
                            NORPAC.DEBRIEFED_AGE_MV.PERFORMANCE
                            FROM NORPAC.DEBRIEFED_AGE_MV
                            WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA'
                            AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ", norpac_species),
                       believeNRows=FALSE)

  haul_join <- RODBC::sqlQuery(akfin,
                        paste0("SELECT TO_CHAR(NORPAC.DEBRIEFED_AGE_MV.HAUL_JOIN)
                             AS HAUL_JOIN
                             FROM NORPAC.DEBRIEFED_AGE_MV
                             WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA'
                             AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ", norpac_species),
                        as.is=TRUE, believeNRows=FALSE)

  port_join <- RODBC::sqlQuery(akfin,
                        paste0("SELECT TO_CHAR(NORPAC.DEBRIEFED_AGE_MV.PORT_JOIN)
                             AS PORT_JOIN
                             FROM NORPAC.DEBRIEFED_AGE_MV
                             WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA'
                             AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ", norpac_species),
                        as.is=TRUE, believeNRows=FALSE)


  dplyr::bind_cols(age_data, haul_join, port_join) %>%
    write.csv(here::here(year, "data", "raw", "fishery_age_comp_data.csv"), row.names = FALSE)

  # Fishery size comp ----

  length_data <- RODBC::sqlQuery(akfin,
                          paste0("SELECT NORPAC.DEBRIEFED_LENGTH_MV.YEAR,
                                NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA,
                                NORPAC.DEBRIEFED_LENGTH_MV.SPECIES,
                                NORPAC.DEBRIEFED_LENGTH_MV.LENGTH,
                                NORPAC.DEBRIEFED_LENGTH_MV.FREQUENCY,
                                NORPAC.DEBRIEFED_LENGTH_MV.PERFORMANCE
                                FROM NORPAC.DEBRIEFED_LENGTH_MV
                                WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA'
                                AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ", norpac_species),
                          believeNRows=FALSE)
  length_haul <- RODBC::sqlQuery(akfin,
                          paste0("SELECT TO_CHAR(NORPAC.DEBRIEFED_LENGTH_MV.HAUL_JOIN)
                                  AS HAUL_JOIN
                                  FROM NORPAC.DEBRIEFED_LENGTH_MV
                                  WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA'
                                  AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ", norpac_species),
                          as.is=TRUE, believeNRows=FALSE)

  length_port <- RODBC::sqlQuery(akfin,
                          paste0("SELECT TO_CHAR(NORPAC.DEBRIEFED_LENGTH_MV.PORT_JOIN)
                                 AS PORT_JOIN
                                 FROM NORPAC.DEBRIEFED_LENGTH_MV
                                 WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA'
                                 AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ", norpac_species),
                          as.is=TRUE, believeNRows=FALSE)

  dplyr::bind_cols(length_data, length_haul, length_port) %>%
    write.csv(here::here(year, "data", "raw", "fishery_size_comp_freq.csv"), row.names = FALSE)

  RODBC::odbcClose(akfin)

  # survey data ----
  afsc = RODBC::odbcConnect("afsc", uid=afsc_user, pwd=afsc_pwd)


  # survey age comp ----
  RODBC::sqlQuery(afsc,
           paste0("SELECT GOA.AGECOMP_TOTAL.SURVEY,
          GOA.AGECOMP_TOTAL.SURVEY_YEAR,
          GOA.AGECOMP_TOTAL.SPECIES_CODE,
          GOA.AGECOMP_TOTAL.SEX,
          GOA.AGECOMP_TOTAL.AGE,
          GOA.AGECOMP_TOTAL.AGEPOP,
          GOA.AGECOMP_TOTAL.MEAN_LENGTH,
          GOA.AGECOMP_TOTAL.STANDARD_DEVIATION
          FROM GOA.AGECOMP_TOTAL
                 WHERE GOA.AGECOMP_TOTAL.SPECIES_CODE = ", afsc_species),
           believeNRows=FALSE) -> sac

  if(species!="DUSK"){
    write.csv(sac, here::here(year, "data", "raw", "srv_age_comp.csv"))
  } else {
    dplyr::bind_rows(sac,
    RODBC::sqlQuery(afsc,
                    paste0("SELECT GOA.AGECOMP_TOTAL.SURVEY,
          GOA.AGECOMP_TOTAL.SURVEY_YEAR,
          GOA.AGECOMP_TOTAL.SPECIES_CODE,
          GOA.AGECOMP_TOTAL.SEX,
          GOA.AGECOMP_TOTAL.AGE,
          GOA.AGECOMP_TOTAL.AGEPOP,
          GOA.AGECOMP_TOTAL.MEAN_LENGTH,
          GOA.AGECOMP_TOTAL.STANDARD_DEVIATION
          FROM GOA.AGECOMP_TOTAL
                 WHERE GOA.AGECOMP_TOTAL.SPECIES_CODE = ", afsc_species),
                    believeNRows=FALSE)) %>%
          write.csv(here::here(year, "data", "raw", "srv_age_comp.csv"), row.names = FALSE)
      }

  # survey age comp II ----

  RODBC::sqlQuery(afsc,
           paste0("SELECT RACEBASE.SPECIMEN.CRUISE,
          GOA.BIENNIAL_SURVEYS.YEAR,
          RACEBASE.SPECIMEN.HAULJOIN,
          RACEBASE.SPECIMEN.CRUISEJOIN,
          RACEBASE.SPECIMEN.SPECIES_CODE,
          RACEBASE.SPECIMEN.SPECIMENID,
          RACEBASE.SPECIMEN.LENGTH,
          RACEBASE.SPECIMEN.SEX,
          RACEBASE.SPECIMEN.AGE,
          RACEBASE.SPECIMEN.WEIGHT,
          RACEBASE.SPECIMEN.REGION,
          RACEBASE.HAUL.PERFORMANCE
          FROM (RACEBASE.HAUL
          INNER JOIN RACEBASE.SPECIMEN
          ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)
          INNER JOIN GOA.BIENNIAL_SURVEYS
          ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
          WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=", afsc_species,")
          AND ((RACEBASE.SPECIMEN.REGION)='GOA')
                 AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
           believeNRows=FALSE) -> saas

  if(species != "DUSK"){
    write.csv(saas, here::here(year, "data", "raw", "srv_age_specimens.csv"))
  } else {
    dplyr::bind_rows(saas,
    RODBC::sqlQuery(afsc,
                    paste0("SELECT RACEBASE.SPECIMEN.CRUISE,
          GOA.BIENNIAL_SURVEYS.YEAR,
          RACEBASE.SPECIMEN.HAULJOIN,
          RACEBASE.SPECIMEN.CRUISEJOIN,
          RACEBASE.SPECIMEN.SPECIES_CODE,
          RACEBASE.SPECIMEN.SPECIMENID,
          RACEBASE.SPECIMEN.LENGTH,
          RACEBASE.SPECIMEN.SEX,
          RACEBASE.SPECIMEN.AGE,
          RACEBASE.SPECIMEN.WEIGHT,
          RACEBASE.SPECIMEN.REGION,
          RACEBASE.HAUL.PERFORMANCE
          FROM (RACEBASE.HAUL
          INNER JOIN RACEBASE.SPECIMEN
          ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)
          INNER JOIN GOA.BIENNIAL_SURVEYS
          ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
          WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=", afsc_species2,")
          AND ((RACEBASE.SPECIMEN.REGION)='GOA')
                 AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
                    believeNRows=FALSE)) %>%
      write.csv(here::here(year, "data", "raw", "srv_age_specimens.csv"), row.names = FALSE)
  }


  # design-based trawl survey biomass ----
  RODBC::sqlQuery(afsc,
           paste0("SELECT GOA.BIOMASS_TOTAL.SURVEY,
             GOA.BIOMASS_TOTAL.YEAR,
             GOA.BIOMASS_TOTAL.SPECIES_CODE,
             GOA.BIOMASS_TOTAL.HAUL_COUNT,
             GOA.BIOMASS_TOTAL.CATCH_COUNT,
             GOA.BIOMASS_TOTAL.MEAN_WGT_CPUE,
             GOA.BIOMASS_TOTAL.VAR_WGT_CPUE,
             GOA.BIOMASS_TOTAL.MEAN_NUM_CPUE,
             GOA.BIOMASS_TOTAL.VAR_NUM_CPUE,
             GOA.BIOMASS_TOTAL.TOTAL_BIOMASS,
             GOA.BIOMASS_TOTAL.BIOMASS_VAR,
             GOA.BIOMASS_TOTAL.MIN_BIOMASS,
             GOA.BIOMASS_TOTAL.MAX_BIOMASS,
             GOA.BIOMASS_TOTAL.TOTAL_POP,
             GOA.BIOMASS_TOTAL.POP_VAR
                  FROM
                  GOA.BIOMASS_TOTAL
                  WHERE
                  (((GOA.BIOMASS_TOTAL.SPECIES_CODE)=", afsc_species,"))"),
           believeNRows=FALSE) -> sb

  if(species != "DUSK"){
    write.csv(sb, here::here(year, "data", "raw", "srv_biomass.csv"), row.names = FALSE)
  } else {
    dplyr::bind_rows(sb,
    RODBC::sqlQuery(afsc,
                    paste0("SELECT GOA.BIOMASS_TOTAL.SURVEY,
             GOA.BIOMASS_TOTAL.YEAR,
             GOA.BIOMASS_TOTAL.SPECIES_CODE,
             GOA.BIOMASS_TOTAL.HAUL_COUNT,
             GOA.BIOMASS_TOTAL.CATCH_COUNT,
             GOA.BIOMASS_TOTAL.MEAN_WGT_CPUE,
             GOA.BIOMASS_TOTAL.VAR_WGT_CPUE,
             GOA.BIOMASS_TOTAL.MEAN_NUM_CPUE,
             GOA.BIOMASS_TOTAL.VAR_NUM_CPUE,
             GOA.BIOMASS_TOTAL.TOTAL_BIOMASS,
             GOA.BIOMASS_TOTAL.BIOMASS_VAR,
             GOA.BIOMASS_TOTAL.MIN_BIOMASS,
             GOA.BIOMASS_TOTAL.MAX_BIOMASS,
             GOA.BIOMASS_TOTAL.TOTAL_POP,
             GOA.BIOMASS_TOTAL.POP_VAR
                  FROM
                  GOA.BIOMASS_TOTAL
                  WHERE
                  (((GOA.BIOMASS_TOTAL.SPECIES_CODE)=", afsc_species2,"))"),
                    believeNRows=FALSE)) %>%
      write.csv("year, data", "raw", "srv_biomass.csv", row.names = FALSE)
    }


  # survey size comp (data)trawl)  ----
  RODBC::sqlQuery(afsc,
           paste0("SELECT GOA.SIZECOMP_TOTAL.SURVEY,
             GOA.SIZECOMP_TOTAL.YEAR,
             GOA.SIZECOMP_TOTAL.SPECIES_CODE,
             GOA.SIZECOMP_TOTAL.LENGTH,
             GOA.SIZECOMP_TOTAL.MALES,
             GOA.SIZECOMP_TOTAL.FEMALES,
             GOA.SIZECOMP_TOTAL.UNSEXED,
             GOA.SIZECOMP_TOTAL.TOTAL
             FROM GOA.SIZECOMP_TOTAL
                    WHERE
                    (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=", afsc_species,"))"),
           believeNRows=FALSE) -> scc

  if(species != "DUSK"){
    write.csv(scc, here::here(year, "data", "raw", "srv_size_comp.csv"), row.names = FALSE)
  } else {
    dplyr::bind_rows(scc,
    RODBC::sqlQuery(afsc,
                    paste0("SELECT GOA.SIZECOMP_TOTAL.SURVEY,
             GOA.SIZECOMP_TOTAL.YEAR,
             GOA.SIZECOMP_TOTAL.SPECIES_CODE,
             GOA.SIZECOMP_TOTAL.LENGTH,
             GOA.SIZECOMP_TOTAL.MALES,
             GOA.SIZECOMP_TOTAL.FEMALES,
             GOA.SIZECOMP_TOTAL.UNSEXED,
             GOA.SIZECOMP_TOTAL.TOTAL
             FROM GOA.SIZECOMP_TOTAL
                    WHERE
                    (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=", afsc_species2,"))"),
                    believeNRows=FALSE)) %>%
      write.csv(here::here(year, "data", "raw", "srv_size_comp.csv"), row.names = FALSE)
  }

  RODBC::sqlQuery(afsc,
           paste0("SELECT RACEBASE.LENGTH.CRUISEJOIN,
             RACEBASE.LENGTH.REGION,
             RACEBASE.LENGTH.SPECIES_CODE,
             RACEBASE.LENGTH.LENGTH,
             RACEBASE.LENGTH.FREQUENCY,
             GOA.BIENNIAL_SURVEYS.YEAR,
             RACEBASE.HAUL.PERFORMANCE,
             RACEBASE.HAUL.HAULJOIN
             FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH
             ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN)
             INNER JOIN GOA.BIENNIAL_SURVEYS
             ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
             WHERE (((RACEBASE.LENGTH.REGION)='GOA')
             AND ((RACEBASE.LENGTH.SPECIES_CODE)=", afsc_species,")
             AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
           believeNRows=FALSE) -> ssf

  if(species != "DUSK"){
    write.csv(ssf, here::here(year, "data", "raw", "srv_size_freq.csv"), row.names = FALSE)
  } else {
    dplyr::bind_rows(ssf,
              RODBC::sqlQuery(afsc,
                              paste0("SELECT RACEBASE.LENGTH.CRUISEJOIN,
             RACEBASE.LENGTH.REGION,
             RACEBASE.LENGTH.SPECIES_CODE,
             RACEBASE.LENGTH.LENGTH,
             RACEBASE.LENGTH.FREQUENCY,
             GOA.BIENNIAL_SURVEYS.YEAR,
             RACEBASE.HAUL.PERFORMANCE,
             RACEBASE.HAUL.HAULJOIN
             FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH
             ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN)
             INNER JOIN GOA.BIENNIAL_SURVEYS
             ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
             WHERE (((RACEBASE.LENGTH.REGION)='GOA')
             AND ((RACEBASE.LENGTH.SPECIES_CODE)=", afsc_species2,")
             AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
                              believeNRows=FALSE)) %>%
      write.csv(ssf, here::here(year, "data", "raw", "srv_size_freq.csv"), row.names = FALSE)
  }

  # size at age data ----
  RODBC::sqlQuery(afsc,
           paste0("SELECT * FROM
                    (RACEBASE.HAUL
                    INNER JOIN
                    RACEBASE.SPECIMEN
                    ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)
                    INNER JOIN GOA.BIENNIAL_SURVEYS
                    ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
                    WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=", afsc_species,")
                    AND ((RACEBASE.SPECIMEN.REGION)='GOA')
                    AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
           believeNRows=FALSE) -> saa

  if(species != "DUSK"){
    write.csv(saa, here::here(year, "data", "raw", "srv_saa_age.csv"))
  } else {
    dplyr::bind_rows(saa,
              RODBC::sqlQuery(afsc,
                              paste0("SELECT * FROM
                    (RACEBASE.HAUL
                    INNER JOIN
                    RACEBASE.SPECIMEN
                    ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)
                    INNER JOIN GOA.BIENNIAL_SURVEYS
                    ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
                    WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=", afsc_species2,")
                    AND ((RACEBASE.SPECIMEN.REGION)='GOA')
                    AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
                              believeNRows=FALSE)) %>%
      write.csv(here::here(year, "data", "raw", "srv_saa_age.csv"), row.names = FALSE)
  }

  RODBC::sqlQuery(afsc,
           paste0("SELECT * FROM
                     (RACEBASE.HAUL
                     INNER JOIN
                     RACEBASE.LENGTH
                     ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN)
                     INNER JOIN GOA.BIENNIAL_SURVEYS
                     ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
                     WHERE (((RACEBASE.LENGTH.REGION)='GOA')
                     AND ((RACEBASE.LENGTH.SPECIES_CODE)=", afsc_species,")
                     AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
           believeNRows=FALSE) -> sal

  if(species != "DUSK"){
    write.csv(sal, here::here(year, "data", "raw", "srv_saa_length.csv"), row.names = FALSE)
  } else {
    dplyr::bind_rows(sal,
              RODBC::sqlQuery(afsc,
                              paste0("SELECT * FROM
                     (RACEBASE.HAUL
                     INNER JOIN
                     RACEBASE.LENGTH
                     ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN)
                     INNER JOIN GOA.BIENNIAL_SURVEYS
                     ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
                     WHERE (((RACEBASE.LENGTH.REGION)='GOA')
                     AND ((RACEBASE.LENGTH.SPECIES_CODE)=", afsc_species2,")
                     AND ((RACEBASE.HAUL.PERFORMANCE)>=0))"),
                              believeNRows=FALSE)) %>%
      write.csv(here::here(year, "data", "raw", "srv_saa_length.csv"))
  }

  RODBC::odbcClose(afsc)


  txt = "Data were downloaded on:"
  dt = format(Sys.time(), "%Y-%m-%d")


  write.table(c(txt, dt), file = here::here(year, "data", "raw", "data_called.txt"),
              sep = "\t", col.names = F, row.names = F)
}
