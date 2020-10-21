#' helper function for comp data
#'
#' @param obs  observed data from .rep file
#' @param pred predicted data from .rep file (if used)
#' @param rec_age  recruitement age
#' @param plus_age plus age group
#' @param comp `age` or `length` - default is length
#' @param lenbins set to base unless using alt in which case the file should be in the `user_input`` folder and the name needs to be provided e.g., `lengthbins.csv` - the column must be named `len_bin`
#'
#' @return
#' @export purrit
#'
#' @examples purrit(obs, pred = NULL, rec_age, plus_age, comp = "length", lenbins = "lengthbins.csv")
purrit <- function(obs, pred = NULL, rec_age, plus_age, comp = "length", lenbins = NULL){

  if(is.null(lenbins)){
    lenbins = read.csv(here::here(year, "data", "user_input", "len_bin_labels.csv"))$len_bins
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }

  obs = stringr::str_split(obs, " ")

  purrr::map_if(obs[-1], is.character, as.numeric) %>%
    purrr::map_df(., ~as.data.frame(t(.))) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) -> obs


  if(comp == "age" & !is.null(pred)){
    obs %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> obs

    pred = stringr::str_split(pred, " ")
    purrr::map_if(pred[-1], is.character, as.numeric) %>%
      purrr::map_df(., ~as.data.frame(t(.))) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> pred

    names(pred) <- names(obs) <- c("year", rec_age:plus_age)

    obs %>%
      tidyr::pivot_longer(-year, "age") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::bind_rows(pred %>%
                         tidyr::pivot_longer(-year, "age") %>%
                         dplyr::mutate(groups = "pred")) %>%
      dplyr::mutate(age = as.integer(age),
                    Age = factor(age),
                    Year = factor(year)) -> dat


  } else if(comp != "age" & !is.null(pred)){

    obs %>%
      dplyr::select(1:(length(lenbins) + 1)) -> obs

    pred = stringr::str_split(pred, " ")
    purrr::map_if(pred[-1], is.character, as.numeric) %>%
      purrr::map_df(., ~as.data.frame(t(.))) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::select(1:(length(lenbins) + 1)) -> pred

    names(pred) <- names(obs) <- c("year", lenbins)

    obs %>%
      tidyr::pivot_longer(-year, "length") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::bind_rows(pred %>%
                         tidyr::pivot_longer(-year, "length") %>%
                         dplyr::mutate(groups = "pred")) %>%
      dplyr::mutate(length = as.integer(length),
                    Length = factor(length),
                    Year = factor(year)) -> dat

  } else if(comp == "age" & is.null(pred)){
    obs %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> obs

    names(obs) <- c("year", rec_age:plus_age)

    obs %>%
      tidyr::pivot_longer(-year, "age") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::mutate(age = as.integer(age),
                    Age = factor(age),
                    Year = factor(year)) -> dat

  } else if(comp != "age" & is.null(pred)){

    obs %>%
      dplyr::select(1:(length(lenbins) + 1)) -> obs

    names(obs) <- c("year", lenbins)

    obs %>%
      tidyr::pivot_longer(-year, "length") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::mutate(length = as.integer(length),
                    Length = factor(length),
                    Year = factor(year)) -> dat
  }

  dat
}
