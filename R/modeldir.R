
#' model function directory and tpl files
#'
#' @return
#' @export modeldir
#'
#' @examples
modeldir <- function(){


  if (!dir.exists(paste0("data/models/ageage"))){
    dir.create("data/models/ageage", recursive=TRUE)
  }

  if (!dir.exists(paste0("data/models/length_sd"))){
    dir.create("data/models/length_sd", recursive=TRUE)
  }

  if (!dir.exists(paste0("data/models/vbl"))){
    dir.create("data/models/VBL", recursive=TRUE)
  }

  if (!dir.exists(paste0("data/models/wvbl"))){
    dir.create("data/models/wVBL", recursive=TRUE)
  }

  file.copy(system.file("models", "AGEAGE.tpl", package = "datacall"),
            "data/models/ageage")

  file.copy(system.file("models", "lengthSD.tpl", package = "datacall"),
            "data/models/length_sd")

  file.copy(system.file("models", "VBL.tpl", package = "datacall"),
            "data/models/vbl")

  file.copy(system.file("models", "wVBL.tpl", package = "datacall"),
            "data/models/wvbl")

}
