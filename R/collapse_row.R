#' helper function for creating dat file
#'
#' @param data
#'
#' @return
#' @export collapse_row
#'
#' @examples
collapse_row <- function(data){

  l1 = paste(as.vector(data[1,]), collapse = " ")

  for(i in 2:nrow(data)){
    l2 = paste(as.vector(data[i,]), collapse = " ")
    l1 = c(l1, l2)
  }
  l1
}
