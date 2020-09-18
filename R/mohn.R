#' Calculate Mohn's rho
#'
#' @param x     retro data - a matrix or dataframe with years as rownames
#' @param peels     defaults to 10, or NULL to use all retro data
#' @param details     whether to return the intermediate calculations of relative bias.
#' @param plot    whether to plot the retrospective trajectories.
#' @return
#' @export mohn
#'
#' @examples
#' mohn(data)
mohn <- function(x){
  # this function comes directly from the icesAdvice package

  function (x, peels = 10, details = FALSE, plot = FALSE, ...)
  {
    if (is.null(peels))
      peels = ncol(x) - 1
    if (peels > ncol(x) - 1)
      stop("peels cannot exceed number of retro columns")
    n = nrow(x)
    retro = rep(NA_real_, n)
    for (i in seq_len(peels)) retro[n - i] = x[n - i, 1 + i]
      compare = na.omit(data.frame(base = x[, 1], retro, row.names = rownames(x)))
      compare$relbias = (compare$retro - compare$base) / compare$base
      rho = mean(compare$relbias)
      out = if (details)
        list(compare = compare, rho = rho)
    else rho
    if (plot) {
      matplot(as.numeric(rownames(x)), x, type = "l",
              lty = 1, ann = FALSE, ...)
      points(as.numeric(rownames(compare)), compare$retro,
             ...)
    }
    out
  }

}
