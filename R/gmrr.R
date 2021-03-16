#' gmrr
#'
#' Calculate geometric means of an xts series of returns.
#'
#' @param returns_xts An xts object whose values are period-over-period returns
#'   observed for the assets specified by the identifiers in the column names.
#'   Returns are NOT assumed to be in 'percent form': i.e., make sure that in
#'   whatever xts is passed as \code{returns_xts}, a return of \emph{12}% is
#'   represented as \emph{0.12}.
#'
#' @return A numeric vector whose values are the geometric means of the returns
#'   in \code{returns_xts} and whose names are the identifiers for which each
#'   geometric mean return was calculated.
#'
#' @export
gmrr <- function(returns_xts){

  part_1 <- prod(as.numeric(zoo::coredata(returns_xts)) + 1)
  part_2 <- 1/nrow(returns_xts)

  part_1^part_2 - 1

}
