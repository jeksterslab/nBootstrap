#' Generate Nonparametric Bootstrap Samples
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams thetahatstar_nb
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @returns Returns the same data type as input data.
#'
#' @examples
#' x <- rnorm(n = 100)
#' xstar_nb(x, bcap = 5L)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
xstar_nb <- function(x,
                     bcap = 5000L,
                     seed = NULL) {
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x)
  )
  bcap <- as.integer(bcap)
  foo <- func_xstar_nb(x)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  return(
    lapply(
      X = seq_len(bcap),
      FUN = foo
    )
  )
}
