#' Jackknife
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams thetahatstar_nb
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @returns Returns a matrix if `func` returns a vector.
#'   Otherwise, it returns a list.
#'
#' @examples
#' x <- rnorm(n = 100)
#' func <- median
#'
#' output <- jackknife(x, func = func)
#' hist(output)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
jackknife <- function(x,
                      func,
                      ...) {
  foo <- func_jackknife(
    x = x,
    func = func,
    ...
  )
  bind <- is.vector(
    func(x, ...)
  )
  if (is.vector(x)) {
    n <- length(x)
  } else {
    n <- nrow(x)
  }
  output <- lapply(
    X = seq_len(n),
    FUN = foo
  )
  if (bind) {
    return(
      do.call(
        what = "rbind",
        args = output
      )
    )
  } else {
    return(output)
  }
}
