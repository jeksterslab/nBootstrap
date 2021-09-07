#' Constructor for the jackknife Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams jackknife
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @returns A function with `x` and `func` in the environment.
#'
#' @examples
#' x <- rnorm(n = 100)
#' func <- median
#'
#' func_jackknife(x, func = func)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
func_jackknife <- function(x,
                           func,
                           ...) {
  # using a constructor puts x, func, ... in the output's environment
  # this is useful when using parLapply
  if (is.vector(x)) {
    output <- function(i) {
      return(
        func(x[-i], ...)
      )
    }
  } else {
    output <- function(i) {
      return(
        func(x[-i, ], ...)
      )
    }
  }
  return(output)
}
