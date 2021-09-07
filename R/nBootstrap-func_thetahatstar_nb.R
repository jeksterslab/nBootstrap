#' Constructor for the thetahatstar_nb Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams thetahatstar_nb
#'
#' @returns A function with `x` and `func` in the environment.
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @examples
#' x <- rnorm(n = 100)
#' func <- median
#'
#' func_thetahatstar_nb(x, func = func)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
func_thetahatstar_nb <- function(x,
                                 func,
                                 ...) {
  # using a constructor puts x, func, ... in the output's environment
  # this is useful when using parLapply
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x)
  )
  if (is.vector(x)) {
    output <- function(i) {
      return(
        func(
          x[sample(length(x), replace = TRUE)],
          ...
        )
      )
    }
  } else {
    output <- function(i) {
      return(
        func(
          x[sample(nrow(x), replace = TRUE), ],
          ...
        )
      )
    }
  }
  return(output)
}
