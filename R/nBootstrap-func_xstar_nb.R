#' Constructor for the xstar_nb Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams xstar_nb
#'
#' @returns A function with `x` in the environment.
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @examples
#' x <- rnorm(n = 100)
#'
#' func_xstar_nb(x)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
func_xstar_nb <- function(x) {
  # using a constructor puts x in the output's environment
  # this is useful when using parLapply
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x)
  )
  if (is.vector(x)) {
    output <- function(i) {
      return(
        x[sample(length(x), replace = TRUE)]
      )
    }
  } else {
    output <- function(i) {
      return(
        x[sample(nrow(x), replace = TRUE), ]
      )
    }
  }
  return(output)
}
