#' Generate Empirical Sampling Distribution
#' Using Nonparametric Bootstrap
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix, data frame, or vector.
#'   Sample data.
#' @param func Fit function.
#'   The first argument should be sample data.
#' @param ... Additional named arguments to pass to `func`.
#' @param bcap Integer.
#'   Number of bootstrap samples.
#' @param seed Integer.
#'   Random seed.
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
#' output <- thetahatstar_nb(x, func = func)
#' hist(output)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
thetahatstar_nb <- function(x,
                            func,
                            ...,
                            bcap = 5000L,
                            seed = NULL) {
  foo <- func_thetahatstar_nb(
    x = x,
    func = func,
    ...
  )
  bcap <- as.integer(bcap)
  bind <- is.vector(
    func(x, ...)
  )
  if (!is.null(seed)) {
    set.seed(seed)
  }
  output <- lapply(
    X = seq_len(bcap),
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
