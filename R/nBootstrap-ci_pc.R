#' Percentile Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix, data frame, or vector.
#'   Vector or matrix of estimates.
#' @param alpha Numeric vector.
#'   Significance level/s.
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @returns A matrix.
#'
#' @examples
#' x <- rnorm(n = 100)
#' func <- median
#' thetahatstar <- thetahatstar_nb(x, func = func)
#'
#' ci_pc(thetahatstar)
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
ci_pc <- function(x,
                  alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x),
    is.vector(alpha),
    !any(alpha <= 0),
    !any(alpha > 1)
  )
  if (is.vector(x)) {
    varnames <- NULL
  } else {
    varnames <- colnames(x)
  }
  lower <- 0.5 * alpha
  upper <- 1 - (lower)
  probs <- sort(
    c(lower, upper)
  )
  if (is.vector(x)) {
    x <- as.matrix(x)
  }
  x <- lapply(
    X = seq_len(ncol(x)),
    FUN = function(i) {
      x[, i]
    }
  )
  output <- lapply(
    X = x,
    FUN = function(x) {
      stats::quantile(x, probs = probs, names = FALSE)
    }
  )
  output <- do.call(
    what = "rbind",
    args = output
  )
  colnames(output) <- paste0("ci_", probs * 100)
  if (!is.null(varnames)) {
    rownames(output) <- varnames
  }
  return(output)
}
