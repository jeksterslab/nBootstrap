#' Bias-Corrected and Accelerated Confidence Interval Helper Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param jackknife Numeric matrix.
#'   jackknife estimates.
#' @param thetahat Numeric vector.
#'   Vector of estimates from the original sample data.
#' @inheritParams ci_pc
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @returns A list.
#'
#' @examples
#' x <- rnorm(n = 100)
#' func <- median
#' thetahat <- func(x)
#' thetahatstar <- thetahatstar_nb(
#'   x,
#'   func = func,
#'   bcap = 5L
#' )
#' jackknife <- jackknife(x, func = func)
#'
#' ci_bca_helper(
#'   thetahatstar,
#'   jackknife = jackknife,
#'   thetahat = thetahat
#' )
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
ci_bca_helper <- function(x,
                          jackknife = NULL,
                          thetahat,
                          alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x),
    is.vector(alpha),
    !any(alpha <= 0),
    !any(alpha > 1)
  )
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
  k <- length(x)
  bcap <- length(x[[1]])
  stopifnot(
    k == length(thetahat)
  )
  z0hat <- lapply(
    X = seq_len(k),
    FUN = function(i) {
      return(
        stats::qnorm(
          sum(x[[i]] < thetahat[i]) / bcap
        )
      )
    }
  )
  z1 <- stats::qnorm(p = probs)
  if (is.null(jackknife)) {
    ahat <- NULL
  } else {
    ahat <- lapply(
      X = seq_len(ncol(jackknife)),
      FUN = function(i) {
        parenthesis <- mean(jackknife[, i]) - jackknife[, i]
        numerator <- sum(parenthesis^3)
        denominator <- 6 * ((sum(parenthesis^2))^(3 / 2))
        return(
          numerator / denominator
        )
      }
    )
  }
  return(
    list(
      x = unname(x),
      probs = probs,
      bcap = bcap,
      k = k,
      z0hat = unname(z0hat),
      z1 = z1,
      ahat = unname(ahat)
    )
  )
}
