#' Bias-Corrected Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams ci_bca_helper
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
#' thetahat <- func(x)
#' thetahatstar <- thetahatstar_nb(x, func = func)
#'
#' ci_bc(
#'   thetahatstar,
#'   thetahat = thetahat
#' )
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
ci_bc <- function(x,
                  thetahat,
                  alpha = c(0.05, 0.01, 0.001)) {
  input <- ci_bca_helper(
    x = x,
    jackknife = NULL,
    thetahat = thetahat,
    alpha = alpha
  )
  if (is.vector(x)) {
    varnames <- NULL
  } else {
    varnames <- colnames(x)
  }
  bc_probs <- lapply(
    X = input$z0hat,
    FUN = function(x) {
      stats::pnorm(
        q = 2 * x + input$z1
      )
    }
  )
  output <- lapply(
    X = seq_len(input$k),
    FUN = function(i) {
      stats::quantile(
        input$x[[i]],
        probs = bc_probs[[i]],
        names = FALSE
      )
    }
  )
  output <- do.call(
    what = "rbind",
    args = output
  )
  colnames(output) <- paste0("ci_", input$probs * 100)
  if (!is.null(varnames)) {
    rownames(output) <- varnames
  }
  return(output)
}
