#' Bias-Corrected and Accelerated Confidence Intervals
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
#' jackknife <- jackknife(x, func = func)
#'
#' ci_bca(
#'   thetahatstar,
#'   jackknife = jackknife,
#'   thetahat = thetahat
#' )
#' @export
#' @family Nonparametric Bootstrap Functions
#' @keywords nBootstrap
ci_bca <- function(x,
                   jackknife,
                   thetahat,
                   alpha = c(0.05, 0.01, 0.001)) {
  input <- ci_bca_helper(
    x = x,
    jackknife = jackknife,
    thetahat = thetahat,
    alpha = alpha
  )
  bca_probs <- lapply(
    X = seq_len(input$k),
    FUN = function(i) {
      return(
        stats::pnorm(
          input$z0hat[[i]] + (input$z0hat[[i]] + input$z1) / (1 - input$ahat[[i]] * (input$z0hat[[i]] + input$z1))
        )
      )
    }
  )
  output <- lapply(
    X = seq_len(input$k),
    FUN = function(i) {
      stats::quantile(
        input$x[[i]],
        probs = bca_probs[[i]],
        names = FALSE
      )
    }
  )
  output <- do.call(
    what = "rbind",
    args = output
  )
  colnames(output) <- paste0("ci_", input$probs * 100)
  return(output)
}
