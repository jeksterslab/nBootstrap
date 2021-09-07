## ---- test-nBootstrap-matrix
set.seed(42)
tol_i <- 0.01
n_i <- 1000
bcap_i <- 5000L
mu_i <- c(0, 0)
sigmacap_i <- matrix(
  data = c(
    1.0,
    0.5,
    0.5,
    1
  ),
  nrow = 2,
  ncol = 2
)
x_i <- rmvn_chol(
  n = n_i,
  mu = mu_i,
  sigmacap = sigmacap_i,
  data_frame = TRUE
)
func_i <- colMeans
thetahat_i <- func_i(x_i)
thetahatstar_i <- thetahatstar_nb(
  x = x_i,
  func = func_i,
  bcap = bcap_i
)
jackknife_i <- jackknife(
  x = x_i,
  func = func_i
)
result_i_pc <- as.vector(
  ci_pc(
    thetahatstar_i,
    alpha = 0.05
  )
)
result_i_bc <- as.vector(
  ci_bc(
    thetahatstar_i,
    thetahat = thetahat_i,
    alpha = 0.05
  )
)
result_i_bca <- as.vector(
  ci_bca(
    thetahatstar_i,
    thetahat = thetahat_i,
    jackknife = jackknife_i,
    alpha = 0.05
  )
)
testthat::test_that("bc", {
  testthat::expect_true(
    all(
      abs(
        result_i_pc - result_i_bc
      ) <= tol_i
    )
  )
})
testthat::test_that("bca", {
  testthat::expect_true(
    all(
      abs(
        result_i_pc - result_i_bca
      ) <= tol_i
    )
  )
})
# xstar
xstar_i <- xstar_nb(
  x = x_i,
  bcap = bcap_i
)
thetahatstar_i <- lapply(
  X = xstar_i,
  FUN = func_i
)
thetahatstar_i <- do.call(
  what = "rbind",
  args = thetahatstar_i
)
result_i_pc <- as.vector(
  ci_pc(
    thetahatstar_i,
    alpha = 0.05
  )
)
result_i_bc <- as.vector(
  ci_bc(
    thetahatstar_i,
    thetahat = thetahat_i,
    alpha = 0.05
  )
)
result_i_bca <- as.vector(
  ci_bca(
    thetahatstar_i,
    thetahat = thetahat_i,
    jackknife = jackknife_i,
    alpha = 0.05
  )
)
testthat::test_that("bc", {
  testthat::expect_true(
    all(
      abs(
        result_i_pc - result_i_bc
      ) <= tol_i
    )
  )
})
testthat::test_that("bca", {
  testthat::expect_true(
    all(
      abs(
        result_i_pc - result_i_bca
      ) <= tol_i
    )
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  bcap_i,
  mu_i,
  sigmacap_i,
  x_i,
  func_i,
  thetahat_i,
  thetahatstar_i,
  jackknife_i,
  result_i_pc,
  result_i_bc,
  result_i_bca,
  xstar_i
)
