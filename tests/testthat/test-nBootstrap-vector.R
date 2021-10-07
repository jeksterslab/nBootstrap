## ---- test-nBootstrap-vector
set.seed(42)
tol_i <- 0.01
n_i <- 1000
bcap_i <- 5000L
x_i <- rnorm(n_i)
func_i <- mean
thetahat_i <- func_i(x_i)
answer_i_pc <- c(-0.09088848, 0.03673862)
answer_i_bca <- c(-0.08919976, 0.03810569)
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
testthat::test_that("test-nBootstrap-vector pc", {
  testthat::expect_true(
    all(
      abs(
        answer_i_pc - result_i_pc
      ) <= tol_i
    )
  )
})
testthat::test_that("test-nBootstrap-vector bc", {
  testthat::expect_true(
    all(
      abs(
        answer_i_bca - result_i_bc
      ) <= tol_i
    )
  )
})
testthat::test_that("test-nBootstrap-vector bca", {
  testthat::expect_true(
    all(
      abs(
        answer_i_bca - result_i_bca
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
testthat::test_that("test-nBootstrap-vector pc", {
  testthat::expect_true(
    all(
      abs(
        answer_i_pc - result_i_pc
      ) <= tol_i
    )
  )
})
testthat::test_that("test-nBootstrap-vector bc", {
  testthat::expect_true(
    all(
      abs(
        answer_i_bca - result_i_bc
      ) <= tol_i
    )
  )
})
testthat::test_that("test-nBootstrap-vector bca", {
  testthat::expect_true(
    all(
      abs(
        answer_i_bca - result_i_bca
      ) <= tol_i
    )
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  bcap_i,
  x_i,
  func_i,
  thetahat_i,
  answer_i_pc,
  answer_i_bca,
  thetahatstar_i,
  jackknife_i,
  result_i_pc,
  result_i_bc,
  result_i_bca,
  xstar_i
)
