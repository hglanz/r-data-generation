
library(testthat)
library(syntheticDatasets)

test_that("make_regression generates correct dimensions", {
  result <- make_regression(n_samples = 100, n_features = 10)
  expect_equal(dim(result$X), c(100, 10))
  expect_equal(length(result$Y), 100)
})

test_that("make_regression handles multiple targets", {
  result <- make_regression(n_samples = 100, n_features = 10, n_targets = 3)
  expect_equal(dim(result$X), c(100, 10))
  expect_equal(dim(result$Y), c(100, 3))
})

test_that("make_regression generates informative features correctly", {
  result <- make_regression(n_samples = 100, n_features = 10, n_informative = 5, coef = TRUE)
  expect_equal(dim(result$X), c(100, 10))
  expect_equal(length(result$Y), 100)
  expect_equal(length(result$coefficients), 5)
})

test_that("make_regression respects the bias term", {
  result <- make_regression(n_samples = 100, n_features = 10, bias = 10, noise = 0)
  expect_equal(mean(result$Y), 10, tolerance = 1)
})


test_that("make_regression adds noise correctly", {
  set.seed(42)
  result <- make_regression(n_samples = 100, n_features = 10, noise = 1)
  set.seed(42)
  result_no_noise <- make_regression(n_samples = 100, n_features = 10, noise = 0)
  expect_true(sd(result$Y - result_no_noise$Y) > 0)
})

test_that("make_regression shuffles samples", {
  set.seed(42)
  result <- make_regression(n_samples = 100, n_features = 10, shuffle = TRUE)
  set.seed(42)
  result_no_shuffle <- make_regression(n_samples = 100, n_features = 10, shuffle = FALSE)
  expect_false(all(result$X == result_no_shuffle$X))
  expect_false(all(result$Y == result_no_shuffle$Y))
})

test_that("make_regression respects random_state", {
  result1 <- make_regression(n_samples = 100, n_features = 10, random_state = 42)
  result2 <- make_regression(n_samples = 100, n_features = 10, random_state = 42)
  expect_equal(result1, result2)
})

test_that("make_regression handles effective rank correctly", {
  result <- make_regression(n_samples = 100, n_features = 10, effective_rank = 5)
  svd_result <- svd(result$X)
  rank_approx <- sum(svd_result$d > 1e-10)
  expect_equal(rank_approx, 5)
})

test_that("make_regression returns coefficients when requested", {
  result <- make_regression(n_samples = 100, n_features = 10, coef = TRUE)
  expect_true("coefficients" %in% names(result))
})

test_that("make_regression handles plot parameter correctly", {
  expect_warning(make_regression(n_samples = 100, n_features = 1, plot = TRUE), "Plotting requires at least 2 features")
  expect_warning(make_regression(n_samples = 100, n_features = 10, n_targets = 2, plot = TRUE), "supports only one target")
  expect_silent(make_regression(n_samples = 100, n_features = 2, plot = TRUE))
})
