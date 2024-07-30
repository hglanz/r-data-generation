# test-make_anova.R

library(testthat)
library(here)
library(scatterplot3d)
library(syntheticDatasets)  # Use the package namespace

test_that("make_anova generates correct dimensions", {
  result <- make_anova(n_groups = 3, n_samples_per_group = 100, n_features = 10)
  expect_equal(dim(result$X), c(300, 10))
  expect_equal(length(result$Y), 300)
  expect_equal(length(result$GroupLabels), 300)
})

test_that("make_anova handles multiple groups", {
  result <- make_anova(n_groups = 5, n_samples_per_group = 50, n_features = 10)
  expect_equal(dim(result$X), c(250, 10))
  expect_equal(length(result$Y), 250)
  expect_equal(length(result$GroupLabels), 250)
})

test_that("make_anova applies group effects correctly", {
  result <- make_anova(n_groups = 3, n_samples_per_group = 100, group_effect = 10, noise = 0)
  group_means <- tapply(result$Y, result$GroupLabels, mean)
  expect_equal(as.numeric(diff(group_means)), rep(10, 2), tolerance = 1)
})

test_that("make_anova shuffles samples", {
  set.seed(42)
  result <- make_anova(n_groups = 3, n_samples_per_group = 100, shuffle = TRUE)
  set.seed(42)
  result_no_shuffle <- make_anova(n_groups = 3, n_samples_per_group = 100, shuffle = FALSE)
  expect_false(all(result$X == result_no_shuffle$X))
  expect_false(all(result$Y == result_no_shuffle$Y))
  expect_false(all(result$GroupLabels == result_no_shuffle$GroupLabels))
})

test_that("make_anova respects random_state", {
  result1 <- make_anova(n_groups = 3, n_samples_per_group = 100, random_state = 42)
  result2 <- make_anova(n_groups = 3, n_samples_per_group = 100, random_state = 42)
  expect_equal(result1, result2)
})

test_that("make_anova returns coefficients when requested", {
  result <- make_anova(n_groups = 3, n_samples_per_group = 100, coef = TRUE)
  expect_true("coefficients" %in% names(result))
})

test_that("make_anova handles effective rank correctly", {
  result <- make_anova(n_groups = 3, n_samples_per_group = 100, n_features = 10, effective_rank = 5)
  svd_result <- svd(result$X)
  rank_approx <- sum(svd_result$d > 1e-10)
  expect_equal(rank_approx, 5)
})

test_that("make_anova generates correct group labels", {
  result <- make_anova(n_groups = 3, n_samples_per_group = 100)
  expect_equal(sort(unique(result$GroupLabels)), 1:3)
  expect_equal(as.vector(table(result$GroupLabels)), rep(100, 3))
})
