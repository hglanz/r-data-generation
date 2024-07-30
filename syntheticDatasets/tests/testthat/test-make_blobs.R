library(testthat)
library(scatterplot3d)

test_that("make_blobs generates correct number of samples and features", {
  data <- make_blobs(n_samples = 100, n_features = 2)
  expect_equal(nrow(data$X), 100)
  expect_equal(ncol(data$X), 2)
  expect_equal(length(data$y), 100)
})

test_that("make_blobs generates correct number of centers", {
  data <- make_blobs(n_samples = 100, centers = 5)
  expect_equal(length(unique(data$y)), 5)
})

test_that("make_blobs adds noise correctly with standard deviation", {
  set.seed(123)
  data <- make_blobs(n_samples = 100, n_features = 2, cluster_std = 2.0)
  expect_true(sd(data$X[,1]) > 1.0)
  expect_true(sd(data$X[,2]) > 1.0)
})

test_that("make_blobs handles custom centers correctly", {
  centers <- matrix(c(0, 0, 5, 5, 10, 10), ncol = 2, byrow = TRUE)
  data <- make_blobs(n_samples = 90, centers = centers, return_centers = TRUE)
  expect_equal(nrow(data$centers), 3)
  expect_equal(ncol(data$centers), 2)
  expect_true(all(data$centers == centers))
})

test_that("make_blobs shuffles data correctly", {
  set.seed(123)
  data_not_shuffled <- make_blobs(n_samples = 100, shuffle = FALSE)
  data_shuffled <- make_blobs(n_samples = 100, shuffle = TRUE)
  expect_false(all(data_not_shuffled$X == data_shuffled$X))
  expect_false(all(data_not_shuffled$y == data_shuffled$y))
})

test_that("make_blobs handles random_state correctly", {
  data1 <- make_blobs(n_samples = 100, random_state = 123)
  data2 <- make_blobs(n_samples = 100, random_state = 123)
  expect_equal(data1$X, data2$X)
  expect_equal(data1$y, data2$y)
})

test_that("make_blobs plot functionality works", {
  expect_silent(make_blobs(n_samples = 100, n_features = 2, plot = TRUE))
  expect_silent(make_blobs(n_samples = 100, n_features = 3, plot = TRUE))
})

test_that("make_blobs returns centers when requested", {
  data <- make_blobs(n_samples = 100, return_centers = TRUE)
  expect_true(!is.null(data$centers))
  expect_equal(ncol(data$centers), ncol(data$X))
})
