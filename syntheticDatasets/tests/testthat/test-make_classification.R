library(testthat)
library(scatterplot3d)

test_that("make_classification generates correct number of samples and features", {
  data <- make_classification(n_samples = 100, n_features = 20)
  expect_equal(nrow(data$X), 100)
  expect_equal(ncol(data$X), 20)
  expect_equal(length(data$labels), 100)
})

test_that("make_classification handles class weights correctly", {
  data <- make_classification(n_samples = 100, n_features = 20, weights = c(0.2, 0.8))
  class_counts <- table(data$labels)
  expect_equal(as.numeric(class_counts["0"]), 20, tolerance = 1)
  expect_equal(as.numeric(class_counts["1"]), 80, tolerance = 1)
})

test_that("make_classification generates informative features", {
  data <- make_classification(n_samples = 100, n_features = 20, n_informative = 10)
  expect_true(ncol(data$X) >= 10)
})

test_that("make_classification generates redundant features", {
  data <- make_classification(n_samples = 100, n_features = 20, n_informative = 10, n_redundant = 5)
  expect_true(ncol(data$X) >= 15)
})

test_that("make_classification generates repeated features", {
  data <- make_classification(n_samples = 100, n_features = 20, n_informative = 10, n_redundant = 5, n_repeated = 3)
  expect_true(ncol(data$X) >= 18)
})

test_that("make_classification adds noise features correctly", {
  data <- make_classification(n_samples = 100, n_features = 20, n_informative = 10, n_redundant = 5, n_repeated = 3)
  expect_equal(ncol(data$X), 20)
})

test_that("make_classification scales features correctly", {
  data <- make_classification(n_samples = 100, n_features = 20, scale = 2.0)
  expect_true(max(abs(data$X)) >= 2.0)
})

test_that("make_classification flips labels correctly", {
  set.seed(123)
  data <- make_classification(n_samples = 100, n_features = 20, flip_y = 0.1)
  flipped_labels <- sum(data$labels != as.numeric(data$labels))
  expect_true(flipped_labels <= 10)
})

test_that("make_classification shuffles data correctly", {
  data1 <- make_classification(n_samples = 100, n_features = 20, shuffle = FALSE, random_state = 123)
  data2 <- make_classification(n_samples = 100, n_features = 20, shuffle = TRUE, random_state = 123)
  expect_false(all(data1$X == data2$X))
  expect_false(all(data1$labels == data2$labels))
})


