library(testthat)
library(scatterplot3d)

test_that("make_spiral generates correct number of samples", {
  data <- make_spiral(n_samples = 100)
  expect_equal(nrow(data$X), 100)
  expect_equal(length(data$y), 100)
})

test_that("make_spiral generates two spirals with equal number of samples", {
  data <- make_spiral(n_samples = 100)
  spiral_counts <- as.numeric(table(data$y))
  expect_equal(spiral_counts[1], 50)
  expect_equal(spiral_counts[2], 50)
})

test_that("make_spiral adds noise correctly", {
  set.seed(123)
  data_no_noise <- make_spiral(n_samples = 100, noise = NULL)
  data_with_noise <- make_spiral(n_samples = 100, noise = 0.1)
  expect_false(all(data_no_noise$X == data_with_noise$X))
})

test_that("make_spiral shuffles data correctly", {
  set.seed(123)
  data_not_shuffled <- make_spiral(n_samples = 100, shuffle = FALSE)
  data_shuffled <- make_spiral(n_samples = 100, shuffle = TRUE)
  expect_false(all(data_not_shuffled$X == data_shuffled$X))
  expect_false(all(data_not_shuffled$y == data_shuffled$y))
})

test_that("make_spiral handles random_state correctly", {
  data1 <- make_spiral(n_samples = 100, random_state = 123)
  data2 <- make_spiral(n_samples = 100, random_state = 123)
  expect_equal(data1$X, data2$X)
  expect_equal(data1$y, data2$y)
})

test_that("make_spiral plot functionality works", {
  expect_silent(make_spiral(n_samples = 100, plot = TRUE))
})

test_that("make_spiral labels are correct", {
  data <- make_spiral(n_samples = 100)
  expect_equal(unique(data$y), c(1, 2))
})
