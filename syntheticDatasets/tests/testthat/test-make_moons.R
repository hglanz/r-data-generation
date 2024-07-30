library(testthat)

test_that("make_moons generates correct number of samples and features", {
  data <- make_moons(n_samples = 100)
  expect_equal(nrow(data$X), 100)
  expect_equal(ncol(data$X), 2)
  expect_equal(length(data$y), 100)
})

test_that("make_moons generates balanced classes", {
  data <- make_moons(n_samples = 100)
  class_counts <- table(data$y)
  expect_equal(as.numeric(class_counts["1"]), 50)
  expect_equal(as.numeric(class_counts["2"]), 50)
})

test_that("make_moons adds noise correctly", {
  set.seed(123)
  data_no_noise <- make_moons(n_samples = 100, noise = 0.0)
  data_with_noise <- make_moons(n_samples = 100, noise = 0.5)
  expect_false(all(data_no_noise$X == data_with_noise$X))
  expect_true(sd(data_with_noise$X) > sd(data_no_noise$X))
})

test_that("make_moons shuffles data correctly", {
  set.seed(123)
  data_not_shuffled <- make_moons(n_samples = 100, shuffle = FALSE)
  data_shuffled <- make_moons(n_samples = 100, shuffle = TRUE)
  expect_false(all(data_not_shuffled$X == data_shuffled$X))
  expect_false(all(data_not_shuffled$y == data_shuffled$y))
})

test_that("make_moons handles random_state correctly", {
  data1 <- make_moons(n_samples = 100, random_state = 123)
  data2 <- make_moons(n_samples = 100, random_state = 123)
  expect_equal(data1$X, data2$X)
  expect_equal(data1$y, data2$y)
})

test_that("make_moons plot functionality works", {
  expect_silent(make_moons(n_samples = 100, plot = TRUE))
})

test_that("make_moons generates correct coordinates for moons", {
  set.seed(123)
  data <- make_moons(n_samples = 100, noise = 0)
  moon1 <- data$X[data$y == 1, ]
  moon2 <- data$X[data$y == 2, ]

  expect_true(all(moon1[, 1] >= -1 & moon1[, 1] <= 1))
  expect_true(all(moon1[, 2] >= 0 & moon1[, 2] <= 1))

  expect_true(all(moon2[, 1] >= 0 & moon2[, 1] <= 2))
  expect_true(all(moon2[, 2] >= -1.5 & moon2[, 2] <= 0.5))
})
