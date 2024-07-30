library(testthat)
library(scatterplot3d)

test_that("make_circles generates correct number of samples", {
  data <- make_circles(n_samples = 100)
  expect_equal(nrow(data$data), 100)
  expect_equal(length(data$labels), 100)
})

test_that("make_circles generates two circles with equal number of samples", {
  data <- make_circles(n_samples = 100)
  circle_counts <- as.numeric(table(data$labels))
  expect_equal(circle_counts[1], 50)
  expect_equal(circle_counts[2], 50)
})

test_that("make_circles adds noise correctly", {
  set.seed(123)
  data_no_noise <- make_circles(n_samples = 100, noise = NULL)
  data_with_noise <- make_circles(n_samples = 100, noise = 0.1)
  expect_false(all(data_no_noise$data == data_with_noise$data))
})

test_that("make_circles shuffles data correctly", {
  set.seed(123)
  data_not_shuffled <- make_circles(n_samples = 100, shuffle = FALSE)
  data_shuffled <- make_circles(n_samples = 100, shuffle = TRUE)
  expect_false(all(data_not_shuffled$data == data_shuffled$data))
  expect_false(all(data_not_shuffled$labels == data_shuffled$labels))
})

test_that("make_circles handles random_state correctly", {
  data1 <- make_circles(n_samples = 100, random_state = 123)
  data2 <- make_circles(n_samples = 100, random_state = 123)
  expect_equal(data1$data, data2$data)
  expect_equal(data1$labels, data2$labels)
})

test_that("make_circles plot functionality works", {
  expect_silent(make_circles(n_samples = 100, plot = TRUE))
})

test_that("make_circles labels are correct", {
  data <- make_circles(n_samples = 100)
  expect_equal(unique(data$labels), c(1, 2))
})

test_that("make_circles factor parameter works", {
  data <- make_circles(n_samples = 100, factor = 0.5)
  outer_circle_radius <- mean(sqrt(rowSums(data$data[data$labels == 1, ]^2)))
  inner_circle_radius <- mean(sqrt(rowSums(data$data[data$labels == 2, ]^2)))
  expect_true(abs(outer_circle_radius / inner_circle_radius - 2) < 0.1)
})
