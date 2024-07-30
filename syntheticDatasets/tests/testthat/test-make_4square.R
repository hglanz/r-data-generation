library(testthat)

test_that("make_four_square generates correct number of samples and features", {
  data <- make_four_square(n_samples = 100)
  expect_equal(nrow(data$features), 100)
  expect_equal(ncol(data$features), 2)
  expect_equal(length(data$target), 100)
})

test_that("make_four_square generates balanced classes", {
  data <- make_four_square(n_samples = 100)
  class_counts <- table(data$target)
  expect_equal(as.numeric(class_counts["1"]), 50)
  expect_equal(as.numeric(class_counts["2"]), 50)
})

test_that("make_four_square adds noise correctly", {
  set.seed(123)
  data_no_noise <- make_four_square(n_samples = 100, noise = 0.0)
  data_with_noise <- make_four_square(n_samples = 100, noise = 0.5)
  expect_false(all(data_no_noise$features == data_with_noise$features))
  expect_true(sd(data_with_noise$features) > sd(data_no_noise$features))
})

test_that("make_four_square shuffles data correctly", {
  set.seed(123)
  data_not_shuffled <- make_four_square(n_samples = 100, shuffle = FALSE)
  data_shuffled <- make_four_square(n_samples = 100, shuffle = TRUE)
  expect_false(all(data_not_shuffled$features == data_shuffled$features))
  expect_false(all(data_not_shuffled$target == data_shuffled$target))
})

test_that("make_four_square handles random_state correctly", {
  data1 <- make_four_square(n_samples = 100, random_state = 123)
  data2 <- make_four_square(n_samples = 100, random_state = 123)
  expect_equal(data1$features, data2$features)
  expect_equal(data1$target, data2$target)
})

test_that("make_four_square plot functionality works", {
  expect_silent(make_four_square(n_samples = 100, plot = TRUE))
})

test_that("make_four_square generates correct coordinates for classes", {
  set.seed(123)
  data <- make_four_square(n_samples = 100, noise = 0)
  class1 <- data$features[data$target == 1, ]
  class2 <- data$features[data$target == 2, ]

  expect_true(all(class1[, 1] <= 0.5 | class1[, 1] >= 0.5))
  expect_true(all(class1[, 2] <= 0.5 | class1[, 2] >= 0.5))

  expect_true(all(class2[, 1] <= 0.5 | class2[, 1] >= 0.5))
  expect_true(all(class2[, 2] >= 0.5 | class2[, 2] <= 0.5))
})
