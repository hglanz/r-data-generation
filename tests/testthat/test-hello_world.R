
context("Hello world function tests")

test_that("hello_world prints correctly", {
  expect_output(hello_world(), "Hello, World!")
})