library(scatterplot3d)
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")
#' Generate Circles Dataset
#'
#' This function generates a two-dimensional dataset comprising two circles, one within the other, known as the "circles" dataset.
#' It is useful for visualizing and testing algorithms that require a non-linear decision boundary, such as clustering and classification algorithms.
#'
#' @param n_samples Total number of points to generate. Default is 100.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param noise Standard deviation of Gaussian noise added to the samples. If `NULL`, no noise is added. Default is `NULL`.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be `NULL`. Default is `NULL`.
#' @param factor Scale factor between the two circles. A smaller factor makes the inner circle smaller. Default is 0.8.
#' @param plot Logical, indicating whether to plot the dataset. Default is FALSE.
#' @return A list with two elements: 'data' is a matrix with the coordinates of each sample, and 'labels' is a vector indicating the class (1 for outer circle, 2 for inner circle) of each sample.
#' @examples
#' # Generate and plot a default circles dataset
#' circles_data <- make_circles(plot = TRUE)
#'
#' # Generate circles with noise and plot
#' noisy_circles <- make_circles(noise = 0.1, plot = TRUE)
#'
#' # Generate circles with a different factor and plot
#' custom_circles <- make_circles(factor = 0.5, plot = TRUE)
#' @export
make_circles <- function(n_samples = 100, shuffle = TRUE, noise = NULL, random_state = NULL, factor = 0.8, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  # sampling in radians
  t <- seq(0, 2*pi, length.out = n_samples / 2 + 1)
  t <- t[-length(t)]
  # https://www.reddit.com/r/MachineLearning/comments/20gfyy/clustering_points_around_a_circle/
  circle_outer <- cbind(cos(t), sin(t))
  # a scaled version of the outer circle
  circle_inner <- factor * circle_outer

  # Combine the circles
  data <- rbind(circle_outer, circle_inner)

  # adding noise
  if (!is.null(noise)) {
    data <- data + matrix(rnorm(2 * n_samples, sd = noise), ncol = 2)
  }

  # create labels 1 and 2 for the two classes
  labels <- c(rep(1, n_samples / 2), rep(2, n_samples / 2))

  # shuffle
  if (shuffle) {
    indices <- sample(n_samples)
    data <- data[indices, ]
    labels <- labels[indices]
  }

  if (plot) {
    colors <- okabe_ito_colors[1:2]
    plot(data, col = colors[labels], asp = 1, xlab = "Feature 1", ylab = "Feature 2", main = "Generated Circles Dataset", pch = 16)
    legend("topright", legend = c("Outer Circle", "Inner Circle"), col = colors[1:2], pch = 16)
  }

  return(list("data" = data, "labels" = labels))
}
