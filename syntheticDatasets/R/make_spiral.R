library(scatterplot3d)
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")
#' Generate Spiral Dataset
#'
#' This function generates a two-dimensional dataset comprising two interwoven spirals. It's useful for visualizing and testing algorithms that require learning complex, non-linear decision boundaries, such as neural networks or advanced clustering techniques.
#'
#' @param n_samples Total number of points to generate. Default is 100.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param noise Standard deviation of Gaussian noise added to the samples. If `NULL`, no noise is added. Default is `NULL`.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be `NULL`. Default is `NULL`.
#' @param plot Logical, indicating whether to plot the dataset. Default is FALSE.
#' @return A list with two elements: 'X' is a matrix with the coordinates of each sample, and 'y' is a vector indicating the class (1 or 2) of each sample.
#' @examples
#' # Generate and plot a default spiral dataset
#' spiral_data <- make_spiral(plot = TRUE)
#'
#' # Generate and plot a spiral dataset with added noise
#' noisy_spiral <- make_spiral(n_samples = 200, noise = 0.1, plot = TRUE)
#'
#' # Generate a spiral dataset with a specific random state and plot it
#' spiral_seed <- make_spiral(n_samples = 200, noise = 0.1, random_state = 123, plot = TRUE)
#' @export
make_spiral <- function(n_samples = 100, shuffle = TRUE, noise = NULL, random_state = NULL, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  n_samples_per_spiral <- n_samples / 2
  max_rotation <- 4 * pi  # how many times we rotate

  #  spirals (same math from make_moons and make_circles)
  t1 <- seq(0, max_rotation, length.out = n_samples_per_spiral)
  spiral1_x <- t1 * cos(t1)
  spiral1_y <- t1 * sin(t1)

  t2 <- seq(0, max_rotation, length.out = n_samples_per_spiral)
  spiral2_x <- t2 * cos(t2 + pi)
  spiral2_y <- t2 * sin(t2 + pi)

  # combine the spirals
  data <- rbind(cbind(spiral1_x, spiral1_y), cbind(spiral2_x, spiral2_y))

  # adding noise
  if (!is.null(noise)) {
    data <- data + matrix(rnorm(n_samples * 2, sd = noise), ncol = 2)
  }

  # shuffle data if specified
  if (shuffle) {
    shuffle_index <- sample(n_samples)
    data <- data[shuffle_index, ]
  }

  # labels for the spirals
  labels <- c(rep(1, n_samples_per_spiral), rep(2, n_samples_per_spiral))
  if (shuffle) {
    labels <- labels[shuffle_index]
  }

  # plot if specified
  if (plot) {
    colors <- okabe_ito_colors[1:2]
    plot(data, col=colors[labels], xlab = "Feature 1", ylab = "Feature 2", pch = 19, main = "Generated Spiral Dataset")
    legend("topright", legend = c("Spiral 1", "Spiral 2"), fill=colors)
  }

  return(list("X" = data, "y" = labels))
}



