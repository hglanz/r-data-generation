#' Generate Moons Dataset
#'
#' This function generates a two-dimensional dataset comprising two interleaving half circles, known as the "moons" dataset. It is particularly useful for visualizing and testing clustering and classification algorithms that require a non-linear decision boundary.
#'
#' @param n_samples Total number of points to generate. Default is 100.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param noise Standard deviation of Gaussian noise added to the samples. If `NULL`, no noise is added. Default is `NULL`.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be `NULL`. Default is `NULL`.
#' @param plot Logical, indicating whether to plot the dataset. Default is FALSE.
#' @return A list with two elements: 'X' is a matrix with the coordinates of each sample, and 'y' is a vector indicating the class (1 or 2) of each sample.
#' @examples
#' # Generate and plot a default moons dataset
#' moons_data <- make_moons(plot = TRUE)
#'
#' # Generate and plot moons dataset with added noise
#' noisy_moons <- make_moons(noise = 0.1, plot = TRUE)
#'
#' # Generate, shuffle, and plot moons dataset with specific random state
#' shuffled_moons <- make_moons(random_state = 42, plot = TRUE)
#' @export
make_moons <- function(n_samples = 100, shuffle = TRUE, noise = NULL, random_state = NULL, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  # generate semicircle for the first moon
  t1 <- seq(0, pi, length.out = n_samples / 2)
  moon1_x <- cos(t1)
  moon1_y <- sin(t1)

  # generate semicircle for the second moon
  t2 <- seq(0, pi, length.out = n_samples / 2)
  # shifted to the right
  moon2_x <- 1 - cos(t2)
  # flipped vertically and shift downwards
  moon2_y <- 1 - sin(t2) - 0.5

  # combine the moons
  data <- rbind(cbind(moon1_x, moon1_y), cbind(moon2_x, moon2_y))

  # adding noise
  if (!is.null(noise)) {
    data <- data + matrix(rnorm(n_samples * 2, sd = noise), ncol = 2)
  }

  # shuffle data if specified
  if (shuffle) {
    shuffle_index <- sample(n_samples)
    data <- data[shuffle_index, ]
  }

  # labels for the moons
  labels <- c(rep(1, n_samples / 2), rep(2, n_samples / 2))
  if (shuffle) {
    labels <- labels[shuffle_index]
  }

  # plot if specified
  if (plot) {
    plot(data, col = labels, xlab = "Feature 1", ylab = "Feature 2",pch = 19)
    legend("topright", legend = c("Moon 1", "Moon 2"), col = 1:2, pch = 19)
  }

  return(list("X" = data, "y" = labels))
}


# Default settings
make_moons(plot = TRUE)

# Higher number of samples
make_moons(n_samples = 300, plot = TRUE)

# noise
make_moons(noise = 0.1, plot = TRUE)

# Increased noise
make_moons(noise = 0.3, plot = TRUE)

# No shuffle
make_moons(shuffle = FALSE, plot = TRUE)

# Specific random state for reproducibility
make_moons(random_state = 42, plot = TRUE)

# Large number of samples with noise
make_moons(n_samples = 500, noise = 0.1, plot = TRUE)

# Large number of samples, high noise, no shuffle
make_moons(n_samples = 500, noise = 0.2, shuffle = FALSE, plot = TRUE)

# Medium noise with a specific random state
make_moons(noise = 0.15, random_state = 24, plot = TRUE)

# High noise, no shuffle, specific random state
make_moons(noise = 0.25, shuffle = FALSE, random_state = 11, plot = TRUE)

