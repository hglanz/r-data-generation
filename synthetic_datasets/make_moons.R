
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

