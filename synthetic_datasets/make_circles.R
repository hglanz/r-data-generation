
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
  
  # shuffle
  if (shuffle) {
    data <- data[sample(n_samples), ]
  }
  
  # create labels 1 and 2 for the two classes (already in order so we can just assign down the middle)
  labels <- c(rep(1, n_samples / 2), rep(2, n_samples / 2))
  
  if (plot) {
    plot(data, col = labels, asp = 1, xlab = "Feature 1", ylab = "Feature 2", pch = 16)
    legend("topright", legend = c("Outer Circle", "Inner Circle"), col = 1:2 ,pch = 16 )
  }
  
  return(list("data" = data, "labels" = labels))
}



# Default settings
make_circles(plot = TRUE)


# Increased number of samples
make_circles(n_samples = 200, plot = TRUE)


# Adding noise
make_circles(noise = 0.1, plot = TRUE)


# Without shuffling
make_circles(shuffle = FALSE, plot = TRUE)

# Different factor for inner and outer circle
make_circles(factor = 0.5, plot = TRUE)


# High noise level
make_circles(noise = 0.3, plot = TRUE)


# Large number of samples with noise
make_circles(n_samples = 300, noise = 0.1, plot = TRUE)

# Small factor and high noise
make_circles(factor = 0.3, noise = 0.2, plot = TRUE)

# Large factor, no noise
make_circles(factor = 0.9, noise = NULL, plot = TRUE)

# Very small number of samples
make_circles(n_samples = 50, plot = TRUE)

