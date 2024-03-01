make_spiral <- function(n_samples = 100, shuffle = TRUE, noise = NULL, random_state = NULL, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  n_samples_per_spiral <- n_samples / 2
  max_rotation <- 4 * pi  # how many times we rotate
  
  #  spirals (sing math from make_moons and make_circles)
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
    colors <- c("red", "blue")
    plot(data, col=colors[labels], xlab = "Feature 1", ylab = "Feature 2", pch = 19)
    legend("topright", legend = c("Spiral 1", "Spiral 2"), fill=colors)
  }
  
  return(list("X" = data, "y" = labels))
}


make_spiral(n_samples = 500, plot = TRUE, shuffle = FALSE)

make_spiral(n_samples = 200, noise = 0.1, plot = TRUE, shuffle = FALSE)

make_spiral(n_samples = 200, noise = 0.5, random_state = 123, plot = TRUE)


