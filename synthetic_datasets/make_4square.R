make_four_square_dataset <- function(n_samples=100, noise=0.0, shuffle=TRUE, random_state=NULL, plot=FALSE) {
  
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  n_samples_per_class <- n_samples / 4
  
  # first class (bottom left and top right)
  x1_class1_bottom_left <- runif(n_samples_per_class, min=0, max=0.5)
  x2_class1_bottom_left <- runif(n_samples_per_class, min=0, max=0.5)
  x1_class1_top_right <- runif(n_samples_per_class, min=0.5, max=1)
  x2_class1_top_right <- runif(n_samples_per_class, min=0.5, max=1)
  
  # second class (top left and bottom right)
  x1_class2_top_left <- runif(n_samples_per_class, min=0, max=0.5)
  x2_class2_top_left <- runif(n_samples_per_class, min=0.5, max=1)
  x1_class2_bottom_right <- runif(n_samples_per_class, min=0.5, max=1)
  x2_class2_bottom_right <- runif(n_samples_per_class, min=0, max=0.5)
  
  features <- rbind(
    cbind(x1_class1_bottom_left, x2_class1_bottom_left),
    cbind(x1_class1_top_right, x2_class1_top_right),
    cbind(x1_class2_top_left, x2_class2_top_left),
    cbind(x1_class2_bottom_right, x2_class2_bottom_right)
  )
  features <- features + matrix(rnorm(n_samples * 2, mean = 0, sd = noise), ncol = 2)
  
  target <- c(rep(1, n_samples_per_class * 2), rep(2, n_samples_per_class * 2))
  
  # shuffle the dataset if requested
  if (shuffle) {
    indices <- sample(n_samples)
    features <- features[indices, ]
    target <- target[indices]
  }
  
  if (plot) {
    colors <- c("red", "blue")
    plot(features, col=colors[target], pch=19, xlab="Feature 1", ylab="Feature 2")
    legend("topright", legend=c("Class 1", "Class 2"), fill=colors)
  }
  
  list("features" = features, "target" = target)
}

# perfect 4 squares
make_four_square_dataset(n_samples=1000, shuffle=FALSE, random_state=123, plot=TRUE)
