make_classification <- function(n_samples = 100, n_features = 20, n_informative = 2, 
                                n_redundant = 2, n_repeated = 0, n_classes = 2, 
                                n_clusters_per_class = 2, class_sep = 1.0, flip_y = 0.01, 
                                weights = NULL, random_state = NULL, shuffle = TRUE, scale = 1.0, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  # check total features
  total_features = n_informative + n_redundant + n_repeated
  if (total_features > n_features) {
    stop("Total number of informative, redundant, and repeated features must be <= n_features")
  }
  
  # random data values
  informative_features <- matrix(rnorm(n_samples * n_informative), nrow=n_samples, ncol=n_informative)
  
  # generate redundant features: linear combinations of informative features
  if (n_redundant > 0) {
    # n_informative * n_redundant random values bet 0-1 using runif
    coeffs <- matrix(runif(n_informative * n_redundant), ncol = n_redundant)
    redundant_features <- informative_features %*% coeffs
  } else {
    redundant_features <- NULL
  }
  
  # combine features and possibly add repeated features
  X <- cbind(informative_features, redundant_features)
  if (n_repeated > 0) {
    #  indices for repeated features randomly from both informative and redundant features.
    repeated_indices <- sample(n_informative + n_redundant, n_repeated, replace = TRUE)
    repeated_features <- X[, repeated_indices]
    X <- cbind(X, repeated_features)
  }
  
  # scale features
  X <- X * scale
  
  # generate labels with class separation
  # according to the specified number of classes and their distribution (weights).
  labels <- sample(0:(n_classes-1), n_samples, replace = TRUE, prob = weights)
  
  
  # sampling indices by random with the number fraction flip_y* n_samples
  flip_indices <- sample(n_samples, size = round(flip_y * n_samples))
  # replace with another randomly sampled class
  labels[flip_indices] <- sample(0:(n_classes-1), length(flip_indices), replace = TRUE)
  
  # shuffle if requested
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices, ]
    labels <- labels[indices]
  }
  
  # plot if needed
  if (plot && n_features >= 2) {
    colors <- c('red', 'blue', 'green', 'yellow', 'purple', 'orange')[1:n_classes]
    plot(X[,1], X[,2], col=colors[labels + 1], xlab="Feature 1", ylab="Feature 2", main="Classification Plot", pch = 19)
    legend("topright", legend=paste("Class", 0:(n_classes-1)), fill=colors[1:n_classes])
  } else if (plot) {
    warning("Plotting requires at least 2 features.")
  }
  
  return(list("X" = X, "labels" = labels))
}

# plotting and testing


# Test cases

# Default settings
make_classification(plot=TRUE)

# Increase informative features
make_classification(n_informative=4, plot=TRUE)

# High class separation
make_classification(class_sep=2.0, plot=TRUE)

# More samples, high noise (flip_y)
make_classification(n_samples=200, flip_y=0.1, plot=TRUE)

# More clusters per class
make_classification(n_clusters_per_class=3, plot=TRUE)

# Unbalanced class weights
make_classification(weights=c(0.7, 0.3), plot=TRUE)

# Only informative features
make_classification(n_redundant=0, n_repeated=0, plot=TRUE)

# Many features, mostly noise
make_classification(n_features=40, n_informative=2, plot=TRUE)

# Different scale
make_classification(scale=0.5, plot=TRUE)

