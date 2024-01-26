make_classification <- function(n_samples = 100, n_features = 20, n_informative = 2, 
                                n_redundant = 2, n_repeated = 0, n_classes = 2, 
                                n_clusters_per_class = 2, class_sep = 1.0, flip_y = 0.01, 
                                weights = NULL, random_state = NULL, shuffle = TRUE, scale = 1.0) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  # quick check
  total_features = n_informative + n_redundant + n_repeated
  if (total_features > n_features) {
    stop("Total number of informative, redundant and repeated features must be <= n_features")
  }
  
  informative_features <- matrix(rnorm(n_samples * n_informative), ncol = n_informative)
  

  # To do:
  # Generate redundant features (linear combinations of informative features)
  # Generate repeated features (random selection of informative and redundant features)
  # Generate labels with optional class separation
  # Add noise by flipping labels
  # Add weight
  # Optional functionality: plotting the points on a plane
  
  
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices, ]
    labels <- labels[indices]
  }
  
  return(list("X" = X, "Y" = labels))
}


